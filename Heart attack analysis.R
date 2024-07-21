library(tidyverse)
library(caret)
library(party)
library(readxl)
################################ STEP 1: GET THE DATA  #########################################
#loading the dataset into R 
#set the working directory
setwd("C:/Users/Baha/Downloads")
heart <-read_excel("heart.xlsx") ### data from kaggle.com
#read the first 10 observations of the data
head(heart, n=10)
names(heart)[[1]] <-"age"
#recoding the target variable
heart$target <- dplyr::recode(heart$target, `0` = 1L, `1` = 0L)
head(heart,n=10)
sapply(heart, function(x) table(is.na(x)))
table(duplicated(heart))
heart<-heart[!duplicated(heart),]
table(duplicated(heart))
####We see that missingness (is.na) is FALSE for all columns, which is great. 
# there is one duplicated record, which we remove.

####################### STEP 2: VISUAL INSPECTION/ DESCRIPTIVE STATISTICS #######################
#Note that if your data has 100 columns instead of only 14, you could divide your data into parts of, say, 25 columns each. 
#Just start the following code chunk with, e.g., dat[,1:25] %>% …
heart |> gather() |> 
  ggplot(aes(x=value)) +
  geom_histogram(fill = "steelblue", alpha=.7) +
  theme_minimal() +
  facet_wrap(~key, scales = "free")
#bivariate statistics (correlation matrix)
cormat <-cor(heart |> keep(is.numeric))
cormat %>% as.data.frame %>% mutate(var2=rownames(.)) %>%
  pivot_longer(!var2, values_to = "value") %>%
  ggplot(aes(x=name,y=var2,fill=abs(value),label=round(value,2))) +
  geom_tile() + geom_label() + xlab("") + ylab("") +
  ggtitle("Correlation matrix of our predictors") +
  labs(fill="Correlation\n(absolute):")
highcorr <-which(cormat >.8, arr.ind = T)
paste(rownames(cormat)[row(cormat)[highcorr]],
colnames(cormat)[col(cormat)[highcorr]], sep = " vs. ")  |> 
  cbind(cormat[highcorr])
##bivariate relations between the predictors and the outcome variable
heart %>% select(-c(sex,cp,caa,thall,restecg,slope,exng,fbs)) %>%
  pivot_longer(!target, values_to = "value") %>%
  ggplot(aes(x=factor(target), y=value, fill=factor(target))) +
  geom_boxplot(outlier.shape = NA) + geom_jitter(size=.7, width=.1, alpha=.5) +
  scale_fill_manual(values=c("steelblue", "orangered1")) +
  labs(fill="Heart disease:") +
  theme_minimal() +
  facet_wrap(~name, scales="free")
##stacked barplots for categorical variables to show the difference between the sick and 
## healthy patients
heart |> 
  select(sex, cp, caa, thall, exng, fbs,restecg, slope, target) |> 
  pivot_longer(!target, values_to = "value") |> 
  ggplot(aes(x=factor(value), fill = factor(target))) +
  scale_fill_manual(values = c("steelblue","orangered1")) +
  geom_bar(position = "fill",alpha=.7) +
  theme_minimal() +
  labs("Heart disease:") +
  facet_wrap(~name, scales = "free")
########## PARTITION DATA INTO TRAINING AND TESTING SETS ###################
#example
pred <-as.factor(ifelse(heart$sex==1,1,0))
confusionMatrix(pred, as.factor(heart$target))
set.seed(2022)
#split the data
split_heart <-sample(1:nrow(heart), as.integer(0.7*nrow(heart)), F)
train_data <-heart[split_heart,]
test_data <-heart[-split_heart,]
dplyr::glimpse(train_data)
dplyr::glimpse(test_data)

######## STEP 4: PRE-PROCESSING AND FEATURE ENGENEERING #############################
preprocess_data <- function(df){

  # Outliers are assigned the modal value
  df <- df %>% mutate(restecg = recode(restecg, `2`=1L, .default = as.integer(restecg)),
                      thall = recode(thall, `0`=2L, .default = as.integer(thall)),
                      caa = recode(caa, `4`=0L, .default = as.integer(caa)))

  # Nominal variables
  nom_variable <- c("cp", "caa", "thall", "restecg", "slope")
  df[,names(df) %in% nom_variable] <- lapply(df[,names(df) %in% nom_variable], as.factor)
  dummies <- dummyVars(~ ., df)
  df <- predict(dummies, newdata = df) %>% as.data.frame

  # Age-standardized variables
  df$hr_age <- df$thalach / df$age
  df$chol_age <- df$chol / df$age

  # Oldpeak: Is there any ST depression
  df$st <- ifelse(df$oldpeak > 0, 1, 0)

  return(df[,names(df) != "target"])
}
x_train <- preprocess_data(train_data)
x_test <-preprocess_data(test_data)
# Check for missing values in the target variables
sum(is.na(train_data$target))
sum(is.na(test_data$target))
# Encode target variables as factors
y_train <- factor(train_data$target, levels = c(1,0))
y_test <- factor(test_data$target, levels = c(1,0))
# Ensure there are no missing values in y_train and y_test
if (any(is.na(y_train)) || any(is.na(y_test))) {
  stop("There are missing values in the target variable.")
}
########### STEP 5: VISUALIZE EXEMPLARY ALGORITHMS ####################
set.seed(2022)
tree1 <-party::ctree(y_train ~ ., data = cbind(x_train,y_train), 
controls = ctree_control(minsplit = 10, mincriterion = .9))
plot(tree1)
tree2 <-party::ctree(y_train~ ., data = cbind(x_train,y_train),
controls = ctree_control(minsplit = 1, mincriterion = 0.01, minbucket = 0))
plot(tree2) ##overfitted tree
############################### STEP 6: MODEL TRAINING #################
library(randomForest)
#random forest model
rand_model <-caret::train(x_train, y_train, method ="rf",
tuneGrid=expand.grid(mtry=seq(5, ncol(heart),by=5)),
trControl = trainControl(method = "cv", number = 5, verboseIter = T))
rand_model
max(rand_model$results$Accuracy)
# Get variable importance
var_imp <- varImp(rand_model, scale = FALSE)
var_imp
plot(var_imp, main ="Feature importance of the random forest model on training data")
#artificial neural networks
neu_net_model <-caret::train(x_train, y_train, method ="avNNet",
preProcess = c("center","scale","nzv"),
tuneGrid=expand.grid(size=seq(3,21, by=3), decay=c(1e-03, 0.01, 0.1,0), bag=c(T,F)),
trControl=trainControl(method = "cv",number = 5,verboseIter = T),
importance=T)
neu_net_model # output of neural network training
###Thus, our best-performing model yields 83.89% accuracy, which is a slight improvement over the random forest.
var_Imp <-varImp(neu_net_model, scale = F)
plot(var_Imp, main="Feature importance of neural network classifier on the training data")
##(extreme) gradient boosted machines, (xgboost) 
xgboost_model <-caret::train(x_train, y_train, method="xgbTree",
tuneGrid=expand.grid(nrounds=c(50,100), max_depth=c(5,7,9),
colsample_bytree=c(.8,1), subsample=c(.8,1),
min_child_weight=c(1,5,10), eta=c(0.1,0.3),gamma=c(0,0.5)),
trControl=trainControl(method = "cv",number = 5, verboseIter = T))
xgboost_model
var_Imp<-varImp(xgboost_model, scale = F)
plot(var_Imp, main="Feature importance of xgBoost model on the training data")
results <- data.frame(Model = c(rand_model$method,neu_net_model$method, xgboost_model$method),
                      Accuracy = c(max(rand_model$results$Accuracy), max(neu_net_model$results$Accuracy), max(xgboost_model$results$Accuracy)))
results %>% ggplot(aes(x=Model, y=Accuracy, label=paste(round(100*Accuracy,1),"%"))) +
  geom_col(fill="steelblue") + theme_minimal() + geom_label() +
  ggtitle("Accuracy in the training data by algorithm")
#######  STEP 7: MODEL EVALUATION AGAINST THE TEST DATA ###########################################
predictions <-predict(xgboost_model, newdata = x_test)
confusionMatrix(predictions, y_test)
precision(predictions, y_test) ##proportion of true positive predictions relative to all “positive” predictions
recall(predictions, y_test)  ##proportion of true positive predictions relative to all actual positives
F_meas(predictions, y_test)##harmonic mean of precision and recall
#################### STEP 8: MODEL DEPLOYMENT #######################
newpatient <- data.frame(age=62,sex=1,cp=0,trestbps =130,chol=220, fbs=0, restecg=0, 
  thalach=161, exng=0, oldpeak=0, slope=0, caa=0, thall=2)
  preprocess_new_data <- function(df){
  
    #Convert features to int like the original dataset
    df[,names(df) != "oldpeak"] <- purrr::map_df(df[,names(df) != "oldpeak"], as.integer)
    
    df <- df %>% mutate(restecg = recode(restecg, `2`=1L),
                        thall = recode(thall, `0`=2L),
                        caa= recode(caa, `4`=0L))
    
    #Nominal variables - attention: we don't have all the values for the dummies in the new dataset!
    existing_cols <- names(x_train)[names(x_train) %in% names(df)]
    new_cols <- names(x_train)[!names(x_train) %in% names(df)]
    df[new_cols] <- 0
    nom_variable <- c("cp", "caa", "thall", "restecg", "slope")
    
    for (i in 1:nrow(df)){
      for(j in 1:length(nom_variable)){
        df[i,paste0(nom_variable[j],df[nom_variable[j]][i])] <- 1 
      }
    }
    
    df <- df[,names(df) %in% c(existing_cols, new_cols)]
    
    df$hr_age <- df$thalach / df$age
    df$chol_age <- df$chol / df$age
    df$st <- ifelse(df$oldpeak>0,1,0)
    
    return(df)
  }
  
  save(xgboost_model, x_train, preprocess_new_data, file="Heart_disease_prediction.RData")
  predict(xgboost_model, newdata = preprocess_new_data(newpatient))

  predict(xgboost_model, newdata = preprocess_new_data(newpatient), type="prob")
