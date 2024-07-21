###load the required packages
library(tidyverse)
library(caret)
library(party)
library(readxl)
#################### get the data ###########################
Finan_inclusion <-read_excel("C:/Users/Baha/OneDrive/Documents/Datasets/JUDE DOCS/HACK_FINAN_INCL 2022/FinancialInclusionEA.xlsx") ## data from kaggle
##read the first 10 observations of the loaded data
head(Finan_inclusion,10)
str(Finan_inclusion)
##check whether the dataset has missing values
colSums(is.na(Finan_inclusion)) ## no missing values
###convert the categorical variables to factors and then to numeric
Finan_inclusion$location_type <-as.numeric(as.factor(Finan_inclusion$location_type))
Finan_inclusion$cellphone_access<-as.numeric(as.factor(Finan_inclusion$cellphone_access))
Finan_inclusion$gender_of_respondent<-as.numeric(as.factor(Finan_inclusion$gender_of_respondent))
Finan_inclusion$relationship_with_head<-as.numeric(as.factor(Finan_inclusion$relationship_with_head))
Finan_inclusion$marital_status<-as.numeric(as.factor(Finan_inclusion$marital_status))
Finan_inclusion$education_level<-as.numeric(as.factor(Finan_inclusion$education_level))
Finan_inclusion$job_type <-as.numeric(as.factor(Finan_inclusion$job_type))
Finan_inclusion$bank_account<-as.numeric(as.factor(Finan_inclusion$bank_account))
str(Finan_inclusion)
##recode all the binary variables
Finan_inclusion$location_type<-dplyr::recode(Finan_inclusion$location_type, `1` = 0L, `2` = 1L)
Finan_inclusion$cellphone_access<-dplyr::recode(Finan_inclusion$cellphone_access,`1` = 0L, `2` = 1L)
Finan_inclusion$gender_of_respondent<-dplyr::recode(Finan_inclusion$gender_of_respondent,`1` = 0L, `2` = 1L)
Finan_inclusion$bank_account<-dplyr::recode(Finan_inclusion$bank_account,`1` = 0L, `2` = 1L)
str(Finan_inclusion)
#check for missing values
sapply(Finan_inclusion, function(x) table(is.na(x)))
##check for duplicates in the data
table(duplicated(Finan_inclusion)) ## no duplicates in the data
#incase duplicates were present, the code for their removal is as follows

## Finan_inclusion<-Finan_inclusion[!duplicated(Finan_inclusion),]
################# visual inspection/descriptive statistics ###################
Finan_inclusion |> 
  gather() |> 
  ggplot(aes(x=value)) +
  geom_bar(fill="steelblue", alpha=.7) +
  theme_minimal() +
  facet_wrap(~key, scales = "free")
##bivariate statistics (the correlation matrix)
cormat <-cor(Finan_inclusion |> 
  keep(is.numeric))
cormat |> 
  as.data.frame() %>%
  mutate(var2=rownames(.)) %>%
    pivot_longer(!var2, values_to="value") %>%
  ggplot(aes(x=name,y=var2,fill=abs(value), label=round(value,2))) +
  geom_tile() + geom_label() + xlab("") + ylab("") +
  ggtitle("correlation matrix of our predictors") +
  labs(fill="correlation\n(absolute):")
highcorr<-which(cormat>.8, arr.ind = T)
paste(rownames(cormat)[row(cormat)[highcorr]],
colnames(cormat)[col(cormat)[highcorr]], sep = " vs. ")  |> 
  cbind(cormat[highcorr])
##bivariate relations between the predictors and the outcome variable
##stacked barplots for categorical variables to show the difference between the sick and 
## healthy patients
Finan_inclusion |> 
  select(cellphone_access, location_type, gender_of_respondent, relationship_with_head,
     marital_status, education_level,job_type, bank_account) |> 
  pivot_longer(!bank_account, values_to = "value") |> 
  ggplot(aes(x=factor(value), fill = factor(bank_account))) +
  scale_fill_manual(values = c("steelblue","orangered1")) +
  geom_bar(position = "fill",alpha=.7) +
  theme_minimal() +
  labs("Bank account:") +
  facet_wrap(~name, scales = "free")

Finan_inclusion %>% select(-c(uniqueid,country,year,location_type,cellphone_access,
  gender_of_respondent,relationship_with_head,marital_status,
education_level,job_type)) %>%
  pivot_longer(!bank_account, values_to = "value") %>%
  ggplot(aes(x=factor(bank_account), y=value, fill=factor(bank_account))) +
  geom_boxplot(outlier.shape = NA) + geom_jitter(size=.7, width=.1, alpha=.5) +
  scale_fill_manual(values=c("steelblue", "orangered1")) +
  labs(fill="Bank account:") +
  theme_minimal() +
  facet_wrap(~name, scales="free")
#################### partition data into training and testing sets ################
set.seed(2024)
Finan_inclusion <-Finan_inclusion |> 
  select(-c(uniqueid,country,year))
split_finan_inclusion<-sample(1:nrow(Finan_inclusion),as.integer(0.7*nrow(Finan_inclusion)),F)
train_data<-Finan_inclusion[split_finan_inclusion,]
test_data<-Finan_inclusion[-split_finan_inclusion,]
dplyr::glimpse(train_data)
dplyr::glimpse(test_data)
################# pre-processing and feature engeneering ######################
summary(Finan_inclusion)
attach(Finan_inclusion)
count(Finan_inclusion, vars=location_type)
count(Finan_inclusion, vars=relationship_with_head)
count(Finan_inclusion, vars=job_type)
##pre-processing procedure (train data)
#Convert necessary variables to factors
nom_variable <- c("location_type", "cellphone_access", "gender_of_respondent", 
                  "relationship_with_head", "marital_status", "education_level", "job_type")
train_data[nom_variable] <- lapply(train_data[nom_variable], as.factor)
#Create dummy variables
dummies <- dummyVars(~ ., data = train_data)
train_data_dummy <- predict(dummies, newdata = train_data) %>% as.data.frame()
#Remove target variable and separate x_train and y_train
y_train <- train_data$bank_account
x_train <- train_data_dummy
##check for missing values in the target variables (bank account)
sum(is.na(train_data$bank_account))
sum(is.na(test_data$bank_account))
#encode the target variables as factors
y_train<-factor(train_data$bank_account, levels = c(1,0))
y_test<-factor(test_data$bank_account, levels = c(1,0))
##ensure there are no missing values in y_train and y_test
any(is.na(y_train))
any(is.na(y_test))

##pre-processing procedure (test data)
#Convert necessary variables to factors
nom_variable <- c("location_type", "cellphone_access", "gender_of_respondent", 
                  "relationship_with_head", "marital_status", "education_level", "job_type")
test_data[nom_variable] <- lapply(test_data[nom_variable], as.factor)
#Create dummy variables
dummies <- dummyVars(~ ., data = test_data)
test_data_dummy <- predict(dummies, newdata = test_data) %>% as.data.frame()
#Remove target variable and separate x_test and y_test
y_test <- test_data$bank_account
x_test <- test_data_dummy
#encode the target variables as factors
y_train<-factor(train_data$bank_account, levels = c(1,0))
y_test<-factor(test_data$bank_account, levels = c(1,0))
############### model training ##########################################
library(randomForest)
rand_model <-caret::train(x_train,y_train, method="rf",
tuneGrid=expand.grid(mtry=3),
trControl=trainControl(method = "cv", number = 5, verboseIter = T))
rand_model
max(rand_model$results$Accuracy)
##get variable importance
var_imp <-varImp(rand_model, scale = F)
var_imp
plot(var_imp, main="feature importance of the random forest on the training data")
#artificial neural networks
neu_net_model <-caret::train(x_train, y_train, method ="avNNet",
preProcess = c("center","scale","nzv"),
tuneGrid=expand.grid(size=1, decay=0.1, bag=F),
trControl=trainControl(method = "cv",number = 5,verboseIter = T),
importance=T)
neu_net_model
max(neu_net_model$results$Accuracy)#the accuracy of the model is 100%
##get variable importance
var_imp <-varImp(neu_net_model, scale = F)
var_imp
plot(var_imp, main="feature importance of the artificial neural network on the training data")
##(extreme) gradient boosted machines, (xgboost) 
xgboost_model <-caret::train(x_train, y_train, method="xgbTree",
tuneGrid=expand.grid(nrounds=50, max_depth=5,
colsample_bytree=c(.8,1), subsample=c(.8,1),
min_child_weight=c(1,5,10), eta=0.1,gamma=0),
trControl=trainControl(method = "cv",number = 5, verboseIter = T))
xgboost_model
max(xgboost_model$results$Accuracy)
##get variable importance
var_imp <-varImp(xgboost_model, scale = F)
var_imp
plot(var_imp, main="feature importance of the XGBOOST MODEL on the training data")
plot(var_Imp, main="Feature importance of xgBoost model on the training data")
results <- data.frame(Model = c(rand_model$method,neu_net_model$method, xgboost_model$method),
                      Accuracy = c(max(rand_model$results$Accuracy), max(neu_net_model$results$Accuracy), 
                      max(xgboost_model$results$Accuracy)))
results %>% ggplot(aes(x=Model, y=Accuracy, label=paste(round(100*Accuracy,1),"%"))) +
  geom_col(fill="steelblue") + theme_minimal() + geom_label() +
  ggtitle("Accuracy in the training data by algorithm")
################## model evaluation against the test data ##############################
### predict using xgboost model
y_pred <-predict(xgboost_model, newdata = x_test)
accuracy <-confusionMatrix(y_pred, y_test)
precision(y_pred, y_test) ##proportion of true positive predictions relative to all “positive” predictions
recall(y_pred, y_test)  ##proportion of true positive predictions relative to all actual positives
F_meas(y_pred, y_test) ##harmonic mean of precision and recall
### predict using random forest model
y_pred <-predict(rand_model, newdata = x_test)
accuracy <-confusionMatrix(y_pred, y_test)
precision(y_pred, y_test) ##proportion of true positive predictions relative to all “positive” predictions
recall(y_pred, y_test)  ##proportion of true positive predictions relative to all actual positives
F_meas(y_pred, y_test) ##harmonic mean of precision and recall
### predict using artificial neural networks
y_pred <-predict(neu_net_model, newdata = x_test)
accuracy <-confusionMatrix(y_pred, y_test)
precision(y_pred, y_test) ##proportion of true positive predictions relative to all “positive” predictions
recall(y_pred, y_test)  ##proportion of true positive predictions relative to all actual positives
F_meas(y_pred, y_test) ##harmonic mean of precision and recall
