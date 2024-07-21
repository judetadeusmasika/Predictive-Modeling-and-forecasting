# loading packages
library(tidyverse)
library(caret)
library(OneR)
library(gt)
library(gtsummary)
## set the working directory
setwd("C:/Users/Baha/Downloads")
##load the datasets
happiness_2015 <-read.table("2015.csv", sep = ",", header = T)
happiness_2016 <-read.table("2016.csv", sep = ",", header = T)
happiness_2015 |> 
  gt()
happiness_2016 |> 
  gt()
## combine the two datasets using only columns common in both the datasets
common_feats <-colnames(happiness_2016)[which(colnames(happiness_2016) %in% colnames(happiness_2015))]
##features and response variable for modeling
feats <-setdiff(common_feats, c("Country",
"Happiness.Rank","Happiness.Score"))
response <-"Happiness.Score"
##combine data from 2015 and 2016
happiness_data <-rbind(select(happiness_2015, one_of(c(feats, response))),
select(happiness_2016, one_of(c(feats, response))))
str(happiness_data)
objects(happiness_data)
##The response variable happiness score is on a numeric scale. 
##OneR could also perform regression but here, I want to compare classification tasks. 
##For classifying happiness, I create three bins for low, medium and high values of the happiness score.
##In order to not having to deal with unbalanced data, I am using the bin() function from OneR with method = "content"
##For plotting the cut-points, I am extracting the numbers from the default level names.
happiness_data$Happiness.Score.1 <-bin(happiness_data$Happiness.Score, nbins = 3,
method = "content")

intervals <-paste(levels(happiness_data$Happiness.Score.1), collapse = " ")
intervals <-gsub("\\(|]", "", intervals)
intervals <-gsub(","," ", intervals)
intervals <-as.numeric(unique(strsplit(intervals," ")[[1]]))
happiness_data |> 
  ggplot() +
  geom_density(aes(x=Happiness.Score), 
color="blue", fill="blue", alpha=0.4) +
  geom_vline(xintercept = intervals[2]) +
  geom_vline(xintercept = intervals[3])
##Now I am removing the original happiness score column 
##from the data for modeling and rename the factor levels of the response 
happiness_data <-select(happiness_data, -Happiness.Score) |> 
  mutate(Happiness.Score.1 = plyr::revalue(Happiness.Score.1, 
  c("(2.83,4.79]"="low",
"(4.79,5.89]"="medium", "(5.89,7.59]"="high")))
str(happiness_data)
## exploring the 9 features individually from the dataset
##ploting the categorical variable; Region
happiness_data |> 
  ggplot(aes(x=Region, fill=Happiness.Score.1)) +
  geom_bar(position = "dodge",alpha=0.7) +
  theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1),
plot.margin = unit(c(0,0,0,1.5),
"cm")) +
  scale_fill_brewer(palette = "Set1")
###This plots shows that there are a few regions with very strong biases 
##in happiness: People in Western Europe, Australia, New Zealand, North America, 
##Latin American and the Caribbean tend to me in the high happiness group, while people
##in sub-saharan Africa and Southern Asia tend to be the least happiest.

###The remaining quantitative variables show happiness biases to varying degrees: 
##e.g. low health and life expectancy is strongly biased towards low happiness, 
##economic factors, family and freedom show a bias in the same direction, albeit not as strong.
happiness_data |> 
  gather(x,y,Economy..GDP.per.Capita.:Dystopia.Residual) |> 
  ggplot(aes(x=y,fill=Happiness.Score.1)) +
  geom_histogram(alpha=.7) +
  facet_wrap(~x, scales = "free",ncol = 4) +
  scale_fill_brewer(palette = "Set1")
####While OneR could also handle categorical data, in this example,
##I only want to consider the quantitative features to show the 
##differences between OneR and other machine learning algorithms.
happiness_data <-select(happiness_data, -Region)

############# modeling ##############################
## configure multicore
library(doParallel)
cluster_object <-makeCluster(detectCores())
registerDoParallel(cluster_object)

##partition the data into training and testing sets
set.seed(42)
index<-createDataPartition(happiness_data$Happiness.Score.1,p=.7,list = F)
train_data <-happiness_data[index, ]
test_data <-happiness_data[-index, ]
dplyr::glimpse(train_data)
dplyr::glimpse(test_data)

############# OneR ############################
###OneR only accepts categorical features. Because we have numerical
##features, we need to convert them to factors by splitting them into
##appropriate bins. While the original OneR algorithm splits the values 
##into ever smaller factors, this has been changed in this R-implementation
##with the argument of preventing overfitting. We can either split the data into
##pre-defined numbers of buckets (by length, content or cluster) or we can use 
##the optbin() function to obtain the optimal number of factors from pairwise logistic
##regression or information gain.

##default method length
p1 <-bin(train_data,nbins = 5, method = "length")
p1
library(ggplot2)
library(reshape2)
# Melting the data for ggplot
melted_data <- melt(p1, id.vars = "Happiness.Score.1")
str(melted_data)
# Create the plot
ggplot(melted_data, aes(x = value, fill = Happiness.Score.1)) +
  geom_bar(position = "dodge",alpha=.7) +
  facet_wrap(~variable, scales = "free_x") +
  theme_minimal() +
  labs(x = "y", y = "count", fill = "Happiness.Score.1",
       title = "This is how the data looks like following discretization:\nDefault method") +
  theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1),
plot.margin = unit(c(0,0,0,1.5),
"cm")) +
  scale_fill_brewer(palette = "Set1")
#method content
p2<-bin(train_data, nbins = 5, method = "content")
melted_data <- melt(p2, id.vars = "Happiness.Score.1")
str(melted_data)
# Create the plot
ggplot(melted_data, aes(x = value, fill = Happiness.Score.1)) +
  geom_bar(position = "dodge",alpha=.7) +
  facet_wrap(~variable, scales = "free_x") +
  theme_minimal() +
  labs(x = "y", y = "count", fill = "Happiness.Score.1",
       title = "5 bins with content method") +
  theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1),
plot.margin = unit(c(0,0,0,1.5),
"cm")) +
  scale_fill_brewer(palette = "Set1")
#method cluster
p3<-bin(train_data, nbins = 3, method = "cluster")
melted_data <-melt(p3, id.vars = "Happiness.Score.1")
str(melted_data)
# Create the plot
ggplot(melted_data, aes(x = value, fill = Happiness.Score.1)) +
  geom_bar(position = "dodge",alpha=.7) +
  facet_wrap(~variable, scales = "free_x") +
  theme_minimal() +
  labs(x = "y", y = "count", fill = "Happiness.Score.1",
       title = " 3 bins with Cluster method") +
  theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1),
plot.margin = unit(c(0,0,0,1.5),
"cm")) +
  scale_fill_brewer(palette = "Set1")
##optimal bin number logistic regression
p4<-optbin(formula=Happiness.Score.1~.,data=train_data,
method="logreg")
melted_data <-melt(p4, id.vars = "Happiness.Score.1")
# Create the plot
ggplot(melted_data, aes(x = value, fill = Happiness.Score.1)) +
  geom_bar(position = "dodge",alpha=.7) +
  facet_wrap(~variable, scales = "free_x") +
  theme_minimal() +
  labs(x = "y", y = "count", fill = "Happiness.Score.1",
       title = "Optimal bin number according to Logistic regression method") +
  theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1),
plot.margin = unit(c(0,0,0,1.5),
"cm")) +
  scale_fill_brewer(palette = "Set1")
##optimal bin number information gain
p5<-optbin(formula=Happiness.Score.1~.,data=train_data,
method="infogain")
melted_data <-melt(p5, id.vars = "Happiness.Score.1")
# Create the plot
ggplot(melted_data, aes(x = value, fill = Happiness.Score.1)) +
  geom_bar(position = "dodge",alpha=.7) +
  facet_wrap(~variable, scales = "free_x") +
  theme_minimal() +
  labs(x = "y", y = "count", fill = "Happiness.Score.1",
       title = "Optimal bin number according to Information gain method") +
  theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1),
plot.margin = unit(c(0,0,0,1.5),
"cm")) +
  scale_fill_brewer(palette = "Set1")
############## model building ###############
##Now I am running the OneR models. During model building, the chosen 
##attribute/feature with highest accuracy along with the top 7 features
##decision rules and accuracies are printed. Unfortunately, this information 
##is not saved in the model object; this would have been nice in order to 
##compare the importance of features across models later on.

# Model building with OneR
model_oneR <- for (i in 1:5) {
  data <- get(paste0("p", i))
  print(model <- OneR(formula = Happiness.Score.1 ~ ., data = data, verbose = TRUE))
  assign(paste0("model_", i), model)
}

######################## model evaluation ################################
##The function eval_model() prints confusion matrices for absolute and relative predictions, as well as accuracy,
##error and error rate reduction. For comparison with other models, it would have been convenient to be able to extract 
##these performance metrics directly from the eval_model object, instead of only the confusion matrix and values of 
##correct/all instances and having to re-calculate performance metrics again manually.

model_evaluation <- for(
  i in 1:5
) {
  model = get(paste0("model_", i))
  eval_model(predict(model, test_data),
test_data$Happiness.Score.1)
}

####Because I want to calculate performance measures for the different classes separately and like to
##have a more detailed look at the prediction probabilities I get from the models, I prefer to obtain 
##predictions with type = "prob. While I am not looking at it here, this would also allow me to test
##different prediction thresholds.

for(i in 1:5) {
  model = get(paste0("model_", i))
  pred <- data.frame(model = paste0("model_", i),
sample_id = 1:nrow(test_data),
predict(model, test_data, type = "prob"),
actual = test_data$Happiness.Score.1)
  pred$prediction <-colnames(pred)[3:5] [apply(pred[, 3:5], 1, which.max)]
  pred$correct <- ifelse(pred$actual == pred$prediction, "correct", "wrong")
  pred$pred_prob <-NA

  for(j in 1:nrow(pred)) {
    pred[j, "pred_prob"] <-max(pred[j, 3:5])
  }
  if(i == 1) {
    pred_df <- pred
  } else {
    pred_df <- rbind(pred_df, pred)
  }
}


###################### comparing other algorithms ###########################
## decision trees
##Economy GDP per capita is the second highest node here, the best predictor here would 
## be health and life expectancy.
library(rpart)
library(rpart.plot)
attach(happiness_data)
set.seed(42)
decision_tree_model <- rpart(Happiness.Score.1~.,
data = train_data,
method = "class",
control = rpart.control(xval = 10),
parms = list(split = "information"))
rpart.plot(decision_tree_model, extra = 100)

##In order to compare the models, I am producing the same output table for predictions
## from this model and combine it with the table from the OneR models.
# Perform prediction and inspect the output
pred_probs <- predict(decision_tree_model, test_data, type = "prob")
str(pred_probs)
# Create the prediction dataframe
pred <- data.frame(
  model = "rpart",
  sample_id = 1:nrow(test_data),
  pred_probs,
  actual = test_data$Happiness.Score.1
)
  pred$prediction <- colnames(pred)[3:5][apply(pred[, 3:5], 1, which.max)]
  pred$correct <- ifelse(pred$actual == pred$prediction, "correct", "wrong")
  pred$pred_prob <- NA
  
  for (j in 1:nrow(pred)) {
    pred[j, "pred_prob"] <- max(pred[j, 3:5])
  }

pred_df_final <-rbind(pred_df,pred)
pred_df_final


#### random forest ##################
set.seed(42)
model_rf <-caret::train(Happiness.Score.1~.,
data = train_data,
method = "rf",
trControl = trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5,
  verboseIter = F
))
model_rf
###The varImp() function from caret shows us
##which feature was of highest importance for the model and its predictions.
varImp(model_rf)
# Perform prediction and inspect the output
pred_probs <- predict(model_rf, test_data, type = "prob")
# Create the prediction dataframe
pred <- data.frame(
  model = "rf",
  sample_id = 1:nrow(test_data),
  pred_probs,
  actual = test_data$Happiness.Score.1
)
  pred$prediction <- colnames(pred)[3:5][apply(pred[, 3:5], 1, which.max)]
  pred$correct <- ifelse(pred$actual == pred$prediction, "correct", "wrong")
  pred$pred_prob <- NA
  
  for (j in 1:nrow(pred)) {
    pred[j, "pred_prob"] <- max(pred[j, 3:5])
  }
pred_df_final <-rbind(pred_df_final, pred)
pred_df_final


########### Extreme gradient boosting trees #############
set.seed(42)
model_xgb <-caret::train(Happiness.Score.1 ~.,
data = train_data,
method ="xgbTree",
trControl=trainControl(method = "repeatedcv",
number = 10,
repeats = 5,
verboseIter = F))
model_xgb
###As before, we again find Economy GDP per capita as most important feature.
varImp(model_xgb)
# Perform prediction and inspect the output
pred_probs <- predict(model_xgb, test_data, type = "prob")
# Create the prediction dataframe
pred <- data.frame(
  model = "xgb",
  sample_id = 1:nrow(test_data),
  pred_probs,
  actual = test_data$Happiness.Score.1
)
  pred$prediction <- colnames(pred)[3:5][apply(pred[, 3:5], 1, which.max)]
  pred$correct <- ifelse(pred$actual == pred$prediction, "correct", "wrong")
  pred$pred_prob <- NA
  
  for (j in 1:nrow(pred)) {
    pred[j, "pred_prob"] <- max(pred[j, 3:5])
  }
pred_df_final <-rbind(pred_df_final, pred)
pred_df_final

############ Neural network ################
set.seed(42)
library(RSNNS)
model_nn <- caret::train(Happiness.Score.1 ~ .,
                         data = train_data,
                         method = "mlp",
                         trControl = trainControl(method = "repeatedcv", 
                                                  number = 10, 
                                                  repeats = 5, 
                                                  verboseIter = FALSE))
##And Economy GDP per capita is again the most important feature!
varImp(model_nn)
# Perform prediction and inspect the output
pred_probs <- predict(model_nn, test_data, type = "prob")
# Create the prediction dataframe
pred <- data.frame(
  model = "nn",
  sample_id = 1:nrow(test_data),
  pred_probs,
  actual = test_data$Happiness.Score.1
)
  pred$prediction <- colnames(pred)[3:5][apply(pred[, 3:5], 1, which.max)]
  pred$correct <- ifelse(pred$actual == pred$prediction, "correct", "wrong")
  pred$pred_prob <- NA
  
  for (j in 1:nrow(pred)) {
    pred[j, "pred_prob"] <- max(pred[j, 3:5])
  }
pred_df_final <-rbind(pred_df_final, pred)
pred_df_final

################# model comparisons ###################
##Now to the final verdict: How do the different models compare?
##The first plot below shows the prediction probabilites for the three 
##happiness levels low, medium and high for each test data instance. 
##For each instance, only the prediction probability of the predicted class 
##(i.e. with the highest value) is shown. The upper row shows correct predictions, 
##the lower row shows wrong predictions.
#Sometimes, it is obvious from such a plot if a more stringent prediction 
##threshold could improve things (when wrong predictions tend to be close 
##to the threshold). With three classes to predict, this is obviously not
##as trivial as if we only had two but the same principle holds true: 
##the smaller the prediction probability, the more uncertain it tends to be.
pred_df_final |> 
  ggplot(aes(x=actual, y=pred_prob, fill=prediction,color=prediction)) +
  geom_boxplot(alpha=.7) +
  facet_grid(correct ~model) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1")

##Probably the most straight-forwards performance measure is accuracy: 
##i.e. the proportion of correct predictions vs the total number of
##instances to predict. The closer to 1, the better the accuracy.

##Not surprisingly, the more complex models tend to be more accurate - albeit only slightly.

pred_df_final |> 
  group_by(model) |> 
  dplyr::summarise(correct = sum(correct=="correct")) |> 
  mutate(accuracy = correct/nrow(test_data)) |> 
  ggplot(aes(x=model, y=accuracy,fill=model)) +
  geom_bar(stat = "Identity") +
  scale_fill_brewer( palette = "Set1"
  )
##When we look at the three classes individually, it looks a bit more 
##complicated but most models achieved highest accuracy for class “high”.

pred_df_final |> 
  group_by(model, prediction) |> 
  dplyr::summarise(correct=sum(correct=="correct"),
n=n()) |> 
  mutate(accuracy=correct/n) |> 
  ggplot(aes(x=model, y= accuracy, fill=prediction)) +
  geom_bar(stat = "Identity", position = "dodge") +
  scale_fill_brewer(palette = "Set1")
sessionInfo()
