##load the required dependencies/libraries
library(knitr)
library(tidyverse)
library(ggplot2)
library(mice)
library(lattice)
library(reshape2) 
library(DataExplorer)
#importing the datasets
## Need to fetch the excel file
path <- "https://code.datasciencedojo.com/datasciencedojo/datasets/raw/master/Default%20of%20Credit%20Card%20Clients/default%20of%20credit%20card%20clients.csv"
credit <-read.csv(file = path, header = TRUE)
colnames(credit) <- as.character(unlist(credit[1,]))
credit = credit[-1, ]
head(credit)
##To avoid any complications ahead, we'll rename our target variable "default payment next month" to a name without spaces using the code below.
colnames(credit)[colnames(credit)=="default payment next month"] <-"default_payment"
head(credit)
################ exploratory data analysis ######################
dim(credit)
str(credit)
##the variables are all characters, we convert then to integer
credit[,1:25] <-sapply(credit[,1:25], as.numeric)
str(credit)
attach(credit)
#check the summary of the data
summary(credit)
introduce(credit)
count(credit,vars=EDUCATION)
count(credit, vars=MARRIAGE)
##replace 0's with NAN, and replace others too
credit$EDUCATION[credit$EDUCATION==0] <-4
credit$EDUCATION[credit$EDUCATION==5] <-4
credit$EDUCATION[credit$EDUCATION==6] <-4
credit$MARRIAGE[credit$MARRIAGE==0] <-3
count(credit, vars=EDUCATION)
count(credit, vars=MARRIAGE)
##correlation heatmap
plot_correlation(na.omit(credit), maxcat = 5L)
##We can observe the week correlation of AGE, 
##BILL_AMT1, BILL_AMT2, BILL_AMT3, BILL_AMT4, BILL_AMT5, BILL_AMT6 with our target variable.
#plot the histogram of the PAY variables
plot_histogram(credit) ###The distribution above shows that nearly all PAY attributes are rightly skewed.
############### feature engeneering ##############################
#deleting the columns with low correlation with the final target variable, default payment
credit_new <- select(credit,
-one_of('ID','AGE', 'BILL_AMT2',
       'BILL_AMT3','BILL_AMT4','BILL_AMT5','BILL_AMT6'))
head(credit_new)
################ data pre-processing #############################
#scaling the data
credit_new[,1:17] <-scale(credit_new[,1:17])
head(credit_new)
##spliting the data into training and testing sets
##create a list of random numbers from 1 to the number of rows from the actual data
# then get 70% of the data into the training data
credit_split <-sort(sample(nrow(credit_new), nrow(credit_new)*.7))
##creating the training data set by selecting the output row values
train_data <-credit_new[credit_split,]
##creating the testing data set by not selecting the output row values
test_data <-credit_new[-credit_split,]
library(dplyr)
dplyr::glimpse(train_data)
dplyr::glimpse(test_data)
##################### model development ###############################
##fit a logistic regression model using the training data
fit_logit <-glm(default_payment~., data = train_data, family = binomial(link = "logit"))
summary(fit_logit)

################# prediction ################################
#to predict using the logistic regression model, probabilities were obtained
predict_logit <-predict(fit_logit, newdata = test_data,
type = "response")
##look at probability output
head(predict_logit,10)
y_predict <-ifelse(predict_logit>0.5, 1,0)
head(y_predict,10)
##################### model evaluation #######################
#confusion matrix
library(Metrics)
library(party)
library(caret)
accuracy <-table(y_predict, test_data[,18])
sum(diag(accuracy))/sum(accuracy)
