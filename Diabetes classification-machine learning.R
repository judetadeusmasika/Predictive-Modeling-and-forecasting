#Preliminaries:
knitr::opts_chunk$set(message=FALSE, warning=FALSE, 
  eval = FALSE) #set eval = TRUE when run first

rm(list=ls())
###load the required libraries
library(tidyverse)
library(tidymodels)
library(themis)
library(doParallel)
library(gtsummary) 
library(gt)
library(bonsai) 
library(discrim)
library(finetune)
library(patchwork)
library(vip)
library(DALEXtra)  

### In order to be able to reliably predict the disease we first need to learn the association 
##between risk factors and diabetes from data, and secondly, we need to provide out-of-sample 
##predictions using new patient data. But how can we learn about the association between
##available features (risk factors) and diabetes? We use the tidymodels framework and 
##apply different machine learning methods to the Kaggle diabetes data set. 


######################### data and model strategy ###############################
##set the working directory
setwd("C:/Users/Baha/OneDrive/Documents/Welcome to Data Science Project")
diabetes_df <-read.csv("diabetes_prediction_dataset.csv")
str(diabetes_df)
diabetes_df <- diabetes_df %>%
  mutate(diabetes = as.factor(diabetes)) %>%
  mutate(bmi = as.numeric(case_when(bmi != 'N/A' ~ bmi, TRUE ~ NA_real_))) %>%
  mutate_if(is.character, as.factor)
str(diabetes_df)
sapply(diabetes_df, function(x) table(is.na(x)))
### create data into training and testing sets
set.seed(1005)
factor(diabetes_df$diabetes, levels = c(0,1))
diabetes_split <- diabetes_df |> 
  initial_split(prop = 0.75,
  strata = diabetes)
diabetes_split
train_data <-training(diabetes_split)
test_data <-testing(diabetes_split)
dplyr::glimpse(train_data)
dplyr::glimpse(test_data)
##encode target variable as a factor
factor(train_data$diabetes, levels = c(1,0))
factor(test_data$diabetes, levels = c(1,0))
#create summary statistics:
diabetes_tab1 <- train_data %>%
  tbl_summary(
    by = diabetes,
    statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)"),
    digits = all_continuous() ~ 2
  ) %>%
  add_p(test = list(all_continuous() ~ "t.test", all_categorical() ~ "chisq.test.no.correct")) %>%
  add_overall() %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**diabetes**") %>%
  modify_caption("**Table 1: Descriptive Statistics Training Data**")
#save summary statistics:
diabetes_tab1 %>%
  as_gt()
##A first look at the dataset shows that all explanatory variables are associated with diabetes, 
##as the p-values < 0.001 suggest. As expected aging and morbidities hypertension and heart disease 
##are positively correlated with diabetes. We also see that our outcome of interest - diabetes - is very unbalanced, 
##which we should keep in mind when selecting our evaluation metric. The feature engineering -steps are pretty 
##straightforward. After having created the training and test partitions we normalize all numeric predictors
##and create dummy variables for the categorical ones. Furthermore, we down-sample the data toward the minority class.

## data preparation before training
diabetes_recipe <- recipe(diabetes ~ ., data = train_data) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_downsample(diabetes)
###We compare different model types in our analysis because we want to select the model which predicts diabetes best.
##As Domingos (2012) points out, the best model depends on the use case and cannot be known in advance. Henceforth, 
##we use XGBoost, naive Bayes, a support vector machine, a decision tree model, and logistic regression in our workflow. 
##On the one hand, we consider logistic regression as our baseline model because it is simple and needs no additional 
##hyperparameter tuning. On the other hand, simple decision trees have the advantage that they a highly interpretable 
##and are therefore part of the human decision-making toolkit

## try out different machine learning approaches
library(kernlab)
library(naivebayes)
library(partykit)
logistic_model <-logistic_reg() |> 
  set_engine("glm") |> 
  set_mode("classification")

svm_model <-svm_linear(cost = tune(), margin = tune()) |> 
  set_engine("kernlab") |> 
  set_mode("classification")

xgboost_model <-boost_tree(tree_depth = tune(), learn_rate = tune(), loss_reduction = tune(),
min_n = tune(), sample_size = tune(), trees = tune()) |> 
  set_engine("xgboost") |> 
  set_mode("classification")

naive_bayes_model <-naive_Bayes(smoothness = tune(), Laplace = tune()) |> 
  set_engine("naivebayes") |> 
  set_mode("classification")

decision_tree_model <- decision_tree(tree_depth = tune(), min_n = tune()) |> 
  set_engine(engine = "partykit") |> 
  set_mode(mode = "classification")

###################### hyperparameter tuning ##########################
##To manage the bias-variance tradeoff the model’s hyperparameters are selected by combining cross-validation
##using a space-filling grid search design. We consider ROC-AUC as our evaluation metric of interest. 
##The AUC is threshold-independent and therefore often a good choice in situations when the outcome variable
##is relatively unbalanced. (See for example Fawcett 2006). Finally, parallelization helps speed up the whole process.
##prepare cross validation
set.seed(1001)
train_folds <- vfold_cv(data = train_data, v = 8, strata = diabetes)
# Prepare workflow
wf_set <- workflow_set(
  preproc = list(mod = diabetes_recipe),
  models = list(
    log_reg = logistic_model,
    svm_linear = svm_model,
    xgboost = xgboost_model,
    naiveBayes = naive_bayes_model,
    tree = decision_tree_model
  )
)

# Prepare grid
grid_control <- control_grid(
  save_pred = TRUE,
  parallel_over = "everything",
  save_workflow = TRUE,
  event_level = "second"
)
# Prepare parallel processing
cores <- parallel::detectCores(logical = TRUE)
# Create a cluster object then register
cluster_object <- makePSOCKcluster(cores)
registerDoParallel(cluster_object)
# Start hyperparameter tuning
train_results <- wf_set %>%
  workflow_map(
    fn = 'tune_grid',
    metrics = metric_set(roc_auc),
    seed = 1503,
    resamples = train_folds,
    grid = 25,
    control = grid_control
  )
# Check results
 show_notes(train_results)
# Stop parallel cluster
stopCluster(cluster_object)
##plot results of hyperparameter tuning
p1_diab <-train_results |> 
  autoplot() +
  theme_minimal() +
  labs(title = "Figure 1: Results Hyperparameter Tuning")
p1_diab
ggsave("hyperparameter_results.png", plot = p1_diab)
##Figure 1 shows performance metrics for all experiments ranked by AUC.
##We find that XGBoost gives the best results concerning AUC followed by
##the decision tree, naive Bayes, and logistic regression.

#################### further optimization ###########################
##Can we still improve on the current result? Although we considered a space-filling grid 
##search design when tuning hyperparameters, there may be room for further improvement. 
##Hence, we apply simulated annealing, iteratively trying out little hyperparameter changes, 
##starting from the current best model.
xgb_results <-train_results |> 
  extract_workflow_set_result("mod_xgboost")
xgb_wf<-train_results |> 
  extract_workflow("mod_xgboost")
cluster_object <-makePSOCKcluster(cores)
registerDoParallel(cluster_object)
## increase performance with simulated annealing

set.seed(1005)
xgb_sa <-xgb_wf |> 
  tune_sim_anneal(
    resamples = train_folds,
    metrics = metric_set(roc_auc),
    initial = xgb_results,
    iter = 3,
    control = control_sim_anneal(verbose = T,
    no_improve = 10L,
  event_level = "second",cooling_coef = 0.1)
  )
stopCluster(cluster_object)
##save maximum auc
auc_out <-xgb_sa |> 
  collect_metrics() |> 
  slice_max(mean) |> 
  pull(mean)

##visualize sim annealing
p2_diab <-autoplot(xgb_sa, type = "performance",
metric = 'roc_auc') +
  geom_hline(yintercept = auc_out, linetype="dashed",
color='red') +
  labs(title = "Figure 2: Performance improvement by simulated annealing") +
  theme_minimal()
p2_diab
ggsave("simulated annealing.png", plot = p2_diab)


################## interpretable machine learning #########################
#One important aspect of ML classifiers is that models with high predictive 
##power do not necessarily have good explanatory power
#extract model fit after simulated annealing
xgb_fit <-xgb_sa |> 
  extract_workflow() |> 
  finalize_workflow(xgb_sa |> select_best()) |> 
  fit(data=train_data) |> 
  extract_fit_parsnip()
##variable importance plot
p3_diab <-xgb_fit |> 
  vip() +
  theme_minimal() +
  labs(title = "Figure 3: Variable importance")
p3_diab
ggsave("variable importance plot.png", plot = p3_diab)

##prepare training data for pdp (partial dependnce plot)
xgb_df <-xgb_sa |> 
  extract_workflow() |> 
  finalize_workflow(xgb_sa |> select_best()) |>
  fit(data=train_data) |> 
  extract_recipe() |> 
  bake(new_data=train_data)
explain_xgb<-explain_tidymodels(
  model = xgb_fit,
  data = (xgb_df |> dplyr::select(-diabetes)), ##data without target column
  y=xgb_df$diabetes,
  label = "xgboost",
  verbose = F
)
##create model profile
pdp_diab <-model_profile(explain_xgb, N=1000,variables = "HbA1c_level", groups='hypertension')
pdp_diab
#Create ggplot manually for HbA1c, grouped by hypertension:
p4_diab <-pdp_diab$agr_profiles |> 
  as.tibble() |> 
  mutate(RiskFactor=paste0('hypertension=',
ifelse(stringr::str_sub(`_label_`,9,9)=='-','0','1'))) |> 
  ggplot(aes(x=`_x_`,y=`_yhat_`,color=RiskFactor)) +
  geom_line(linewidth=2) +
  labs(y='Diabetes Risk score', x='HbA1c_level',
title = 'Figure4: Partial Dependence plot') +
  theme_minimal()
p4_diab
ggsave("partial dependence plot.png", plot = p4_diab)

##in Figure 4 we see that the conditional association between HbA1c level and diabetes is positive, 
##and even more pronounced for people with hypertension, which makes sense as hypertension is another
##known risk factor for diabetes. At this point, we should also remember that our predictions cannot
##be interpreted as probabilities (Van Calster et al. 2019). If we would like the predicted values 
##to equal expected probabilities we would need to recalibrate the risk scores or use another modeling strategy

################################ model testing ##############################################
##fit the new best model once on the test data at the final end of the process
test_results <-xgb_sa |> 
  extract_workflow() |> 
  finalize_workflow(xgb_sa |> select_best()) |>
  last_fit(split=diabetes_split)
##create predictions
test_p <-collect_predictions(test_results)
##construct the confusion matrix
confusion_matrix <-conf_mat(test_p, truth=diabetes, estimate=.pred_class)
confusion_matrix
library(ggplot2)
p5a_diab<-autoplot(confusion_matrix, type ="heatmap") +
  theme(legend.position = "none") +
  labs(title = 'Confusion Matrix')
p5a_diab
ggsave("confuion matrix plot.png", plot = p5a_diab)
##AUC overall
auc <-test_p |> 
  roc_auc(diabetes, .pred_1, event_level="second") |> 
  mutate(.estimate=round(.estimate,3)) |> 
  pull(.estimate)
## ROC curve
roc_curve <-roc_curve(test_p, diabetes, .pred_1, event_level="second")
p5b_diab <- roc_curve |> 
  autoplot() +
  annotate('text', x=0.3, y=0.75, label=auc) +
  theme_minimal() +
  labs(title = 'ROC-AUC')
p5b_diab
##combine both plots
p5b_diab <-p5a_diab +p5b_diab +plot_annotation('Figure 5: Evaluation on test data')
ggsave("evaluation on test data.png", plot = p5b_diab)

###We did a comparative analysis of different machine learning classifiers to predict 
##diabetes with some medical risk factors using the tidymodels framework. Then we showed 
##how to further improve the best model (in our case XGBoost) using iterative grid search 
##before validating the model on test data. In addition, we applied visualization methods to 
##better understand how the predictions are generated. Overall, we see that machine learning 
##methods can fruitfully support medical decision-making, if the model is well developed, validated, 
##and integrated carefully into the application of interest.