####install the tidyAML package
install.packages("tidyAML")
#load the required libraries
library(tidymodels)
library(tidyAML)
###Part of the reason to use {tidyAML} is so that you can generate many models of your data set
fast_regression_parsnip_spec_tbl(.parsnip_fns = "linear_reg")
fast_regression_parsnip_spec_tbl(.parsnip_eng = c("lm","glm"))
fast_regression_parsnip_spec_tbl(.parsnip_eng = c("lm","glm","gee"),
.parsnip_fns = "linear_reg")
#the function also adds a class
class(fast_regression_parsnip_spec_tbl())
#### want to create a non-tuning model spec without using the fast_regression_parsnip_spec_tbl() function
create_model_spec(.parsnip_eng = list("lm","glm","glmnet","cubist"),
  .parsnip_fns = list("linear_reg",
"linear_reg",
"linear_reg",
"cubist_rules"))

create_model_spec(
  .parsnip_eng = list("lm","glm","glmnet","cubist"),
  .parsnip_fns = list(
    "linear_reg",
    "linear_reg",
    "linear_reg",
    "cubist_rules"
  ),
  .return_tibble = FALSE
)

