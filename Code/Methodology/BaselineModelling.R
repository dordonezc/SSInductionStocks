library(e1071)
library(glmnet)
library(caret)
library(tidyverse)
library(randomForest)
setwd("C:/Users/USUARIO/Dropbox/Capstone Project")
# Read Data ---------------------------------------------------------------

source("Code/Modelling/BaselineReadData.R")

# Review Cross Correlation ---------------------------------------------------------------

## CCF for EDA 

## Twitter
with(all_data, ccf(Dir, PolarityTwitter, na.action = na.exclude))

## SA
with(all_data, ccf(Dir, PolaritySA, na.action = na.exclude))

## WSJ
with(all_data, ccf(Dir, PolarityWSJ, na.action = na.exclude))

## The correlation is apparently stronger with the lead in both cases
## WSJ is weird

# Direction ---------------------------------------------------------------

## This part generates the lags of the sentiment to include in the models
nlag <- 7

reg_data <- all_data %>% 
  select(Dir, starts_with("Polarity")) %>% 
  mutate(across(.fns = map(1:nlag, ~partial(dplyr::lag, n=.x)),
                .names = "L{.fn}{.col}"))

# Glmnet ------------------------------------------------------------------

## Separate in Y and X
y_reg <- reg_data %>% 
  select(Dir) %>% 
  pull() 

x_reg <- reg_data %>%  
  select(-Dir) %>% 
  mutate(across(.fns = as.numeric)) %>% 
  as.matrix()


## Basic logit for exploration
mod_glm <- glm(y_reg[-1] ~ x_reg[-1, 1:4], family = "binomial",
               subset = setdiff(2:nrow(x_reg), holdout) - 1)
mod_glm %>% summary()

## Seeking Alpha is statistically relevant

## Glmnet to choose predictors
mode_net <- cv.glmnet(x_reg[-(1:nlag),], y_reg[-(1:nlag)], family = "binomial")
plot(mode_net)
mode_net %>% coef("lambda.min")


# CARET -------------------------------------------------------------------

## Split for caret
set.seed(728) 
train.control <- trainControl(method = "cv", number = 5)

# Glm with Caret ----------------------------------------------------------
set.seed(752)

reg_data_f <- reg_data %>% select(1:5) 
test_data <- reg_data %>% slice(holdout)
actual <- test_data %>% pull(Dir)

model <- train(Dir ~ ., data =reg_data_f %>% slice(-holdout), method = "glm",
               trControl = train.control, na.action = na.omit)

pred_glm <- predict(model, test_data)
table(pred_glm, actual)


# Glm + AIC with Caret ---------------------------------------------------------------
set.seed(752)

test_data <- reg_data %>% slice(holdout)

# Train the model
model <- train(Dir ~., data = reg_data  %>%  slice(-holdout), method = "glmStepAIC",
               trControl = train.control, na.action = na.omit)

pred_glmaic <- predict(model, test_data)
table(pred_glmaic, actual)

## This is worst than the model with restricted options and cv

# SVM Caret ---------------------------------------------------------------------
set.seed(752)

# Train the model
model <- train(Dir ~., data = reg_data  %>%  slice(-holdout), method = 'svmRadialSigma',
               trControl = train.control, na.action = na.omit, tuneLength = 5)

pred_svm <- predict(model, test_data)
table(pred_svm, actual)

# Gradient Boosting Caret -------------------------------------------------
set.seed(752)

# Train the model
model <- train(Dir ~., data = reg_data  %>%  slice(-holdout), method = 'adaboost',
               trControl = train.control, na.action = na.omit, tuneLength = 5)

pred_ada <- predict(model, test_data)
table(pred_ada, actual)


# Random Forest -----------------------------------------------------------
set.seed(752)

mod_rf <- train(Dir ~., data = reg_data  %>%  slice(-holdout), method = 'rf',
                trControl = train.control, na.action = na.omit, tuneLength = 5)

pred_rf <- predict(mod_rf, test_data)
table(pred_rf, actual)

# Consolidate -------------------------------------------------------------

dafiles <- ls(pattern="pred")
all_preds <- map_dfr(dafiles %>% setNames(dafiles), get) %>% 
  bind_cols("Actual"=actual) %>% 
  mutate(holdout_id = row_number()) %>% 
  pivot_longer(-c(holdout_id, Actual))

write_csv(all_preds, "Results/Direction/BaselineSentimentLast3.csv")
