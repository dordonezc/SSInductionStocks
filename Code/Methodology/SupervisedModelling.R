library(e1071)
library(glmnet)
library(caret)
library(tidyverse)
library(randomForest)
setwd("C:/Users/dordo/Dropbox/Capstone Project")
# Read Data ---------------------------------------------------------------

source("Code/Modelling/SupervisedReadData.R")

res <- all_data %>% select(-c(Ret, Ehat)) %>% 
  pivot_longer(!c(Date:Vol)) %>% 
  group_by(name) %>% 
  nest() %>% 
  mutate(ccf_dir = map(data, ~ccf(.x[[2]], .x[[4]], plot = F)),
         ccf_vol = map(data, ~ccf(.x[[3]], .x[[4]], plot = F)))

res$ccf_dir %>% walk(plot)

# Direction ---------------------------------------------------------------

## This parts generates the principal components
ncp <- 5

pca_mod <- all_data %>% select(-c(Date:Vol, Ehat)) %>% 
  princomp(cor = T)

summary(pca_mod)
factoextra::fviz_screeplot(pca_mod)

x_reg_pca <- pca_mod$scores[,1:ncp] %>% as_tibble()
colnames(x_reg_pca) <- str_c("Comp", 1:ncp)
reg_pca <- all_data %>% 
  select(1:4) %>% 
  bind_cols(x_reg_pca)

res <- x_reg_pca %>% 
  summarise(across(.fns = ~list(ccf(.x, all_data$Dir))))

## This part generates the lags of the sentiment to include in the models
nlag <- 5
reg_data <- reg_pca %>% 
  select(-c(Date, Ret, Vol)) %>% 
  mutate(across(everything(), .fns = map(1:nlag, ~partial(dplyr::lag, n=.x)),
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

with(reg_data, chisq.test(Dir[-holdout], L1Dir[-holdout]))
with(reg_data, chisq.test(Dir[-holdout], lag(Dir, n =2)[-holdout]))


# EDA -----------------------------------------------------------------

## Glmnet to choose predictors
mode_net <- cv.glmnet(x_reg[-(1:nlag),], y_reg[-(1:nlag)], family = "binomial")
plot(mode_net)
mode_net %>% coef("lambda.min")

# CARET -------------------------------------------------------------------

## Split for caret
set.seed(728) 
train.control <- trainControl(method = "cv", number = 5)

# Glm + AIC with Caret ---------------------------------------------------------------
set.seed(752)

test_data <- reg_data %>% slice(holdout)
actual <- test_data %>% pull(Dir)

# Train the model
model <- train(Dir ~., data = reg_data %>% 
                 slice(-holdout), method = "glmStepAIC",
               trControl = train.control, na.action = na.omit)

pred_glmaic <- predict(model, test_data)
table(pred_glmaic, actual)

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

write_csv(all_preds, "Results/Direction/SupervisedSentimentSentiPropLast3.csv")
