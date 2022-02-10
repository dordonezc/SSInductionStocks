library(e1071)
library(glmnet)
library(caret)
library(tidyverse)
library(randomForest)
setwd("C:/Users/dordo/Dropbox/Capstone Project")
# Read Data ---------------------------------------------------------------

source("Code/Modelling/Doc2VecReadData.R")

# Direction ---------------------------------------------------------------

## This part generates the lags of the sentiment to include in the models
## This parts generates the principal components
ncp <- 70

pca_mod <- reg_data %>% select(-c(Date:Vol, Ehat)) %>% 
  princomp(cor = T)

summary(pca_mod)
factoextra::fviz_screeplot(pca_mod, ncp=100)

x_reg_pca <- pca_mod$scores[,1:ncp] %>% as_tibble()
colnames(x_reg_pca) <- str_c("Comp", 1:ncp)
reg_pca <- reg_data %>% 
  select(1:4) %>% 
  bind_cols(x_reg_pca)

res <- x_reg_pca %>% 
  summarise(across(.fns = ~list(ccf(.x, reg_pca$Dir))))

## This part generates the lags of the sentiment to include in the models
nlag <- 2
reg_data <- reg_pca %>% 
  select(-c(Date, Ret, Dir)) %>% 
  mutate(across(everything(), .fns = map(1:nlag, ~partial(dplyr::lag, n=.x)),
                .names = "L{.fn}{.col}"))

# Glmnet ------------------------------------------------------------------

## Separate in Y and X
y_reg <- reg_data %>% 
  select(Vol) %>% 
  pull() 

x_reg <- reg_data %>%  
  select(-Vol) %>% 
  mutate(across(.fns = as.numeric)) %>% 
  as.matrix()

# EDA -----------------------------------------------------------------

## PCA
# pca_mod <- princomp(x_reg[, -(ncol(x_reg)-c(0,1))], cor = T)
# factoextra::fviz_screeplot(pca_mod, ncp = 100)
# pca_mod %>% summary()
# 
# ## Can keep 30- 50%, 40-60%, 60-70%. 
# 
# ## PCA Xreg version
# ncomp <- 60
# reg_pca <- reg_data %>% select(ends_with("Vol")) %>% 
#   bind_cols(pca_mod$scores[,1:ncomp] %>%  as_tibble)
# 
# x_reg_pca <- reg_pca %>%  
#   select(-Vol) %>% 
#   mutate(across(.fns = as.numeric)) %>% 
#   as.matrix()

## Glmnet to choose predictors
mode_net <- cv.glmnet(x_reg[-(1:nlag),], y_reg[-(1:nlag)], family = "gaussian")
plot(mode_net)
mode_net %>% coef("lambda.min")

## Glmnet with pca
# mode_net_pca <- cv.glmnet(x_reg_pca[-(1:nlag),], y_reg[-(1:nlag)], family = "gaussian")
# plot(mode_net_pca)
# mode_net_pca %>% coef("lambda.min")

# CARET -------------------------------------------------------------------

## Split for caret
set.seed(728) 
train.control <- trainControl(method = "cv", number = 5)

# Glm + AIC with Caret ---------------------------------------------------------------
set.seed(752)

test_data <- reg_pca %>% slice(holdout)
actual <- test_data %>% pull(Vol)

# Train the model
model <- train(Vol ~., data = reg_pca %>% 
                 slice(-holdout), method = "glmStepAIC",
               trControl = train.control, na.action = na.omit)

pred_glmaic <- predict(model, test_data)

## This is worst than the model with restricted options and cv

# SVM Caret ---------------------------------------------------------------------
set.seed(752)

# Train the model
model <- train(Vol ~., data = reg_pca  %>%  slice(-holdout), method = 'svmRadialSigma',
               trControl = train.control, na.action = na.omit, tuneLength = 5)

pred_svm <- predict(model, test_data)

# Gradient Boosting Caret -------------------------------------------------
set.seed(752)

# Train the model
model <- train(Vol ~., data = reg_pca  %>%  slice(-holdout), method = 'gcvEarth',
               trControl = train.control, na.action = na.omit, tuneLength = 5)

pred_mars <- predict(model, test_data)

# Random Forest -----------------------------------------------------------
set.seed(752)

mod_rf <- train(Vol ~., data = reg_pca  %>%  slice(-holdout), method = 'rf',
                trControl = train.control, na.action = na.omit, tuneLength = 5)

pred_rf <- predict(mod_rf, test_data)

# Consolidate -------------------------------------------------------------

dafiles <- ls(pattern="pred")
all_preds <- map_dfr(dafiles %>% setNames(dafiles), get) %>% 
  bind_cols("Actual"=actual) %>% 
  mutate(holdout_id = row_number()) %>% 
  pivot_longer(-c(holdout_id, Actual))

## MSE
all_preds %>% arrange(name, holdout_id) %>% 
  group_by(name) %>%  
  summarise(mean(abs(Actual-value)))

write_csv(all_preds, "Results/Volatility/Doc2VecSentiment.csv")

