library(tsibble)
library(fable)
library(glmnet)


# Modelling Returns -------------------------------------------------------


## Independence with lag? Apparently not with one
table(reg_data$Dir[-holdout], lag(reg_data$Dir)[-holdout])

## Exploratory logit
mod_prw <- glm(reg_data$Dir ~ lag(reg_data$Dir), family = "binomial", 
               subset = setdiff(1:nrow(reg_data), holdout)) 
mod_prw %>% summary()
ehat <- mod_prw %>%  residuals()

## This is with pre-withened obs
set.seed(12)
cv_model <- cv.glmnet(y=y_reg[-c(1, holdout)], 
                      x=cbind(lag(y_reg)[-c(1, holdout)], x_regtrain[-1,]), family = "binomial")
cv_modele <- cv.glmnet(y=ehat, x=x_regtrain[-1,], family = "multinomial")
coef(cv_model)
coef(cv_modele)

## Nothing to do here

##-------------------------------------##
## Support Vector machine
set.seed(752)
mod_svm <- svm(y = as.factor(y_reg[-c(1, holdout)]), 
               cbind(lag(y_reg)[-c(1, holdout)], x_regtrain[-1,]),
    type = "C", kernel = "radial", cross = 5, gamma = 0.1, cost = 4)

summary(mod_svm)

## Results are ok proceed with tuning
set.seed(752)

svm_tune <- tune("svm", train.x =  cbind(lag(y_reg)[-c(1, holdout)],
                             x_regtrain[-1,]),
     train.y = as.factor(y_reg[-c(1, holdout)]),
     type = "C", kernel = "radial",
     validation.x =  cbind(lag(y_reg)[holdout],
                           x_regtest),
     validation.y = as.factor(y_reg[holdout]),
     ranges =  list(gamma = seq(0.1, 2, length.out = 20), cost = 2^(2:4)),
     tunecontrol = tune.control(sampling = "fix"))

svm_tune
## Looks promising

##------------------------------------------##
## Random Forest

## Cross validation for variable selection
# set.seed(505)
# train_index <- sample(1:length(y_reg), size = 100)
# mod_rcv <- rfcv(x_reg[train_index,], trainy = as.factor(y_reg[train_index]), step = 0.75,
#                 recursive = T)
# 
# mod_rcv

## Random Forest 

rf_mod <- randomForest(y = as.factor(y_reg[-1]), x = cbind("Lag"=lag(y_reg), x_reg)[-1,],
                       mtry = 30)

rf_mod

## Not so good should prefer svm

# Modelling Volatility ----------------------------------------------------

## Tsibble

reg_tsibble <- reg_data %>% 
  mutate(Dir=as.numeric(Dir)-1) %>% 
  mutate(across(starts_with("X"), lag, .names="{.col}_L")) %>% 
  pivot_longer(!Date, names_to = "Variable", values_to="Value") %>% 
  mutate(Date=as.Date(Date)) %>% 
  tsibble(key="Variable", index = "Date")

## Ts Modelling No predictors 

## Check ts properties
vol_tsibble <- reg_tsibble %>% 
  filter(Variable=="Vol") %>% 
  fill_gaps()

mod_arima <- vol_tsibble %>% 
  model(arima=ARIMA(Value))

mod_arima %>%  report()
mod_arima %>%  accuracy()

## Checking residuals 
ehat <- mod_arima %>%
  augment() %>% 
  filter(!is.na(.resid)) %>%  
  select(Date, .resid) 

# GLM Selection -----------------------------------------------------------

## Throw the lasso with pre-withen in case one want to add covariates
y_nw <- reg_data %>% 
  select(Vol) %>% 
  pull()

## This is with pre-withened obs
set.seed(12)
cv_model <- cv.glmnet(y=ehat$.resid[-c(1, holdout)], x=x_reg[-c(1, holdout),])
coef(cv_model)

## Non pre-withened obs
set.seed(8)
cv_model_nw <- cv.glmnet(y=y_nw[-c(1, holdout)], x=x_reg[-c(1, holdout),])
cf <- coef(cv_model_nw)
cf

vars_tou <- rownames(cf)[as.logical(abs(cf)>0)][-1]
model_nw <- lm(y_nw~x_reg[,vars_tou], subset = setdiff(1:nrow(x_reg), holdout)) 
summary(model_nw)
forecast::checkresiduals(residuals(model_nw))

## Note: Remaining autocorrelation

# TS Cross Validation --------------------------------------------------------

reg_tsibble_c <-  reg_tsibble %>% 
  pivot_wider(names_from = Variable, values_from = Value) %>% 
  fill_gaps() 

reg_cvtsibble <- reg_tsibble_c %>% 
  stretch_tsibble(.init=100) 

pop <- as.formula(str_c("Vol ~ ", str_c(vars_tou, collapse = " + ")))

fit <- reg_cvtsibble %>%
  model(arima = ARIMA(Vol ~ pdq(2,0,0)),
        lm = TSLM(pop))

test <- new_data(reg_cvtsibble, n = 1) %>% 
  left_join(reg_tsibble_c)

results <- forecast(fit, new_data = test) %>%
  group_by(.id) %>%
  mutate(h = row_number()) %>%
  ungroup()
  
results %>%
  accuracy(reg_tsibble_c) 

# PCA ---------------------------------------------------------------------

## Throw a pca on the summary matrix
model_pca <- x_reg[-1,] %>%  princomp() 
model_pca %>% factoextra::fviz_screeplot(ncp=100)
model_pca %>%  summary()
ncp <- 70
x_regpca <- model_pca$scores[,1:ncp]

## This is with pre-withened obs
set.seed(12)
cv_model_pca <- cv.glmnet(y=ehat$.resid[-c(1, holdout)], x=x_regpca[-c(holdout),])
(cf_pca <- coef(cv_model_pca, s="lambda.min"))
vars_tou_pca <- rownames(cf_pca)[as.logical(abs(cf_pca)>0)][-1]
model_nw_pca <- lm(ehat$.resid[-1]~x_regpca[,vars_tou_pca], 
                   subset = setdiff(1:nrow(x_regpca), holdout)) 
summary(model_nw_pca)

## PCA Tsibble
reg_tsibblepca <- bind_cols(reg_tsibble %>%  filter(Variable == "Vol") %>%
                              pivot_wider(names_from = Variable, values_from = Value),
                            as_tibble(rbind(NA,x_regpca))) %>% 
  fill_gaps()

pop <- as.formula(str_c("Vol ~ pdq(2, 0, 0) +", str_c(vars_tou_pca, collapse = " + ")))

reg_cvtsibble_pca <- reg_tsibblepca %>% 
  stretch_tsibble(.init=100) 

fit_pca <- reg_cvtsibble_pca %>%
  model(arima_reg = ARIMA(pop),
        arima = ARIMA(Vol ~ pdq(2,0,0)))

test_pca <- new_data(reg_cvtsibble_pca, n = 1) %>% 
  left_join(reg_tsibblepca)

results_pca <- forecast(fit_pca, new_data = test_pca) %>%
  group_by(.id) %>%
  mutate(h = row_number()) %>%
  ungroup()

results_pca %>%
  accuracy(reg_tsibblepca) 

##----------------------------------------------------------------------------------##
## Random Forest

rf_mod <- randomForest(y = y_nw[-1], x = cbind("Lag"=lag(y_nw), x_reg)[-1,],
                       mtry = 30)

rf_mod


##---------------------------------------------------------------------------------##
## SVM

## Support Vector machine
set.seed(1119)
mod_svm <- svm(y = y_nw[-c(1, holdout)], 
               cbind(lag(y_nw)[-c(1, holdout)], x_regtrain[-1,]), 
               kernel = "radial", cross = 5, gamma = 0.5)

summary(mod_svm)

## Results are very bad maybe retune?

set.seed(1119)

svm_tune <- tune("svm", train.x =  cbind(lag(y_nw)[-c(1, holdout)],
                                         x_regtrain[-1,]),
                 train.y = y_nw[-c(1, holdout)],
                 type = "eps", kernel = "radial",
                 validation.x =  cbind(lag(y_nw)[holdout],
                                       x_regtest),
                 validation.y = y_nw[holdout],
                 ranges =  list(gamma = seq(0.1, 2, length.out = 20), cost = 2^(2:4)),
                 tunecontrol = tune.control(sampling = "fix"))

svm_tune

## Looks promising

