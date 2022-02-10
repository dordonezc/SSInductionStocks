## Doc2Vec news Freshness
library(tidyverse)
library(lubridate)
library(caret)
source("Code/RHKS_clasico.R")
setwd("C:/Users/dordo/Dropbox/Capstone Project")


# Read Freshness Index ----------------------------------------------------
fls <- dir("Results/Freshness", full.names = T)
data <- map(fls, read_csv) %>% reduce(inner_join, suffix = c("SA", "TW"), by = "Date")

# Read Financial Data -----------------------------------------------------

## Returns
data_oex <- read_csv("Data/S&P/LogReturnData.csv") %>% 
  select(1:2) %>%
  rename(Ret=`^OEX`) %>%
  mutate(Dir=sign(Ret),
         Dir = as.factor(ifelse(Dir == 0, -1, Dir))) 

## Volatility
data_oex_var <- read_csv("Data/S&P/VarData.csv") %>% 
  select(1:2) %>% 
  rename(Vol=`^OEX`)

all_oex <- inner_join(data_oex, data_oex_var) %>%
  filter(!is.na(Vol)) %>%
  mutate(Ehat = c(NA,residuals(lm(Vol~lag(Vol)))))

rm(data_oex, data_oex_var)


# Join Data ---------------------------------------------------------------

res <- inner_join(all_oex, data)
res

## Winsorization
# winsor <- function (x, fraction=.05){
#   lim <- quantile(x, probs=c(fraction, 1-fraction))
#   x[x < lim[1]] <- lim[1]
#   x[x > lim[2]] <- lim[2]
#   x
# }


# EDA ---------------------------------------------------------------------

aux <- res %>% select(-c(Dir, Ehat, Ret)) %>% 
  mutate(across(-Date, ~as.numeric(scale(.x)))) %>% 
  mutate(across(-Date, Get_trend, type = "Henderson", m = 6))

colors <- c("Volatility" = "black", "Freshness" = "red")

aux %>% 
  #select(Date, Vol, CosSimSA, CosSimTW) %>% 
  pivot_longer(-c(Date, Vol)) %>% 
  mutate(Type = ifelse(str_detect(name, "V"), "Variance", "Average"),
         name = ifelse(str_detect(name, "SA"), "Seeking Alpha", "Twitter")) %>% 
  ggplot(aes(x = Date)) + geom_line(aes(y=Vol, color = "Volatility")) + 
  geom_line(aes(y=value,  color = "Freshness")) +
  facet_wrap(~name + Type) + theme_classic() + scale_x_datetime(date_labels = "%m/%y") + 
  ggtitle("") + ylab("Scaled Value") +
  scale_color_manual(values = colors) + theme(legend.position="bottom")

res %>% 
  mutate(Dir = fct_recode(Dir, Up="1", Down = "-1")) %>% 
  rename(Direction = Dir) %>% 
  #select(-c(Ret, Ehat, CosSimSA, CosSimTW)) %>% 
  pivot_longer(-c(Date:Ehat)) %>% 
  mutate(Type = ifelse(str_detect(name, "V"), "Variance", "Average"),
         name = ifelse(str_detect(name, "SA"), "Seeking Alpha", "Twitter")) %>% 
  ggplot(aes(x = value, y = Vol)) + geom_point(aes(color = Direction)) + 
  facet_wrap(~name + Type, scales = "free") + 
  scale_color_manual(values=c("#FF0000", "#00FF00")) + 
  geom_smooth(method = "lm")  + 
  ggtitle("") + xlab("Scaled Value") +
  ylab("Volatility")  + theme_classic() + theme(legend.position="bottom") 

res %>% 
  mutate(Dir = fct_recode(Dir, Up="1", Down = "-1")) %>% 
  select(-c(Ret, Vol, Ehat)) %>% 
  rename(Direction = Dir) %>% 
  #select(-c(Ret, Ehat, CosSimSA, CosSimTW)) %>% 
  pivot_longer(-c(Date, Direction)) %>% 
  mutate(Type = ifelse(str_detect(name, "V"), "Variance", "Average"),
         name = ifelse(str_detect(name, "SA"), "Seeking Alpha", "Twitter")) %>%  
  ggplot(aes(y=value, x = Direction, fill = Direction)) + geom_violin() + 
  facet_wrap(~Type + name, scales = "free") + theme_classic() + 
  scale_fill_manual(values=c("#FF0000", "#00FF00")) + 
  theme(legend.position="bottom") + 
  ylab("Cosine") + ggtitle("")
  

## Plots
data_plot <- data %>% pivot_longer(-Date) %>%
  mutate(Source = str_detect(name, "SA"),
         Measure = str_detect(name, "V")) %>%
  mutate(Source = ifelse(Source, "SA", "TW"),
         Measure = ifelse(Measure, "Variance", "Average")) %>%
  select(-name)
# 
# data_plot %>% 
#   ggplot(aes(x = Date, y = value, color = Source)) + geom_line() + facet_wrap(~Measure)
# 
# data_plot %>% pivot_wider(names_from = Source, values_from = value) %>% 
#   ggplot(aes(x=SA, y=TW)) + geom_point() + facet_wrap(~Measure)


# Modelling ---------------------------------------------------------------

## Split in train - test --- Same as the Doc2Vec script
set.seed(910)
holdout <- sample(1:nrow(res), size = 0.2 * nrow(res))

res_ccf <- data_plot %>% 
  inner_join(all_oex %>% select(Date, Ehat)) %>% 
  group_by(Source, Measure) %>% 
  nest() %>% 
  mutate(ccf = map(data, ~ccf(.x[[3]],.x[[2]])))

## Seven lags for DIR
## Seven lags for Vol

nlag <- 5
reg_data <- res %>%  select(-c(Date, Ret, Dir, Ehat)) %>% 
  mutate(across(.fns = map(1:nlag, ~partial(dplyr::lag, n=.x)),
                .names = "L{.fn}{.col}")) 

# CARET -------------------------------------------------------------------

## Split for caret
set.seed(728) 
train.control <- trainControl(method = "cv", number = 5)

set.seed(752)
test_data <- reg_data %>% slice(holdout)
actual <- test_data %>% pull(Vol)

# Train the model
model <- train(Vol ~., data = reg_data  %>%  slice(-holdout), method = "glmStepAIC",
               trControl = train.control, na.action = na.omit)

model_base <- train(Vol ~ L1Vol , data = reg_data  %>%  slice(-holdout), method = "glm",
                             trControl = train.control, na.action = na.omit)

# model_base <- train(Vol ~ L1Vol + L2Vol, data = reg_data  %>%  slice(-holdout), method = "glm",
#                     trControl = train.control, na.action = na.omit)

pred_glmaic <- predict(model, test_data)
pred_base <- predict(model_base, test_data)


# Consolidate preds --------------------------------------------------------------

dafiles <- ls(pattern="pred")

all_preds <- map_dfc(dafiles %>% setNames(dafiles), get) %>% 
  bind_cols("Actual"=actual) %>% 
  mutate(holdout_id = row_number()) %>% 
  pivot_longer(-c(holdout_id, Actual))

# tab <- all_preds %>% arrange(name, holdout_id) %>%
#   group_by(name) %>% 
#   summarise(RMSE = sqrt(mean((Actual-value)^2)),
#             MAE = sqrt(mean(abs(Actual-value))))

tab <- all_preds %>% arrange(name, holdout_id) %>%
  group_by(name) %>%
  summarise(RMSE = sqrt(mean((Actual-value)^2)),
            MAE = sqrt(mean(abs(Actual-value))))

tab
xtable(tab)

# More Plots --------------------------------------------------------------

ggplot(res, aes(x=lag(CosSimTW, 1), y=Ehat)) + geom_point(aes(color=Dir)) + 
  geom_smooth(method="lm")

ggplot(res, aes(x=lag(CosSimTW, 1), y=Vol)) + geom_point(aes(color=Dir)) + 
  geom_smooth(method="lm")

ggplot(res, aes())