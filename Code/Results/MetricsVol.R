## Volatility metrics 

library(tidyverse)
library(caret)
setwd("C:/Users/USUARIO/Dropbox/Capstone Project")

## Function to get Metrics
get_metrics <- function(x){
  ## Create Table
  tab <- x %>% 
    select(value, Actual) 
  ## Metrics  
  metrics <- with(x, postResample(value, Actual))
  list(metrics, tab)
}

pivot_metrics <- function(path){
  ## Read CSV
  data <- read_csv(path) 
  
  ## Nest by model name
  data_nest <- data %>% 
    arrange(name, holdout_id) %>% 
    group_by(name) %>% 
    nest()
  
  ## Get metrics and tables
  all_res <- data_nest %>% 
    mutate(res = map(data, get_metrics),
           metrics = map(res, 1))
  
  ## Pivot to wide format
  ltab <- c("RMSE", "RSquared", "MAE")
  final_tab <- all_res %>% select(name, metrics) %>% 
    unnest(metrics) %>% 
    mutate(Col = row_number(),
           Col = ltab[Col]) %>% 
    ungroup() %>% 
    pivot_wider(values_from = metrics, names_from = Col)
  
  final_tab
}

# Getting metrics for all files -------------------------------------------
fls <- dir("Results/Volatility/tra", full.names = T)
res <- map(fls, pivot_metrics)
res



# Plot --------------------------------------------------------------------


res <- map(res, ~.x %>% filter(name != "pred_glm")) %>% 
  keep(~nrow(.x) > 1) %>% 
  set_names(c("McDonald", "NoRegs", "Doc2Vec", "SemAxis", "SentiProp"))


res_plot <- tibble(res) %>% 
  bind_cols(Method=c("McDonald", "NoRegs", "Doc2Vec", "SemAxis", "SentiProp")) %>% 
  unnest()


## Across Methods and models
data_plot_1 <- res_plot %>% group_by(Method) %>% 
  summarise(across(RMSE:MAE, mean)) %>% 
  pivot_longer(-Method) %>% 
  rename(Metric = name) %>% 
  mutate(Metric = as.factor(Metric),
         Metric = fct_reorder(Metric, value, min)) %>% 
  mutate(Type = "Method")

data_plot_2 <- res_plot %>% 
  rename(Model = name) %>%
  group_by(Model) %>% 
  summarise(across(RMSE:MAE, mean)) %>% 
  pivot_longer(-Model) %>% 
  rename(Metric = name) %>% 
  mutate(Metric = as.factor(Metric),
         Metric = fct_reorder(Metric, value, min),
         Model = case_when(
           Model == "pred_rf" ~ "RF",
           Model == "pred_svm" ~ "SVM",
           Model == "pred_mars" ~ "MARS",
           T ~ "GLM + AIC"
         )) %>% 
  rename(Method = Model) %>% 
  mutate(Type = "Model")

data_plot <- data_plot_1 %>% full_join(data_plot_2)

data_plot %>% 
  filter(Metric != "RSquared") %>% 
  ggplot(aes(x=Method, y=value)) + 
  ylab("Score") + 
  coord_cartesian(ylim=c(0.5,1.3)) + 
  geom_col(aes(fill = Metric), position = "dodge") +
  scale_fill_brewer(palette = "Paired") +
  theme_classic()  + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~Type, scales = "free_x") +
  xlab("")


# Rest --------------------------------------------------------------------

# export the result to csv files
# list_of_fls = c('BaselineSentiment.csv', 
#                 'BaselineSentimentNoRegs.csv',
#                 'Doc2Vec.csv',
#                 'SemAxis.csv',
#                 'SentiProp.csv')
# ind = list(list_of_fls, res)
# for (i in 1:5){
#   name = ind[[1]][i]
#   result = ind[[2]][i]
#   write.csv(result, name)
# }
# 
# # visualize the results, use RMSE 
# 
# methods = c('Baseline', 'BaselineNoRegs', 'Doc2Vec', 'SemAxis', 'SentiProp')
# 
# names = rep(0,5)
# values = rep(0,5)
# 
# for (i in 1:5){
#   values[i] = min(res[[i]]$RMSE  )
#   na = res[[i]]$name[which.min(res[[i]]$RMSE)]
#   names[i] = paste(methods[i], na, sep = '\n')
# }
# 
# barplot(values, names.arg = names, cex.names=1, ylab = 'RMSE')
# 
