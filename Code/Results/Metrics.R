## Compute metrics for the classification task 

library(tidyverse)
library(caret)
library(purrr)
library(dplyr)
library(e1071)
setwd("C:/Users/USUARIO/Dropbox/Capstone Project")


## Function to get Metrics
get_metrics <- function(x){
  ## Create Table
  tab <- x %>% 
    select(value, Actual) %>% 
    table() 
  ## Confusion Matrix
  aux <- confusionMatrix(tab, positive = "1")
  metrics <- c(aux$overall["Accuracy"], aux$byClass[c("Precision", "Recall", "F1",
                                                      "Detection Prevalence")])
  list(metrics, tab)
}

pivot_metrics <- function(path){
  ## Read CSV
  data <- read_csv(path) %>% 
    mutate(Actual = as_factor(Actual), value = as_factor(value)) 
  
  # no_preds <- data %>% select(holdout_id, Actual) %>% 
  #   distinct() %>% 
  #   count(Actual)
  # 
  # no_preds$n[2]/sum(no_preds$n)
  
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
  ltab <- c("Accuracy", "Precision", "Recall", "F1", "DP")
  final_tab <- all_res %>% select(name, metrics) %>% 
    unnest(metrics) %>% 
    mutate(Col = row_number(),
           Col = ltab[Col]) %>% 
    ungroup() %>% 
    pivot_wider(values_from = metrics, names_from = Col)
  
  final_tab
}

# Getting metrics for all files -------------------------------------------
fls <- dir("Results/Direction/tra", full.names = T)
res <- map(fls, pivot_metrics)
res

## Fix the results for gupta
ltab <- c("Accuracy", "Precision", "Recall", "F1", "DP")
aux <- res[[4]] %>% 
       colMeans() %>% 
       as.list() %>% 
       tibble() %>% 
       unnest() 
names(aux) <- "Name"

aux_repl <- aux %>% slice(-1) %>% 
  mutate(Col = ltab[row_number()]) %>% 
  pivot_wider(names_from = Col, values_from = Name) %>% 
  mutate(Name = "pred_svm") %>% 
  select(Name, everything()) %>% 
  rename(name = Name)

res[[4]] <- aux_repl

# Plots -------------------------------------------------------------------

res <- map(res, ~.x %>% filter(name != "pred_glm")) %>% 
  keep(~nrow(.x) > 1) %>% 
  set_names(c("McDonald", "NoRegs", "Doc2Vec", "SemAxis", "SentiProp"))


res_plot <- tibble(res) %>% 
  bind_cols(Method=c("McDonald", "NoRegs", "Doc2Vec", "SemAxis", "SentiProp")) %>% 
  unnest()


## Across Methods and models
data_plot_1 <- res_plot %>% group_by(Method) %>% 
  summarise(across(Accuracy:DP, mean)) %>% 
  select(-DP, -F1) %>% 
  mutate(F1 = 2 * Precision * Recall / (Precision + Recall)) %>% 
  pivot_longer(-Method) %>% 
  rename(Metric = name) %>% 
  mutate(Metric = as.factor(Metric),
         Metric = fct_reorder(Metric, value, min)) %>% 
  mutate(Type = "Method")
  
data_plot_2 <- res_plot %>% 
  rename(Model = name) %>%
  group_by(Model) %>% 
  summarise(across(Accuracy:DP, mean)) %>% 
  select(-DP, -F1) %>% 
  mutate(F1 = 2 * Precision * Recall / (Precision + Recall)) %>% 
  pivot_longer(-Model) %>% 
  rename(Metric = name) %>% 
  mutate(Metric = as.factor(Metric),
         Metric = fct_reorder(Metric, value, min),
         Model = case_when(
           Model == "pred_rf" ~ "RF",
           Model == "pred_svm" ~ "SVM",
           Model == "pred_ada" ~ "ADA",
           T ~ "GLM + AIC"
         )) %>% 
  rename(Method = Model) %>% 
  mutate(Type = "Model")

data_plot <- data_plot_1 %>% full_join(data_plot_2)

data_plot %>% 
  ggplot(aes(x=Method, y=value)) + 
  ylab("Score") + 
  geom_col(aes(fill = Metric), position = "dodge") +
  coord_cartesian(ylim=c(0.4,0.9)) + 
  scale_fill_brewer(palette = "Paired") +
  theme_classic()  + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~Type, scales = "free_x") +
  xlab("")


# Rest --------------------------------------------------------------------

# export the accuracy matrix
# list_of_fls = c('BaselineSentiment.csv', 
#                 'BaselineSentimentNoRegs.csv',
#                 'Doc2Vec.csv',
#                 'Gupta.csv',
#                 'SemAxis.csv',
#                 'SentiProp.csv')
# ind = list(list_of_fls, res)
# for (i in 1:6){
#   name = ind[[1]][i]
#   result = ind[[2]][i]
#   write.csv(result, name)
# }
# 
# # visualize the results, use F1 
# names(res[[4]])[1] = 'name' # rename the column of Gupta
# methods = c('Baseline', 'BaselineNoRegs', 'Doc2Vec', 'Gupta', 'SemAxis', 'SentiProp')
# 
# names = rep(0,6)
# values = rep(0,6)
# 
# for (i in 1:6){
# values[i] = max(res[[i]]$F1)
# na = res[[i]]$name[which.max(res[[i]]$F1)]
# names[i] = paste(methods[i], na, sep = '\n')
# }
# 
# barplot(values, names.arg = names, cex.names=0.7, ylab = 'F1', ylim = c(0.6, 0.8))


