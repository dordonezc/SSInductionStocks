## This script implements the financial evaluation of the 
## results obtained
setwd("C:/Users/USUARIO/Dropbox/Capstone Project")
library(tidyverse)
library(lubridate)

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

## Generate baseline index (1 as level)
all_oex <- inner_join(data_oex, data_oex_var) %>%
  filter(!is.na(Vol)) %>% 
  filter(between(year(Date), 2019, 2020)) %>% 
  mutate(BaseIndex = cumprod(Ret + 1) * 1)

## Holdout
set.seed(910)
holdout <- sample(1:nrow(all_oex), size = 0.2 * nrow(all_oex))

rm(data_oex, data_oex_var)

# Reading Preds -----------------------------------------------------------

## List files and names
fls <- list.files("Results/Direction/tra/", full.names = T)
nms <- map_chr(fls, ~.x %>% 
          str_extract("/[A-Za-z0-9]+\\.") %>% 
          str_remove_all("[:punct:]"))

## Read preds
preds <- map(fls, read_csv)

## Change preds for gupta
preds[[4]] <- preds[[4]] %>% 
  mutate(name = as_factor(name)) %>% 
  mutate(name = fct_recode(name, pred_svm = "1")) %>% 
  select(Actual, holdout_id, name, value) %>% 
  group_by(holdout_id) %>% 
  summarise(value = sign(sum(value)), 
            name = name[1], 
            Actual = Actual[1])

## Consolidate preds
all_preds <- 
  map2(preds, nms, ~.x %>% mutate(model = .y) %>% filter(name != "pred_glm")) %>% 
  reduce(full_join)

## nest by model and method
preds_nest <- all_preds %>%  
  mutate(id = holdout[holdout_id]) %>%  
  select(-Actual, -holdout_id) %>% 
  group_by(model, name) %>% 
  nest()

rm(all_preds, preds)

# Simple Trade ------------------------------------------------------------

## Gains dataframe
gains_data <- preds_nest %>%  
  mutate(data = map(data, ~.x %>% 
  mutate(Last = all_oex$BaseIndex[id - 1],
         Current = all_oex$BaseIndex[id],
         PropGains = Current - Last,
         Gains = PropGains * value) %>% 
    arrange(id)))

## Calculate gains of strategy
final_gains <- gains_data %>% 
  mutate(Gains = map(data, ~.x %>% summarise(Gains = sum(Gains)))) %>% 
  select(-data) %>%  
  unnest() %>% 
  arrange(desc(Gains))

# Reading Volatility Predictions ------------------------------------------

fls <- list.files("Results/Volatility/tra/", full.names = T)
preds_vol <- map(fls, read_csv)

## Consolidate preds
all_preds_vol <- 
  map2(preds_vol, nms[-4], ~.x %>% mutate(model = .y) %>% 
         filter(name != "pred_glm")) %>% 
  reduce(full_join)

## Nest by method
preds_nest_vol <- all_preds_vol %>%  
  mutate(id = holdout[holdout_id]) %>%  
  select(-Actual, -holdout_id) %>% 
  group_by(model) %>% 
  nest()

## Average by model
preds_vol_agg <- preds_nest_vol %>% 
  mutate(data = map(data, ~.x %>% group_by(id) %>% 
                      summarise(vol=mean(value)))) %>% 
  rename(datavol = data)

rm(preds_vol, all_preds_vol)

## Join with direction prediction
preds_nest_full <- preds_nest %>% left_join(preds_vol_agg) %>% 
  filter(model != "Gupta") %>% 
  mutate(all_data = map2(data, datavol, left_join, by ="id")) %>% 
  select(-data, -datavol)


# Trade Strategy with Vol -------------------------------------------------

## Function to get prob weights using exp
get_prob_vol <- function(x, par){
  
  ## Function to normalize series
  normalize <- function(x) {
    (x- min(x)) /(max(x)-min(x))
  }
  
  ## Simple equation to convert volatilities to probability weights
  x_scaled <- normalize(x)
  res <- 1 - exp(par * (x_scaled - 1))
  normalize(res)
}

## Function to optimize trade strategy over
f_opt <- function(par, opt = T, seed){
  set.seed(seed)
  gains_data_vol <- preds_nest_full %>% 
    mutate(all_data = map(all_data, ~ .x %>% 
                            arrange(id) %>%
                            mutate(Last = all_oex$BaseIndex[id - 1],
                                   Current = all_oex$BaseIndex[id],
                                   PropGains = Current - Last,
                                   probs = get_prob_vol(vol, par),
                                   value_vol = ifelse(runif(n()) < probs, value , 0),
                                   Gains = PropGains * value_vol)))
  
  ## Calculate final gains
  final_gains <- gains_data_vol %>% 
    mutate(Gains = map(all_data, ~.x %>% summarise(Gains = sum(Gains)))) %>% 
    select(-all_data) %>%  
    unnest() %>% 
    arrange(desc(Gains))
  if(opt){
    
    ## For optimization the end value is argmax
    end_val <- final_gains %>% 
      ungroup() %>% 
      summarise(MG = max(Gains)) %>% 
      #summarise(MG = mean(Gains)) %>% 
      pull()
    
    -end_val
  } else {
    list(gains_data_vol, final_gains)
  }
}


## Optimization (with max average high)
opt_proc <- optimize(f_opt, c(0.01, 7), seed = 1070)
opt_proc

## Final results
#975
(opt_res <- f_opt(opt_proc$minimum, opt = F, seed = 340))

## Comparison
(comp <- opt_res[[2]] %>% ungroup() %>% 
    rename(GainsV=Gains) %>% 
  left_join(final_gains) %>% 
  arrange(desc(Gains)) %>% 
  mutate(Diff = GainsV - Gains))

comp %>% summarise(sum(Diff))


# Table To Latex ----------------------------------------------------------

m_1 <- comp %>% select(name, model, Gains) %>% 
  pivot_wider(names_from = name, values_from = Gains) %>% 
  arrange(model)

m_2 <- comp %>% select(name, model, GainsV) %>% 
  pivot_wider(names_from = name, values_from = GainsV) %>% 
  arrange(model)

m_3 <- comp %>% select(name, model, Diff) %>% 
  pivot_wider(names_from = name, values_from = Diff) %>% 
  arrange(model)

m <- matrix(as.matrix(rbind(m_1, m_2, m_3)[,-1]), nrow = 5)
m

xtable(m[,7:12])


# Figures -----------------------------------------------------------------

data_plot_1 <- comp %>% 
  group_by(name) %>% 
  summarise(across(GainsV:Gains, mean)) %>% 
  pivot_longer(cols = GainsV:Gains, names_to = "Strategy", values_to = "Gains") %>%
  mutate(Strategy = fct_recode(Strategy, "pi" = "Gains", "pi-s" = "GainsV")) %>% 
  mutate(Model = case_when(
    name == "pred_ada" ~ "Ada",
    name == "pred_glmaic" ~ "GLM + AIC",
    name == "pred_rf" ~ "RF", 
    T ~ "SVM"
  )) %>% 
  select(-name) %>% 
  bind_cols(Type = "Model")
  
data_plot_2 <- comp %>% 
  group_by(model) %>% 
  summarise(across(GainsV:Gains, mean)) %>% 
  pivot_longer(cols = GainsV:Gains, names_to = "Strategy", values_to = "Gains") %>%
  mutate(Strategy = fct_recode(Strategy, "pi" = "Gains", "pi-s" = "GainsV")) %>% 
  mutate(Method = case_when(
    str_detect(model, "SemAxis") ~ "SemAxis",
    str_detect(model, "SentiProp") ~ "SentiProp",
    str_detect(model, "NoRegs") ~ "No Regs",
    str_detect(model, "Doc") ~ "Doc2Vec",
    T ~ "McDonald"
  )) %>% 
  select(-model) %>% 
  rename(Model = Method) %>% 
  bind_cols(Type = "Method")
  
data_plot_1 %>% full_join(data_plot_2) %>% 
ggplot(aes(x=Model, y = Gains, fill = Strategy)) + 
  geom_col(position="dodge") + 
  scale_fill_manual(values = c("#04A0F8", "#020660"),
                     labels = list(bquote(pi),bquote(pi-sigma))) + 
  theme_classic() + 
  xlab("") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  facet_wrap(~Type, scales = "free")


# Comparing Series --------------------------------------------------------

gains_data_full <- opt_res[[1]] %>% left_join(gains_data)

subs <- gains_data_full %>% 
  filter(name == "pred_ada" & str_detect(model, "NoRegs") | 
         name == "pred_ada" & str_detect(model, "SentiP") | 
         name == "pred_svm" & str_detect(model, "SemAxis") | 
         name == "pred_rf" & str_detect(model, "Doc")  )
  #filter(name == "pred_ada", str_detect(model, "NoRegs"))
  #filter(name == "pred_ada", str_detect(model, "SentiP"))
  #filter(name == "pred_svm", str_detect(model, "SemAxis"))
  
data_plot <- subs %>% 
  mutate(new_data = map2(all_data, data, ~.x %>% 
                           select(id, GainsV = Gains, probs) %>% 
                           left_join(.y %>%  select(id, Gains), by = "id") %>% 
                           mutate(across(starts_with("G"), cumsum)))) %>% 
  select(name, model, new_data) %>% 
  unnest() %>% 
  mutate(name = str_remove(str_to_title(str_extract(name, "_[a-z]+")), "_"),
         model = str_match(model, "NoRegs|SemAxis|SentiProp|Doc2Vec")) %>% 
  ungroup() %>% 
  unite(name, c(name, model), sep = " | ")

date_labs <- format(ymd(all_oex$Date), '%m-%y')[unique(data_plot$id)]

## Plot strategies
data_plot %>% select(-probs) %>% 
  pivot_longer(cols = GainsV:Gains, names_to = "Strategy", values_to = "Gains") %>%
  group_by(name) %>% 
  mutate(Strategy = fct_recode(Strategy, "pi" = "Gains", "pi-s" = "GainsV"),
         Date = seq_along(id)) %>% 
  ungroup() %>% 
  ggplot(aes(x = Date, y = Gains, color = Strategy)) + geom_line()  +
  theme_classic() +
  xlab("Date") +
  scale_x_continuous(label = date_labs[seq(1, length(date_labs), length.out = 5)]) +
  scale_color_manual(values = c("#04A0F8", "#020660"),
                     labels = list(bquote(pi),bquote(pi-sigma))) +
  facet_wrap(~name, scales = "free")

## Plot Probabilities
data_plot %>% select(-starts_with("G")) %>% 
  group_by(name) %>% 
  mutate(Date = seq_along(id)) %>% 
  ungroup() %>% 
  ggplot(aes(x = Date, y = probs, color = name)) + geom_line()  +
  theme_classic() +
  xlab("Date") +
  scale_x_continuous(label = date_labs[seq(1, length(date_labs), length.out = 5)]) #+
  scale_color_manual(values = c("#04A0F8", "#020660"),
                     labels = list(bquote(pi),bquote(pi-sigma))) 

