library(tidyverse)
library(lubridate)
setwd("C:/Users/dordo/Dropbox/Capstone Project")


# Reading Sentiment  ------------------------------------------------------------

## Twitter
data_tw <- read_csv("Results/Sentiment/Supervised/TwitterSentimentLong.csv") %>% 
  group_by(Type, Method) %>% 
  mutate(Score = as.numeric(scale(Score))) 

## Seeking Alpha
data_sa <- read_csv("Results/Sentiment/Supervised/SASentimentLong.csv") %>% 
  group_by(Type, Method) %>% 
  mutate(Score = as.numeric(scale(Score))) 


# Reading Financial Data --------------------------------------------------

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
  filter(between(year(Date), 2019, 2020)) %>% 
  mutate(across(c(Ret, Vol), ~as.numeric(scale(.x)))) %>% 
  mutate(ehat = c(NA, residuals(lm(Vol~lag(Vol)))))

# Join -------------------------------------------------------------------
# oex_f <- all_oex %>% select(Date, Dir) %>%
#   mutate(Dir = ifelse(as.numeric(Dir) == 2, 1, 0))
oex_f <- all_oex %>% select(Date, ehat)

data_plot <- inner_join(oex_f, data_sa)

aux <- data_plot %>% group_by(Method, Type) %>% 
  nest() %>% 
  mutate(a = map(data, ~ccf(.x[[2]], .x[[3]], na.action = na.omit)))

walk(aux$a, plot)

# Plots -------------------------------------------------------------------

## Line
data_plot %>% filter(Method == "SentiProp") %>% 
  ggplot(aes(x=Date, y=Score)) + geom_line() + geom_smooth() + 
  geom_line(aes(x=Date, y = Ret), color = "red") + facet_wrap(~Type, scale = "free")

## Scatter
data_plot %>% filter(Method == "SemAxis") %>% 
  ggplot(aes(x=Ret, y=Score)) + geom_point() + geom_smooth(method = "lm") +  
  facet_wrap(~Type, scale = "free")

