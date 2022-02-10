## Doc2Vec news Freshness
library(tidyverse)
library(lubridate)
library(caret)
source("Code/RHKS_clasico.R")
setwd("C:/Users/USUARIO/Dropbox/Capstone Project")

## Returns
data_oex <- read_csv("Data/S&P/LogReturnData.csv") %>% 
  select(1:2) %>%
  rename(Ret=`^OEX`) %>%
  mutate(Dir=sign(Ret),
         Dir = as.factor(ifelse(Dir == 0, -1, Dir)))

data_oex_var <- read_csv("Data/S&P/VarData.csv") %>% 
  select(1:2) %>% 
  rename(Vol=`^OEX`)

all_oex <- inner_join(data_oex, data_oex_var) %>%
  filter(!is.na(Vol)) %>% 
  filter(year(Date) > 2018, year(Date) < 2021)


# Returns -----------------------------------------------------------------

all_oex %>% select(Date, Ret) %>% 
  mutate(Smoothed_Ret = Get_trend(Ret, "Henderson", 11)) %>% 
  ggplot(aes(x=Date, y = Ret)) + geom_line() + 
  geom_line(aes(y = Smoothed_Ret), color = "red", size = 0.75) +
  theme_classic() +
  ylab("Returns") +
  scale_x_date(date_labels = "%m/%Y")

all_oex %>% select(Date, Ret) %>% 
  mutate(RetP = ifelse(Ret >= 0, Ret, NA),
         RetN = ifelse(Ret < 0, Ret, NA)) %>% 
  ggplot(aes(x=Date, y = Ret)) + geom_line(color = "black") + 
  geom_point(aes(x=Date, y = RetN), color = "red", size = 1) + 
  geom_point(aes(x=Date, y = RetP),color = "darkgreen", size = 1) +
  geom_hline(aes(yintercept=0), linetype = "dashed", color = "blue") + 
  ylab("Returns") +
  theme_classic() + 
  scale_x_date(date_labels = "%m/%Y")

## Seasonal plots
splots <- all_oex %>% select(Date, Dir) %>% 
  mutate(days = as.factor(wday(Date))) %>% 
  group_by(days) %>% 
  nest() %>% 
  mutate(c=map(data, ~.x %>% count(Dir))) %>% 
  select(days, c) %>% 
  unnest() %>% 
  mutate(all_n = sum(n), 
         prop = n/all_n, 
         sd = sqrt(prop*(1-prop)/all_n)) %>% 
  ungroup() %>% 
  mutate(Dir = fct_recode(Dir, Down = "-1", Up = "1"), 
         days = fct_recode(days, Mon="2", Tue="3", Wed = "4", 
                           Thu = "5", Fri ="6"))
splots %>%  
  ggplot(aes(x=days)) + 
  geom_col(aes(y=prop, fill = Dir)) + 
  geom_errorbar(aes(ymin = prop - sd, ymax = prop + sd), color = "black") + 
  facet_wrap(~Dir) + 
  scale_fill_manual(values=c("#FF0000", "#00FF00")) + 
  ylab("Proportion") + 
  ylim(c(0,0.8)) + 
  xlab("Day of week") + 
  theme_classic()

## Seasonal volatility 
all_oex %>% select(Date, Vol) %>% 
  mutate(days = as.factor(wday(Date))) %>% 
  mutate(days = fct_recode(days, Mon="2", Tue="3", Wed = "4", 
                           Thu = "5", Fri ="6")) %>% 
  ggplot(aes(x=days, y=Vol, fill = days)) + geom_boxplot() +
  theme_classic() + ylab("Volatility") + xlab("Day of week")

# Volatility --------------------------------------------------------------
library(gridExtra)
ac <- ggAcf(all_oex$Vol) + theme_classic() + ggtitle("")
pac <- ggPacf(all_oex$Vol) + theme_classic() + ggtitle("")
grid.arrange(ac, pac, ncol=2)

all_oex %>% select(Date, Vol) %>% 
  mutate(Smoothed_Vol = Get_trend(Vol, "Henderson", 11)) %>% 
  ggplot(aes(x=Date, y = Vol)) + geom_line() + 
  geom_line(aes(y = Smoothed_Vol), color = "red", size = 0.75) +
  theme_classic() +
  ylab("Volatility") +
  scale_x_date() + scale_x_date(date_labels = "%m/%Y")

all_oex %>% select(Date, Ret) %>% 
  mutate(Smoothed_Vol = Get_trend(Vol, "Henderson", 11)) %>% 
  ggplot(aes(x=Date, y = Vol)) + geom_line() + 
  geom_line(aes(y = Smoothed_Vol), color = "red", size = 0.75) +
  theme_classic() +
  ylab("Volatility") +
  scale_x_date() + scale_x_date(date_labels = "%m/%Y")
