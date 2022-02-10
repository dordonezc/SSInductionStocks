## Baseline data reading
library(tidyverse)
library(lubridate)

setwd("C:/Users/USUARIO/Dropbox/Capstone Project")

## This script was writteng to load the baseline sentiment
## series derived from the L&MCd Dictionary and join them with the financial
## data. 

# Read Sentiment Data -----------------------------------------------------

## Sentiment - WSJ  
snts_wsj <- read_csv("Results/Sentiment/Sent_WSJ.csv") %>%
  mutate(Date=dmy(Date)) %>%
  rename(PolarityWSJ = Polarity)

## Sentiment - Twitter
snts_twt <- read_csv("Results/Sentiment/Sent_Twitter.csv") %>%
  rename(PolarityTwitter = Score)

## Sentiment - Seeking Alpha
snts_sa <- read_csv("Results/Sentiment/Sent_SeekingAlpha.csv") %>%
  rename(PolaritySA = Score) 

## Consolidate sentiments
all_snts <- inner_join(snts_twt, snts_sa) %>% 
  inner_join(snts_wsj)

suppressWarnings(rm(snts_twt, snts_wsj, snts_sa))

## Quick ACF for EDA
snts_acf <- all_snts %>% summarise(across(!Date, ~list("ACF"=acf(.x),
                                                       "PACF"=pacf(.x))))
## Check
snts_acf[[1]]
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
  filter(!is.na(Vol))

## No MISSINGS!
all_oex %>% summarise(across(!Date, ~sum(is.na(.x))))

## Quick ACF
oex_acf <- all_oex %>% 
  summarise(across(!c(Date,Dir), ~list("ACF"=acf(.x), "PACF"=pacf(.x))))

## Check
oex_acf$Vol[[1]] %>% plot()
rm(data_oex, data_oex_var)


# Joining Datasets --------------------------------------------------------

## Join Sentiments + Financial
all_data <- left_join(all_oex, all_snts) %>% 
  filter(between(year(Date), 2019, 2020)) %>% 
  fill(-(Date:Vol))

## Pre-whitening
all_data <- all_data %>%
  mutate(Ehat = c(NA,residuals(lm(Vol~lag(Vol)))))

# Split Training Test -----------------------------------------------------

## Split in train - test --- Same as the Doc2Vec script
set.seed(910)
holdout <- sample(1:nrow(all_data), size = 0.2 * nrow(all_data))
holdout <-  all_data %>% select(Date) %>%
  mutate(Id = month(Date) > 8 & year(Date) == 2020) %>%
  pull(Id) %>%
  which()


