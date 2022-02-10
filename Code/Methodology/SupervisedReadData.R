## Baseline data reading
library(tidyverse)
library(lubridate)

setwd("C:/Users/dordo/Dropbox/Capstone Project")

## This script was written to load the supervised sentiment
## series derived from the inducted Dictionaries and join them with the financial
## data. 

# Read Sentiment Data -----------------------------------------------------

## Sentiment - WSJ  
# snts_wsj <- read_csv("Results/Sentiment/Sent_WSJ.csv") %>%
#   mutate(Date=dmy(Date)) %>%
#   rename(PolarityWSJ = Polarity)

#method <- "SemAxis"
method <- "SentiProp"

## Sentiment - Twitter
snts_twt <- read_csv("Results/Sentiment/Supervised/TwitterSentimentLong.csv") %>% 
  filter(Method == !!method)

## Sentiment - Seeking Alpha
snts_sa <- read_csv("Results/Sentiment/Supervised/SASentimentLong.csv") %>% 
  filter(Method == !!method)

## Sentiment - Wall Street Journal
snts_wsj <- read_csv("Results/Sentiment/Supervised/WSJSentimentLong.csv") %>% 
  filter(Method == !!method)


# Pivot ---------------------------------------------------------------------

## Pivot wider both sources
all_series <- map(list(snts_sa, snts_twt, snts_wsj), ~.x %>% 
  unite("EMB", Embedding, GType, BType) %>% 
  select(-Method) %>% 
  pivot_wider(values_from = "Score", names_from = "EMB")) 

aux_f <- function(x, y){
  colnames(x)[-1] <- str_c(colnames(x)[-1], "-", y)
  x
}

all_series <- map2(all_series, list("SA","TWT","WSJ"), aux_f) %>% 
  reduce(inner_join, by ="Date")

## Quick ACF for EDA
snts_acf <- all_series %>% summarise(across(!Date, ~list("ACF"=acf(.x),
                                                       "PACF"=pacf(.x))))
## Check
snts_acf[[3]]

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

rm(data_oex, data_oex_var)

# Joining Datasets --------------------------------------------------------

## Join Sentiments + Financial
all_data <- left_join(all_oex, all_series) %>% 
  filter(between(year(Date), 2019, 2020)) %>% 
  fill(-(Date:Vol))

## Pre-whitening
all_data <- all_data %>%
  mutate(Ehat = c(NA,residuals(lm(Vol~lag(Vol)))))


## Split Training Test -----------------------------------------------------

## Split in train - test --- Same as the Doc2Vec script
set.seed(910)
holdout <- sample(1:nrow(all_data), size = 0.2 * nrow(all_data))
holdout <-  all_data %>% select(Date) %>% 
  mutate(Id = month(Date) > 8 & year(Date) == 2020) %>% 
  pull(Id) %>%
  which()

rm(list=setdiff(ls(), c("all_data", "holdout", "method")))
