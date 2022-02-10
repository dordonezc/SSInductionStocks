## 
library(tidyverse)
library(quanteda)
library(lubridate)
setwd("C:/Users/dordo/Dropbox/Capstone Project")

#######################################################################################################
##                                             Objective                                             ##
##  Basic Exploratory Data analysis based on the Twitter Dataset, create McDonald sentiment series   ##
#######################################################################################################

## Load Data
data <- read_csv("Data/Twitter/TwitterConsolidated.csv") %>% 
  filter(between(year(date), 2019, 2020)) %>% 
  mutate(date = round_date(date, unit = "day"))

## Generate corpus
corp <- corpus(data, text_field = "tweet")

## Create Tokens
toks <- corp %>% tokens(remove_punct = T, remove_numbers = T, remove_url = T)

## Separate hashtags and mentions
toks_hts <- toks %>% tokens_select(pattern="#.+", valuetype="regex")
toks_mts <- toks %>% tokens_select(pattern = "@.+", valuetype = "regex")

## Remove numbers 
toks_oth <- toks %>% 
  tokens_remove(pattern="#.+", valuetype="regex") %>%
  tokens_remove(pattern="@.+", valuetype = "regex") %>% 
  tokens_remove("[0-9]+", valuetype ="regex")

## Create dfm for hastags
dfm_generator <- function(x, stp, group = NULL){
  dfm_hts <- dfm(x, remove = stp, group = group)
  dfm_trim(dfm_hts, min_termfreq = 20, min_docfreq = 20) ## Change threshold
}

## Hashtags
hts <- c("#index", "#usmarkets", "#stockmarket", "s&p100", "#stockmarkets")
dfm_hts <- dfm_generator(toks_hts, hts, group = factor(docvars(toks_hts)$date))

## Mentions
dfm_mts <- dfm_generator(toks_mts, NULL, group = factor(docvars(toks_hts)$date))

## Create dfm for non hashtags
dfm_corp <- dfm_generator(toks_oth, stopwords("en"), group = factor(docvars(toks_hts)$date))

## McDonald Dictionary
dfm_dict <- dfm_lookup(dfm_corp, 
                       dictionary = quanteda.dictionaries::data_dictionary_LoughranMcDonald)

Mcd <- dfm_dict %>% 
  dfm_select(c("POSITIVE","NEGATIVE")) %>% 
  dfm_weight(scheme="prop")

## McDonald score
Mcd_series <- convert(Mcd, to ="data.frame") %>%
  as_tibble() %>%
  mutate(Score = POSITIVE - NEGATIVE) %>%
  select(doc_id, Score)

Mcd_daily <- bind_cols(Mcd_series,"Date"=docvars(Mcd)$date) %>% select(-doc_id) %>%
  arrange(Date) %>% mutate(Date=round_date(Date, unit = "day")) %>%
  group_by(Date) %>%
  summarise(Score=mean(Score))

## Output Sentiment Series
write_csv(Mcd_daily, "Results/Sentiment/Sent_Twitter.csv")
