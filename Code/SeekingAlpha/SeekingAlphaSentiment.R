library(tidyverse)
library(quanteda)
library(lubridate)
library(quanteda.dictionaries)

#######################################################################################################
##                                             Objective                                             ##
##  Basic Exploratory Data analysis based on the SA Dataset, create McDonald sentiment series        ##
#######################################################################################################

setwd("C:/Users/dordo/Dropbox/Capstone Project")

## News to drop
omit <- read_csv("Data/SeekingAlphaExcludeID.csv") %>% pull()

## Load Data
data <- read_csv("Data/SeekingAlphaData/SeekingAlphaUSMarkets.csv") %>% 
  arrange(Date) %>%
  mutate(Date = round_date(Date, unit = "day")) %>% 
  slice(-omit)

## Generate corpus
corp <- corpus(data, text_field = "Text")

## Create Tokens
toks <- corp %>% tokens(remove_punct = T, remove_numbers = T, remove_url = T)

## Create dfm 
dfm_corp <-  toks %>% dfm(remove = stopwords("en"), group = factor(docvars(toks)$Date)) %>% 
            dfm_trim(min_termfreq = 5, min_docfreq = 5)

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

Mcd_daily <- bind_cols(Mcd_series,"Date"=docvars(Mcd)$Date) %>% select(-doc_id) %>%
  arrange(Date) %>% mutate(Date=round_date(Date, unit = "day")) %>%
  group_by(Date) %>%
  summarise(Score=mean(Score))

## Output Sentiment Series
write_csv(Mcd_daily, "Results/Sentiment/Sent_SeekingAlpha.csv")
