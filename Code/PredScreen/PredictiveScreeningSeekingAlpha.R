library(bannerCommenter)
library(tidyverse)
library(lubridate)
library(quanteda)
setwd("C:/Users/dordo/Dropbox/Capstone Project")
################################################################################
##                                 Objective:                                 ##
##  Carries out predictive Screening on the Seeking Alpha data using          ##
##                                of Ke (2019)                                ##
################################################################################

##---------------------------------------------------------##
# Get Data

## Read returns data
data_ret <- read_csv("Data/S&P/LogReturnData.csv") %>% 
  select(1:2) %>% rename(Ret=`^OEX`) %>%
  mutate(Dir=ifelse(sign(Ret) == 0, -1, sign(Ret))) %>%
  filter(!is.na(Ret)) %>% 
  #filter(year(Date)<2019 | year(Date)>2020) %>%
  arrange(Date)

## Read Corpus data
data <- read_csv("Data/SeekingAlphaData/SeekingAlphaUSMarkets.csv") %>% 
  arrange(Date) %>%
  mutate(txtid = row_number())

## IMPORTANT: Be careful with test set leakage 
set.seed(10)
data_aux <- data %>%
  mutate(Date = round_date(Date, unit = "day")) %>%
  group_by(y=year(Date), w=week(Date)) %>%
  group_modify(~slice_sample(.x, n=floor(nrow(.x)*0.1))) %>%
  ungroup() %>%
  select(-y,-w) %>%
  arrange(Date)
#data_aux <- data %>% filter(year(Date)>2020 | year(Date)<2019)  

## Join: 
data_aux <- data_aux %>% select(txtid, Text, Date) %>% mutate(Date=as_date(Date))

## To exclude
to_exclude <- data_aux %>% select(txtid) 
write_csv(to_exclude, "Data/SeekingAlphaExcludeID.csv")

## Auxiliary function to generate df
create_df <- function(x){
  inner_join(data_aux %>% mutate(Date=Date + days(x)),
             data_ret %>% select(Date, Dir))
}

## Data for contemporary + lag
all_data <- map(0:1, create_df)

##--------------------------------------------------------##
# Data Proc Pipe

## Remove url
## Remove numbers
## Remove punctuation (check whether + or - remain)
## Removing very short words?
## Trim by term frequency (document frequency?)
## Removing stopword (careful with up and down)

get_seeds <- function(x, th){
  ## Generate corpus
  corp <- corpus(x, text_field = "Text")
  
  ## Create Tokens
  toks <- corp %>% tokens(remove_punct = T, remove_numbers = T, remove_url = T) %>%
    tokens_remove(setdiff(stopwords("en"), c("up","down")), padding = T) %>%
    tokens_ngrams(1)
  
  ## Remove numbers  hashtags and mentions
  toks_oth <- toks %>% 
    tokens_remove("[0-9]+", valuetype ="regex")
  rm(toks)
  
  # Auxiliary functions to create each dataset
  ## Create dfm for each dataset
  dfm_generator <- function(x, stp, ...){
    dfm_hts <- dfm(x, remove = stp, ...) 
    dfm_trim(dfm_hts, min_termfreq = 15) ## Change threshold if needed: It means token has 
    ## to appear 15 times in the corpus 
  }
  
  ## Get relative frequencies Up vs Down for each word in the dfm
  dfm_proc <- function(x){
    x %>% convert("data.frame") %>% 
      pivot_longer(-doc_id) %>%
      group_by(name) %>% 
      arrange(name, doc_id)  %>% 
      mutate(freq = sum(value)) %>% 
      summarise(K = list(binom::binom.wilson(value, freq)[c("mean", "lower", "upper")])) %>%
      unnest(c(K)) %>% 
      mutate("doc_id"=ifelse(row_number() %% 2, "-1", "1"))
  }

  ## Create dfm (Removing stopwords)
  dfm_corp <- dfm_generator(toks_oth,  NULL, group = "Dir") %>% dfm_proc()
  
  ##-------------------------------------------------##
  ## Filter seed words
  get_seed <- function(x, th){
    f <- function(x, y){((x$mean[1] < x$lower[2]) & (x$mean[2] > x$upper[1])) | 
        ((x$mean[1] > x$upper[2]) & (x$mean[2] < x$lower[1]))}
    use <- x %>% group_by(name) %>% group_map(f) %>% unlist()
    x %>% bind_cols("Use"=rep(use,each=2)) %>% 
      filter(Use) %>% 
      filter(doc_id == "1") %>% 
      filter(mean < th | mean > (1-th)) %>% 
      mutate("Pol"=ifelse(mean > 0.5, "Positive", "Negative"))
  }
  
  ## WE COULD CONSIDER Ngrams
  seed_list <- dfm_corp %>% get_seed(th = th)
  seed_list %>% arrange(mean)
}


##-------------------------##
## Use the function on all data sources
res <- map(all_data, get_seeds, th = 0.45)
