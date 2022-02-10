library(bannerCommenter)
library(tidyverse)
library(lubridate)
library(quanteda)
setwd("C:/Users/dordo/Dropbox/Capstone Project")

################################################################################
##                                 Objective:                                 ##
##  Carries out predictive Screening on the twitter data using the proposal   ##
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
data <- read_csv("Data/Twitter/TwitterConsolidated.csv") %>% 
  rename(Date=date) %>%
  arrange(Date) %>%
  mutate(twid = row_number())

## IMPORTANT: Be careful with test set leakage 
set.seed(10)
data_aux <- data %>%
  mutate(Date = round_date(Date, unit = "day")) %>%
  filter(year(Date)>2018, year(Date)<2021) %>% 
  group_by(Date) %>%
  group_modify(~slice_sample(.x, n=floor(nrow(.x)*0.1)))

## To exclude
to_exclude <- data_aux %>% ungroup() %>% select(twid) 
write_csv(to_exclude, "Data/TwitterExcludeID.csv")

## Join: 
data_aux <- data_aux %>% select(twid, tweet, Date) %>% mutate(Date=as_date(Date))

## Auxiliary function to generate df
create_df <- function(x){
  inner_join(data_aux %>% mutate(Date=Date + days(x)),
             data_ret %>% select(Date, Dir))
}

## Data for contemporary + lag
all_data <- map(0:1, create_df)

## Check predictive screening for lag-0 and lag-1
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
  corp <- corpus(x, text_field = "tweet")
  
  ## Create Tokens
  exc <- c("above", "below", "up", "down", "on", "off", "over", "under", "few", "more", "most")
  toks <- corp %>% tokens(remove_punct = T, remove_numbers = T, remove_url = T) %>%
    tokens_remove(setdiff(stopwords("en"), exc), padding = T) %>%
    tokens_ngrams(1)
  
  ##----------------------------------------------------## 
  ## Separate hashtags and mentions (TWITTER SPECIFIC)
  toks_hts <- toks %>% tokens_select(pattern="#.+", valuetype="regex")
  toks_mts <- toks %>% tokens_select(pattern = "@.+", valuetype = "regex")
  
  ## Remove numbers  hashtags and mentions
  toks_oth <- toks %>% tokens_remove(pattern="#.+", valuetype="regex") %>%
    tokens_remove(pattern="@.+", valuetype = "regex") %>% 
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
      mutate("doc_id"=ifelse(seq_along(name) %% 2, "-1", "1"))
  }
  
  ## Hashtags
  hts <- c("#index", "#usmarkets", "#stockmarket", "s&p100", "#stockmarkets")
  dfm_hts <- dfm_generator(toks_hts, hts, group = "Dir") %>% dfm_proc()
  
  ## Mentions
  dfm_mts <- dfm_generator(toks_mts, NULL, group = "Dir") %>% dfm_proc()
  
  ## Create dfm for non hashtags (Removing stopwords)
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
  dfms <- list(dfm_corp, dfm_hts, dfm_mts)
  seed_list <- dfms %>% map(get_seed, th = th) %>% 
    reduce(bind_rows)
  list(seed_list %>% arrange(mean), list(toks_oth, toks_hts, toks_mts))
}


##-------------------------##
## Use the function on all data sources
res <- map(all_data, get_seeds, th = 0.3)

