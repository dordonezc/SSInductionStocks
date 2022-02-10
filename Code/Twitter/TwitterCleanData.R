## Script to clean Twitter Data
library(tidyverse)
library(quanteda)
setwd("C:/Users/dordo/Dropbox/Capstone Project")

###############################################################################################
##                                        Objective:                                         ##
##  Clean non-useful information of the twitter data by filtering out non relevant hashtags  ##
##  and create consolidated twitter dataset                                                  ##
###############################################################################################


## Auxiliary function to check hashtags
get_hts <- function(data){
  hts <- data %>% select(hashtags) %>% pull()
  hts <- hts %>% str_replace_all("\\[|\\]|'","") %>% str_split(",") %>%
    map(~str_squish(.x)) 
  ## Hashtags by ocurrence
  hts_tab <- unlist(hts) %>% table() %>% as_tibble() 
  hts_tab %>% arrange(desc(n))
}

qread <- function(name){
  # Read Data 2019-2020
  data <- read_csv(str_c("Data/Twitter/", name, ".csv")) %>% 
    arrange(date)
  
  # Read PS data
  data_ps <- read_csv(str_c("Data/Twitter/PreScr/", name, ".csv"))
  
  # Bind
  data <- bind_rows(data, data_ps) %>% 
    arrange(date)
  
  data
}

##----------------------------------------------##
## #SP100
data <- qread("twitter_sp100_data")

# Basic counts of users 
data %>% distinct(username) %>% summarise(n())

## Tweets by user
users <- data %>% count(username) %>% arrange(desc(n))

##  Data of the two most relevant sources is already "clean"
data_clean <- data %>% filter(username %in% (users %>% slice(1:2) %>% pull(username)))

## Dirty data
data_dirt <- data %>% filter(!username %in% (users %>% slice(1:2) %>% pull(username)))

## Words to exclude
wp <- c("boyband", "lincoln", "sound", "gkb", "pcbite", "slingsand",
        "planar", "haztox")
bool <- reduce(with(data_dirt, map(wp, ~str_detect(hashtags,.x))), `|`)

## New clean data
data_clean2 <- data_dirt %>% filter(!bool) 

datasp100 <- data_clean %>% bind_rows(data_clean2) %>% distinct()

#write_csv(datasp100, "twitter_sp100_clean.csv")
rm(list=setdiff(ls(),c("datasp100","get_hts", "qread")))

##---------------------------------------------------------------##
## #usmarkets

# Read Data
data <- qread("twitter_usmarket_data")

# Basic counts of users 
data %>% distinct(username) %>% summarise(n())

# Tweets by user
users <- data %>% count(username) %>% arrange(desc(n))

## Get hashtags
nhts <- get_hts(data)

## Sequential filtering - These are all interesting hashtags
wp <- c("nifty", "trading", "stock", "equity", "forex", "dxy",
        "commodit", "crypto", "futures", "'etfs?'", "invest",
        "nasdaq", "sp500", "volatility", "dax", "covid19",
        "dollar", "bitcoin", "coronavirus", "dowjones",
        "warrenbuffett", "bloomberg","opec", "gold", 
        "pharmaceutical", "wsj", "tradedeal", "marketrisk",
        "gain", "jpmorgan", "capitalmarket", "wallstreet",
        "tradewar", "sgx")

bool <- reduce(with(data, map(wp, ~str_detect(hashtags,.x))), `|`)

## Clean
data_clean <- data %>% filter(bool)

## Dirty
data_dirt <- data %>% filter(!bool)

## Get hashtags of dirty to explore further
check <- get_hts(data_dirt)
check %>% view()

datausmarkets <- data_clean %>% distinct()
#write_csv(datausmarkets, "twitter_usmarket_clean.csv")
rm(list=setdiff(ls(),c("datasp100","get_hts", "datausmarkets", "qread")))

##-------------------------------------------------------------------##
# Read Data
data <- qread("twitter_index_data")

# Basic counts of users 
data %>% distinct(username) %>% summarise(n())

# Tweets by user
users <- data %>% count(username) %>% arrange(desc(n))

## Get hashtags
nhts <- get_hts(data)

## Sequential filtering - These are all interesting hashtags
wp <- c("nifty", "trading", "stock", "equity", "forex", "dxy",
        "commodit", "crypto", "futures", "'etfs?'", "invest",
        "nasdaq", "sp500", "volatility", "dax", "covid19",
        "dollar", "bitcoin", "coronavirus", "dowjones",
        "warrenbuffett", "bloomberg","opec", "gold", 
        "pharmaceutical", "wsj", "tradedeal", "marketrisk",
        "gain", "jpmorgan", "capitalmarket", "wallstreet")
bool <- reduce(with(data, map(wp, ~str_detect(hashtags,.x))), `|`)

## Clean
data_clean <- data %>% filter(bool)

## Dirty
data_dirt <- data %>% filter(!bool)

## Get hashtags of dirty to explore further
check <- get_hts(data_dirt)
check %>% view()

dataindex <- data_clean %>% distinct()

#write_csv(datausmarkets, "twitter_usmarket_clean.csv")
rm(list=setdiff(ls(),c("datasp100","get_hts", "datausmarkets", "dataindex")))

##----------------------------------------------------------------##
## #stockmarket

## Read data
data <- qread("twitter_stockmarket_data")

## Paste
full_data <- bind_rows(data, datausmarkets, datasp100, dataindex) %>%
  arrange(date) %>% distinct()

#write_csv(full_data, "Data/Twitter/TwitterConsolidated.csv")
