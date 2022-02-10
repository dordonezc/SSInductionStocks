## Doc2Vec news Freshness
library(tidyverse)
library(lubridate)
setwd("C:/Users/dordo/Dropbox/Capstone Project")

##------------------------------------------------------------------------------------------##
## Read Doc2Vec data 

##------------- Seeking Alpha
# doc_embed <- read_csv("Data/SeekingAlphaData/DocumentEmbeddings.csv", col_names = F)
# data <- read_csv("Data/SeekingAlphaData/SeekingAlphaUSMarkets.csv")

##------------- Twitter
doc_embed <- read_csv("Data/Twitter/DocumentEmbeddingsTwitter.csv", col_names = F)
data <- read_csv("Data/Twitter/TwitterConsolidated.csv") %>% 
  rename(Date = date)

##-----------------------
## Data ordered by date
all_data <- tibble("Date"=data$Date, doc_embed) %>% arrange(Date)

##EWMA Fun
ewma <- function(x, serie, lambda = 0.2){
  lambda * x + (1-lambda) * serie
}

val <- list(as.matrix(all_data[1, -1]))
for(i in 1:nrow(all_data)){
  val[[i + 1]] <- unname(ewma(as.matrix(all_data[i, -1]), val[[i]]))
}
ewma_mat <- do.call(rbind, val)

## Create DataFrame by Text
news_mat <- tibble("Date"=all_data$Date, as_tibble(ewma_mat[-1,])) %>% arrange(Date)
colnames(news_mat) <- c("Date", str_c("X", 1:ncol(ewma_mat)))

rm(list=setdiff(ls(), c("all_data", "news_mat")))
##---------------------------------------------------------------------------------------------##
## Cosine aux function

cos_sim <- function(x, y){
  sum(x*y)/sqrt(sum(x^2)*sum(y^2))
}

## Cosine Similarity by day - Generate auxiliary dataframes
easy_cosine <- function(x, y){
  a <- x %>% 
    group_by(Date) %>% 
    nest() %>% 
    mutate(data=map(data, as.matrix)) %>%
    rename(dataday = data)
  
  b <- y %>% 
    group_by(Date) %>% 
    nest() %>% 
    mutate(data=map(data, as.matrix))
  
  ## Create cosine
  out <- inner_join(a, b) %>%
    group_by(Date) %>%
    mutate(CosSim=map2_dbl(dataday, data, cos_sim)) %>%
    select(Date, CosSim)
  
  out %>% ungroup()
}

##---------------------------------------------------------------------------------------------##
## Averaging cosines
day_full <- all_data %>% mutate(Date = lag(Date))

out_cos <- easy_cosine(news_mat, day_full) %>% 
  mutate(Date=round_date(Date, unit = "day")) %>% 
  group_by(Date) %>% 
  summarise(VCosSim = mad(CosSim),
            CosSim=mean(CosSim),
  ) %>% filter(between(year(Date), 2019, 2020))

##--------------------------------------------------------------------------------------------##
## Save
#write_csv(out_cos, "Results/Freshness/TWCosine.csv")
