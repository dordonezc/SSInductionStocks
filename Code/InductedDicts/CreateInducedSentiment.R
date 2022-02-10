library(tidyverse)
library(reticulate)
setwd("C:/Users/dordo/Dropbox/Capstone Project")


# Load Datasets -----------------------------------------------------------

## Load Pre Cleaned Corpus
pick <- import("pickle")
py <- import_builtins()
all_data <- with(py$open('Data/SeekingAlphaData/ProcessedSeekingAlpha.pkl', 'rb') %as% file, {
  pick$load(file)
})

## Load Potential Dates
data_twit <- read_csv("Data/SeekingAlphaData/SeekingAlphaUSMarkets.csv")

## Load Senti Prop
data_sents <- read_csv("Data/InducedDicts/SentipropSeekingAlpha.csv")

## Load Sem axis
data_sents_axis <- read_csv("Data/InducedDicts/SemAxisSeekingAlpha.csv") 

# Comparison of tokens ----------------------------------------------------
aux <- unique(unlist(all_data))
index <- !(data_sents$words %in% aux)
data_sents$words[index]

# Auxiliary Functions -----------------------------------------------------

## Create lookup table
lookup <- possibly(function(x, ltab){
  aux <- ltab[x]
  ind <- !is.na(aux)
  c("Score"=sum(aux[ind]), "Matches"=mean(ind))
}, c(NA,NA))

get_sent <- function(x, data){
  ltab <- structure(x[[2]], names = x[[1]])
  res <- map(data, lookup, ltab = ltab)
  as_tibble(do.call(rbind, res))
}

# Generate Polarities -----------------------------------------------------

nested_data <- map(list(data_sents, data_sents_axis), 
                   ~ .x %>% pivot_longer(-words) %>% 
                  arrange(name, words) %>% 
                  group_by(name) %>% 
                  nest()) %>% 
                  reduce(full_join)
  
#system.time({res <- get_sent(nested_data$data[[3]], data = data)})
res <- nested_data %>% 
  ungroup() %>% 
  mutate(Sents = map(data, get_sent, data = all_data ))

# Joining with dates ------------------------------------------------------
res_save <- res %>% select(name, Sents) %>%  
  mutate(Sents = map(Sents, ~tibble("Date"=data_twit$Date, .x)))

saveRDS(res_save, "Data/InducedDicts/ConsolidatedSeekingAlpha.rds")

# Create Daily Sentiment Series -------------------------------------------

sent_series <- res_save %>% 
  mutate(Daily = map(Sents, ~ .x %>% 
                       mutate(Date=round_date(Date, "day")) %>% 
                       filter(between(year(Date),2019,2020)) %>% 
                       group_by(Date) %>%  
                       summarise(SMean=mean(Score)))) %>% 
  select(name, Daily)

all_series <- sent_series %>% 
  unnest() %>% 
  pivot_wider(id_cols = Date, names_from = name, values_from = SMean)

## Save
write_csv(all_series, "Results/Sentiment/SeekingAlphaSI.csv")
