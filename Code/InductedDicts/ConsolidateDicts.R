library(tidyverse)
setwd("C:/Users/dordo/Dropbox/Capstone Project")

## List all files
fls <- list.files("Data/InducedDicts/SeekingAlpha", full.names = T, "Sem")

## Auxiliary function to read csv
aux_fun <- function(x){
  z <- str_extract(x, "[A-Z]+\\.") %>% str_remove_all("\\.")
  read_csv(x) %>% rename(words = Word) %>% 
    rename_with(.cols = -words, .fn = ~str_c(.x, z)) %>% 
    filter(!is.na(words))
}

## Get dictionaries
data <- reduce(map(fls, aux_fun), full_join)
    
## Save
write_csv(data, "Data/InducedDicts/SemAxisSeekingAlpha.csv")

