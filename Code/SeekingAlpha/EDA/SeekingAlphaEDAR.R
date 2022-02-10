setwd("C:/Users/dordo/Dropbox/Capstone Project")
library(reticulate)
library(tidyverse)
library(tidytext)
library(quanteda)
library(spacyr)

## Read data
data <- read_csv("Data/SeekingAlphaData/SeekingAlphaUSMarkets.csv")

## Parse number entities by hand
get_content <- function(x){
  ## Parse Stock names
  x <- str_replace_all(x, "COVID", "covid")
  pat <- "[A-Z]{3,4}"
  ents <- str_extract_all(x, pat)[[1]]
  new_x <- str_replace_all(x, pat, "ENT")
  ## Parse Numbers
  pat <- "[-\\+\\$]?[0-9]{1}[0-9,\\.]*[M\\%\\$]?"
  nums <- str_extract_all(new_x, pat)[[1]]
  nums <- str_remove_all(nums, "[\\.,]$")
  new_x <- str_replace_all(new_x, pat, "NUM")
  list(new_x, list(nums, ents))
}

## Spacy PARSER
spacy_parser <- function(x){
  parsed_txt <- spacy_parse(x,entity = TRUE, lemma = FALSE)
  tokens <- spacy_tokenize(x, remove_punct = T)
  doc_ents <- entity_extract(parsed_txt) %>% 
    filter(!(entity == "ENT" | str_detect(entity, "NUM.?"))) %>%
    select(entity, entity_type)
  relevant <- parsed_txt %>% filter(token %in% tokens$text1) %>%
    mutate(token=ifelse(token %in% c("ENT", "NUM"), token, str_to_lower(token))) %>%
    anti_join(tidytext::stop_words, by=c("token"="word"))
  toks <- relevant %>% select(token) %>% pull()
  pos <- relevant %>% select(pos) %>% pull()
  list(toks, pos, doc_ents)
}


## Get parsed content
new_data <- map(data$Text[1:100], get_content)

## Pluck text
txt_vec <- map(new_data,1)

## Bring spacy to tag
spacy_initialize(model = "en_core_web_sm", condaenv = "C:/Users/dordo/miniconda3")

## Spacy parse
txt_spacy <- map(txt_vec, spacy_parser)

## End Spacy
spacy_finalize()


## Generate full data
txt_spacy[[100]]
data$Text[[100]]


##-------------------------------------------------------------------------------------##
## Read in parser
#source_python("Code/Exploration/SeekingAlphaEDA.py")
