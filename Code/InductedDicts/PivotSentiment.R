library(tidyverse)
setwd("C:/Users/dordo/Dropbox/Capstone Project")

## This script transforms the saved sentiment series from a wide format to a long format 
## and calculates an index for the sentiprop method in the spirit of the original McDondald. 


# Read Data ---------------------------------------------------------------
data <- read_csv("Results/Sentiment/Supervised/SeekingAlphaSI.csv")


# Get Tibble for both methods --------------------------------------------

## Sem Axis for twitter
# sem_axis <- data %>% pivot_longer(-Date) %>%
#   filter(str_detect(name, "Sim")) %>%
#   mutate(name = str_remove_all(name, "SimFull"),
#          name = str_replace_all(name, "SimFiltered([A-Z]+)", "\\1F")) %>%
#   rename(Type=name, Score = value) %>%
#   mutate("Method"="SemAxis")

## Sem axis for Seeking Alpha
sem_axis <- data %>% pivot_longer(-Date) %>%
  filter(str_detect(name, "GF")) %>%
  rename(Type=name, Score = value) %>%
  mutate("Method"="SemAxis")


## Senti Prop for Twitter
# senti_prop <- data %>% pivot_longer(-Date) %>%
#   filter(!str_detect(name, "Sim")) %>%
#   mutate(name = str_remove_all(name, "polarity"),
#          name = str_replace(name, "Neg([A-Z]+)", "Negative-\\1"),
#          name = str_replace(name, "Pos([A-Z]+)", "Positive-\\1")) %>%
#   separate(name, into = c("Polarity", "Type"), sep = "-") %>%
#   arrange(Type, Date)

## Senti prop for Seeking Alpha
senti_prop <- data %>% pivot_longer(-Date) %>%
  filter(!str_detect(name, "GF")) %>%
  mutate(name = str_remove_all(name, "polarity"),
         name = str_replace(name, "Neg([A-Z]+)", "Negative-\\1"),
         name = str_replace(name, "Pos([A-Z]+)", "Positive-\\1")) %>%
  separate(name, into = c("Polarity", "Type"), sep = "-") %>%
  arrange(Type, Date)

## Index of interest
end_senti_prop <- senti_prop %>% 
  group_by(Type, Date) %>% 
  summarise(Score = value[2] - value[1]) %>% 
  ungroup() %>% 
  mutate("Method"="SentiProp")

end_senti_prop <- end_senti_prop %>% mutate(Type = str_replace_all(Type, "([F])", "-\\1")) %>% 
  separate(Type, sep = "-", into = c("Embedding", "GType", "BType")) %>% 
  mutate(BType = ifelse(is.na(BType), GType, BType)) %>% 
  mutate(across(c(GType, BType), ~ifelse(.x == "Full", "NF", "F")))


# Join --------------------------------------------------------------------
all_data <- full_join(sem_axis, end_senti_prop)

## For twitter
sem_axis <- sem_axis %>% mutate(Type = str_replace(Type, "([A-Z]+$)", "-\\1" )) %>% 
  separate(sep="-", Type, into = c("GType", "BType", "Embedding")) %>% 
  mutate(BType = ifelse(BType == "BFi", "F", "NF"),
         GType = ifelse(GType == "GFi", "F", "NF")) %>% 
  select(Date, Embedding, BType, GType, Method, Score)


# Save Series -------------------------------------------------------------
#write_csv(all_data, "Results/Sentiment/Supervised/SASentimentLong.csv")


