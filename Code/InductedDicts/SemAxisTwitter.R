library(tidyverse)
setwd("C:/Users/dordo/Dropbox/Capstone Project")

# Read Embeddings ---------------------------------------------------------

## Pretrained
# data <- read_csv("Data/Embeddings/TwitterPreTrained.csv") %>%
#   select(-1)

## Self Trained
# data <- read_csv("Data/Embeddings/TwitterTrained.csv") %>%
#   select(-1)

## Glove
data <- read_csv("Data/Embeddings/TwitterGlove.csv") %>%
  select(-1)

## Words that appear for glove
wrds_2_keep <- data %>% summarise(across(.fns=~sum(is.na(.x)))) %>%
  pivot_longer(everything()) %>%
  filter(value<50)
data <- data %>% select(wrds_2_keep$name)

##--------------------##

## Readings with problems correspond to white spaces
(norm_factor <- data %>% summarise(across(.fns=~sqrt(sum(.x^2)))))

## Normalize
data <- data %>% mutate(across(.fns=~.x/sqrt(sum(.x^2))))

# Negative Seeds ----------------------------------------------------------
negative <- read_csv("Data/Seeds/Twitter_negative_seeds.csv") %>% 
  select(name, mean, Color)

negative_nony <- negative %>% filter(Color != "Yellow")

## Check
index <- negative$name %in% colnames(data)
mean(index)

## Bad Vector
bad <- data %>% select(negative$name[index]) %>% 
  rowMeans()

bad_nony <- data %>% select(negative_nony$name[negative_nony$name %in% colnames(data)]) %>% 
  rowMeans()

# Positive Seeds ----------------------------------------------------------
positive <- read_csv("Data/Seeds/Twitter_positive_seeds.csv") %>% 
  select(name, mean) %>% 
  mutate(name = ifelse(name == "cash:-", "cash", name)) %>% 
  mutate(name = ifelse(name == "#top40", "#top", name)) ## Fix problems in punctuation and 
                                                        ## numbers
## Check
index <- positive$name %in% colnames(data)
mean(index)

## Good Vector
good <- data %>% select(positive$name[index]) %>% 
  rowMeans()

# Semantic Axis -----------------------------------------------------------

get_polarities <- function(good, bad, data){
  axis <- good - bad
  dot_p <- t(axis %*% as.matrix(data))
  new_dict <- tibble("Word"=rownames(dot_p), "Sim"=dot_p[,1]) %>% 
    arrange(desc(Sim))
  new_dict
}

new_dict <- get_polarities(good, bad, data)
new_dict_nony <- get_polarities(good, bad_nony, data)

# EDA ---------------------------------------------------------------------

new_dict
new_dict_nony 
new_dict_nony %>% ggplot(aes(x=Sim)) + geom_histogram()

## Join
out_df <- new_dict %>% inner_join(new_dict_nony, by = "Word", suffix = c("Full", "Filtered"))
out_df

## Save
write_csv(out_df, "Data/InducedDicts/SemAxisTwitterG.csv")
