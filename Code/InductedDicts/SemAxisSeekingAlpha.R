library(tidyverse)
setwd("C:/Users/Chenxing Zu/Dropbox/Capstone Project")

# Read Embeddings ---------------------------------------------------------
 
## Pretrained
data <- read_csv("Data/Embeddings/SeekingAlphaPreTrained.csv") %>%
  select(-1)

## Self Trained
# data <- read_csv("Data/Embeddings/SeekingAlphaTrained.csv") %>%
#   select(-1)
##--------------------##

## Readings with problems correspond to white spaces
(norm_factor <- data %>% summarise(across(.fns=~sqrt(sum(.x^2)))))

## Normalize
data <- data %>% mutate(across(.fns=~.x/sqrt(sum(.x^2))))

# Negative Seeds ----------------------------------------------------------
negative <- read_csv("Data/Seeds/SAlpha_negative_seeds.csv") %>% 
  select(name, mean, Color) %>% 
  mutate(Color = ifelse(is.na(Color), "None", Color)) %>% 
  filter(Color != "Red")

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
positive <- read_csv("Data/Seeds/SAlpha_positive_seeds.csv") %>% 
  select(name, mean, Color) %>% 
  mutate(Color = ifelse(is.na(Color), "None", Color)) %>% 
  filter(Color != "Red")

positive_nony <- positive %>% filter(Color != "Yellow")

## Check
index <- positive$name %in% colnames(data)
mean(index)

## Good Vector
good <- data %>% select(positive$name[index]) %>% 
  rowMeans()

good_nony <- data %>% select(positive_nony$name[positive_nony$name %in% colnames(data)]) %>% 
  rowMeans()

# Semantic Axis -----------------------------------------------------------

get_polarities <- function(good, bad, data){
  axis <- good - bad
  dot_p <- t(axis %*% as.matrix(data))
  new_dict <- tibble("Word"=rownames(dot_p), "Sim"=dot_p[,1]) %>% 
    arrange(desc(Sim))
  new_dict
}

new_dicts <- expand.grid("Good"=c("good", "good_nony"), "Bad"=c("bad", "bad_nony")) %>% 
            as_tibble() %>% 
            mutate(res = map2(Good, Bad, ~get_polarities(get(as.character(.x)), 
                                                        get(as.character(.y)), data = data)))

# EDA ---------------------------------------------------------------------

new_dicts$res[[4]] %>% ggplot(aes(x=Sim)) + geom_histogram()

## Join
out_df <- reduce(new_dicts$res, ~inner_join(.x, .y, by = "Word"))
colnames(out_df) <- c("Word", "GFu-BFu", "GFi-BFu", "GFu-BFi", "GFi-BFi")
out_df

## Save
write_csv(out_df, "Data/InducedDicts/SemAxisSeekingAlphaPT.csv")
