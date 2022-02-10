## Doc2Vec Classifier 
library(tidyverse)
library(lubridate)

setwd("C:/Users/dordo/Dropbox/Capstone Project")

##########################################################################
##  Modelling Volatility and returns using directly Doc2Vec embeddings  ##
##########################################################################


# Data Reading ------------------------------------------------------------

## Seeking Alpha ---
doc_embed_sa <- read_csv("Data/SeekingAlphaData/DocumentEmbeddings.csv", col_names = F)
data_sa <- read_csv("Data/SeekingAlphaData/SeekingAlphaUSMarkets.csv")
colnames(doc_embed_sa) <- str_replace_all(colnames(doc_embed_sa), "X", "SA")

## Read Index of leaked data
#omit <- read_csv("Data/SeekingAlphaExcludeID.csv") %>% pull()

## Data ordered by date
all_data <- tibble("Date"=data_sa$Date, doc_embed_sa) %>% 
  arrange(Date) #%>%
#slice(-omit)

all_data_agg <- all_data %>%
  mutate(Date = round_date(Date, unit="day")) %>% 
  group_by(Date) %>%  
  summarise(across(.fns=c(mean, sd))) %>% 
  mutate(across(ends_with("_2"), ~ifelse(is.na(.x), 0, .x)))

##------------------

## Twitter ---
doc_embed_tw <- read_csv("Data/Twitter/DocumentEmbeddingsTwitter.csv", col_names = F)
data_tw <- read_csv("Data/Twitter/TwitterConsolidated.csv") %>%
  rename(Date = date) 
colnames(doc_embed_tw) <- str_replace_all(colnames(doc_embed_tw), "X", "TW")

# ## Read Index of leaked data
# omit <- read_csv("Data/TwitterExcludeID.csv") %>% pull()

## Data ordered by date
all_data <- tibble("Date"=data_tw$Date, doc_embed_tw) %>% 
  arrange(Date) %>%
  filter(between(year(Date), 2019, 2020)) #%>%
#slice(-omit)

all_data_agg_tw <- all_data %>%
  mutate(Date = round_date(Date, unit="day")) %>% 
  group_by(Date) %>%  
  summarise(across(.fns=c(mean, sd))) %>% 
  mutate(across(ends_with("_2"), ~ifelse(is.na(.x), 0, .x)))

##------------------
## Keep data
all_data <- list(all_data_agg, all_data_agg_tw)
rm(list = setdiff(ls(), "all_data"))

# Financial Data Reading --------------------------------------------------

## Read Financial Data

## Returns
data_oex <- read_csv("Data/S&P/LogReturnData.csv") %>% 
  select(1:2) %>%
  rename(Ret=`^OEX`) %>%
  mutate(Dir=sign(Ret),
         Dir = as.factor(ifelse(Dir == 0, -1, Dir))) 

## Volatility
data_oex_var <- read_csv("Data/S&P/VarData.csv") %>% 
  select(1:2) %>% 
  rename(Vol=`^OEX`)

all_oex <- inner_join(data_oex, data_oex_var) %>%
  filter(!is.na(Vol)) %>% 
  mutate(Ehat = c(rep(NA,2), residuals(lm(Vol~lag(Vol) + lag(Vol, 2)))))

rm(data_oex, data_oex_var)


# Data Processing  ------------------------------------------------------

## Join with returns data
reg_data <- all_oex %>% 
  inner_join(all_data[[1]]) %>% 
  inner_join(all_data[[2]])


# Train Test ---------------------------------------------------------------

## Split in train - test
set.seed(910)
holdout <- sample(1:nrow(reg_data), size = 0.2*nrow(reg_data))
holdout <-  all_data %>% select(Date) %>% 
  mutate(Id = month(Date) > 8 & year(Date) == 2020) %>% 
  pull(Id) %>%
  which()
# CCF ---------------------------------------------------------------------

## EDA for cross correlation for each dimension
aux <- reg_data %>% 
  select(-(Ret:Vol)) %>% 
  pivot_longer(cols = SA1_1:TW50_2, names_to = "Dim", 
               values_to = "Coord") %>% 
  arrange(Dim, Date) %>% 
  group_by(Dim) %>% 
  nest() %>% 
  mutate(ccf = map(data, ~ccf(.x[[2]], .x[[3]], na.action = na.omit, plot = F)))

aux_distr <- map(aux$ccf, ~.x$acf) %>% 
  do.call(rbind, .) %>% 
  as_tibble() 

## Summary statistics for CCF distribution
sum_distr <- aux_distr %>%
  summarise(across(.fns=c(~abs(mean(.x)), sd, ~max(abs(.x))))) %>% 
  pivot_longer(V1_1:V47_3) %>%
  separate(name, sep = "_", into = c("Variable", "Num")) 

sum_distr %>% 
  arrange(desc(Num), desc(value)) %>% 
  separate(Variable, into = c("into", "lag"), sep = "V") %>% 
  mutate(lag = as.numeric(lag) - 24) %>% 
  select(-into) -> a

rm(list = setdiff(ls(), c("holdout", "reg_data")))
