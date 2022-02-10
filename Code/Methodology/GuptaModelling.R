library(tidyverse)
library(quanteda)
library(e1071)
library(reticulate)
library(lubridate)
setwd("C:/Users/USUARIO/Dropbox/Capstone Project")

##########################################################################
##  Using the Gupta Method to Classify direction in returns             ##
##########################################################################

## NOTE: Should round date to increase precision on results being "good"


# Data Reading ------------------------------------------------------------

## Load Pre Cleaned Corpus
pick <- import("pickle")
py <- import_builtins()
# data <- with(py$open('Data/SeekingAlphaData/ProcessedSeekingAlpha.pkl', 'rb') %as% file, {
#   pick$load(file)
# })
data <- with(py$open('Data/Twitter/ProcessedTwitter.pkl', 'rb') %as% file, {
  pick$load(file)
})

## Seeking Alpha Embeddings - Can Change Embeddings here ---
# word_embed <- read_csv("Data/Embeddings/SeekingAlphaTrained.csv") %>% 
#   select(-1)
word_embed <- read_csv("Data/Embeddings/TwitterPreTrained.csv") %>%
  select(-1)

## Seeking Alpha original Data
# data_sa <- read_csv("Data/SeekingAlphaData/SeekingAlphaUSMarkets.csv") %>% 
#         mutate(Date = round_date(Date, unit = "day"))
data_sa <- read_csv("Data/Twitter/TwitterConsolidated.csv") %>% 
          rename(Date = date) %>% 
        mutate(Date = round_date(Date, unit = "day"))


## Seeking alpha by date
data_sa_gr <- data_sa  %>% 
  group_by(Date) %>% 
  summarise(n=n())

# Read Financial Data -----------------------------------------------------

## Returns Data 
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
  mutate(Ehat = c(rep(NA,2), residuals(lm(Vol~lag(Vol) + lag(Vol, 2))))) %>% 
  filter(between(year(Date), 2019, 2020))

## This is the data that we are using in all the modelling examples
all_data <- all_oex %>% 
  inner_join(data_sa_gr)

## This is the version of the data to keep the labels
all_data_long <- left_join(data_sa, data_oex) 

## Read Index of leaked data
#omit <- read_csv("Data/SeekingAlphaExcludeID.csv") %>% pull()

## Keep dates that are consistent with the data used in all examples (all_data)
index_keep <- all_data_long$Date %in% all_data$Date

data <- data[index_keep]
all_data_long <- all_data_long %>%
  filter(index_keep) %>% 
  group_by(Date) %>%
  mutate(Gr=cur_group_id()) %>% 
  ungroup()


## Approximately 500 news are lost in SA 20k in twitter

rm("file", "pick" ,"py", "data_sa", "data_oex", "all_oex", "data_oex_var", "data_sa_gr",
   "index_keep")


# Get input matrix --------------------------------------------------------

## Create DFM  Th: 5 SA | 40 TW
dfm_words <- as.tokens(data) %>% 
  dfm() %>% 
  dfm_trim(min_docfreq = 25) %>% 
  dfm_group(all_data_long$Gr)

## DFM reorders by the order of the group WATCH OUT

## Check that word names are the same
wrds <- dfm_words %>% colnames()
wrds_2 <- word_embed %>% colnames()
wrds[!wrds %in% wrds_2]

## There are words that get lost when filtering the dates
drop_wrds <- wrds_2 %in% wrds

## Replace embeddings
word_embed <- word_embed[, drop_wrds]

## Drop from DFM word that don't match
wrds_2 <- word_embed %>% colnames()
dfm_words <- dfm_words %>% dfm_keep(intersect(wrds, wrds_2))

## Create Weighted matrix
wts_mat <- dfm_words %>% 
  dfm_weight(scheme = "prop") %>% 
  convert("matrix")

wrd_names <- colnames(word_embed)

## Put it in the format needed: A vector with label and two matrices with embeddings | weights
wts_mat <- wts_mat[,wrd_names]
word_embed <- t(as.matrix(word_embed))
label_vct <- ifelse(as.numeric(all_data$Dir) - 1 == 0, -1, 1)

# Gupta Application -------------------------------------------------------
source("Code/GuptaFuns.R")

## Split in train - test as all other modelling scripts
set.seed(910)
n <- nrow(all_data)
holdout <- sample(1:n, size = 0.2 * n)

## Take out training sample: Re Use Omitted Sample for the Seed parts
pool <- setdiff(1:n, holdout)

## Cross validation: Determine number of folds and generate pool with indexes based on 
## dates
set.seed(1036)
ncv <- 5

## Sample
num_fold <- floor(length(pool)/ncv)
pooled <- sample(pool)

## Create start-end for each cross validation fold and sample from pool of indexes
ends <- num_fold*1:(ncv-1)
index_mat <- rbind(c(1, ends + 1), c(ends, length(pool)))
index_cv <- apply(index_mat, 2, function(x, y){y[seq(x[1],x[2])]}, y = pooled)

## Add the omit training points
train_id <- lapply(index_cv, setdiff, x = pooled)

## Note that test id is index_cv

##-------------------------##
### Modelling
fold_id <- 1

# for(i in 1:ncv){
#   ## Run this code to get lambdas for each cv fold
#   use_index <- train_id [[i]]
# 
#   ## Using the method to calculate the Lambdas
#   system.time({lambdas <- directed_ls_weights(wts_mat[use_index,],
#                                               word_embed, label_vct[use_index])})
#   saveRDS(lambdas, str_c("Data/Lambda_fold", i, "TWPT.rds"))
# }

#saveRDS(lambdas, "Data/Lambda_fold1New.rds")
lambdas <- map(dir("Data/GuptaLambdas", full.names = T, pattern ="TWPT"), read_rds) ## First Version

# SVM Modelling -----------------------------------------------------------
lag_vct <- lag(label_vct)

## Generate lags and new A mat
get_model <- function(use_index, lambdas){
  A_mat <- wts_mat[use_index,] %*% (lambdas$Lambda * word_embed)
  
  ## Find Missing position to exclude
  lag_vct_new <- lag_vct[use_index]
  response_vct <- label_vct[use_index]
  missing <- which(is.na(lag_vct_new))
  
  ## SVM Model
  if(length(missing) != 0){
    mod <- svm(x=cbind(lag_vct_new[-missing], A_mat[-missing,]),
               y=as.factor(response_vct[-missing]))
  } else {
    mod <- svm(x=cbind(lag_vct_new, A_mat),
               y=as.factor(response_vct))
  }
  mod
}

mods <- map2(train_id, lambdas, get_model)

# Testing -----------------------------------------------------------------

## Generate PMat
get_preds <- function(test_index, lambdas, mod){
  P_mat <- wts_mat[test_index,] %*% (lambdas$Lambda * word_embed)
  pred_values <- predict(mod, cbind(lag_vct[test_index], P_mat))
  
  ## Table to check news classification
  tab <- table(pred_values, label_vct[setdiff(test_index, 1)])
  list("Preds"=pred_values, "Confusion"=tab, "Original" = label_vct[setdiff(test_index, 1)])
}

## Predict on CV fold
res <- pmap(list(index_cv, lambdas, mods), get_preds)

## Predict on Holdout set
res_hold <- pmap(list(rep(list(holdout), 5), lambdas, mods), get_preds)

## Create CSV file
data_list <- map2(map(res_hold, "Original"), 
                  map(res_hold, "Preds"), ~tibble("Actual"=.x, "value"=.y))

## All preds
mat <- reduce(map2(data_list, as.list(1:5), ~tibble(.x %>% mutate(holdout_id = row_number()),
                                                    name = .y)), bind_rows)
write_csv(mat, "Results/Direction/Gupta.csv")

## Accuracy
map_dbl(map(res, 2), ~sum(diag(.x))/sum(.x))
acc <- map_dbl(map(res_hold, 2), ~sum(diag(.x))/sum(.x))
mean(acc)

