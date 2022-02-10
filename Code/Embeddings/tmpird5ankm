## Generate Twitter Pre-Trained Glove embeddings and trained Word2Vec
## Word2Vec
import os 
os.chdir("C:/Users/dordo/Dropbox/Capstone Project")
import pandas as pd
import string 
import re
import pickle
import numpy as np
from nltk.tokenize import TweetTokenizer 
import random
from nltk.corpus import stopwords

##---------------------------------------------------------------------------##

## Load twitter vocabulary
data = pd.read_csv("Data/Twitter/TwitterConsolidated.csv")

## Change URL | NUM for placeholder
## Replace $ by # 
txt_new = []
reg = re.compile(r"https[^ ]+", re.IGNORECASE)
reg_num = re.compile(r"([\\+\\-])?[0-9]+[0-9\\.]*")
reg_mon = re.compile(r"\$([A-Z]+)")
for lines in data["tweet"].values:
    aux = reg.sub("URL", lines)
    aux_2 = reg_num.sub(" \\1NUM", aux)
    txt_new.append(reg_mon.sub("#\\1MO", aux_2))

## Define punctuation to replace (Exclude +, -, and %)
new_punct = string.punctuation + "“”’"
for symb in ["%", "+", "-", "&", "#", "@", "$", "~"]:
    new_punct = new_punct.replace(symb, "")

## Tokenize using the Tweet Tokenizer
tknzr = TweetTokenizer()
txt_corp = []
for doc in txt_new:
    ## Change everything to lowercase and exclude string that are only punctuations
    aux = [elem.lower().strip() for elem in tknzr.tokenize(doc) if elem not in set(new_punct)]
    txt_corp.append(aux)

## Remove strings that only have punctuation signs
exclude = set(["", "#index", "#usmarkets", "#stockmarket", "#s&p100", "#stockmarkets"])

## If removing stopwords is needed
exclude = exclude.union(stopwords.words("english"))

txt_end = []
for doc in txt_corp:
    new_list = [elem.translate(str.maketrans('', '', new_punct)) for elem in doc]
    aux_val = [elem for elem in new_list if elem not in exclude and elem != '️']
    txt_end.append(aux_val)

## Check ending
random.seed(10)
check = random.sample(range(data.shape[0]), 10)
np.array(txt_end)[check]

## Just keep the txt and data
with open('Data/Twitter/ProcessedTwitter.pkl', 'wb') as output:
    pickle.dump(txt_end, output, pickle.HIGHEST_PROTOCOL)

