## Data PreProcessing script for Seeking Alpha corpus.
## The script is used to work with Pre-Trained Word2Vec and trained Word2Vec embeddings

import os 
os.chdir("C:/Users/dordo/Dropbox/Capstone Project")
import pandas as pd
import numpy as np
import pickle 
import string 
import re
import nltk
import random

##---------------------------------------------------------------------------##

## Load Seeking Alpha Corpus
data = pd.read_csv("Data/SeekingAlphaData/SeekingAlphaUSMarkets.csv")

## Change Numbers info for placeholder keep signs 
txt_new = []
reg = re.compile(r"([\\+\\-])?[0-9]+[0-9\\.]*")
for lines in data["Text"].values:
    txt_new.append(reg.sub(" \\1NUM", lines))

## Define punctuation to replace (Exclude +, -, and %)
new_punct = string.punctuation + "“”’"
for symb in ["%", "+", "-", "&"]:
    new_punct = new_punct.replace(symb, "")

## String list
txt_corp = []
for doc in txt_new:
    ## Change everything to lowercase and exclude string that are only punctuations
    aux = [elem.lower() for elem in nltk.word_tokenize(doc) if elem not in set(new_punct)]
    txt_corp.append(aux)

## Remove strings that only have punctuation signs
exclude = [""]
txt_end = []
for doc in txt_corp:
    new_list = [elem.translate(str.maketrans('', '', new_punct)) for elem in doc]
    txt_end.append([elem for elem in new_list if elem not in exclude])
    
## Check ending
random.seed(10)
check = random.sample(range(4718), 10)
np.array(txt_end)[check]

## Just keep the txt and data
with open('Data/SeekingAlphaData/ProcessedSeekingAlpha.pkl', 'wb') as output:
    pickle.dump(txt_end, output, pickle.HIGHEST_PROTOCOL)


## We could do Stop Word Removal
## Could also add stemming
    



##---------------------------------------------
## Old VERSION
## Remove punctuations "\n"
# new_punct = string.punctuation + "“”"
# for symb in ["%", "+", "-"]:
#     new_punct = new_punct.replace(symb, "")
# txt_new = [line.translate(str.maketrans(' ', ' ', new_punct)).replace("\n", " ") for line in txt_new]

## Separate tokens
#txt_corp = [[tok.strip() for tok in txt.lower().split(" ") if tok not in ""] for txt in txt_new]
##--------------------------------------------