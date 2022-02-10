import os  
import sys
os.chdir("C:/Users/dordo/Dropbox/Capstone Project")
sys.path.append('C:/Users/dordo/Dropbox/Capstone Project/Code')
import numpy as np
from sklearn.preprocessing import normalize
import matplotlib.pyplot as plt
import pandas as pd 
from RandomWalk import random_walk

## Objective: Use Hamilton method to induce dictionary

## Bad words
bad_words_data = pd.read_csv("Data/Seeds/SAlpha_negative_seeds.csv")

## Filtered
index = (bad_words_data["Color"] != "Red") & (bad_words_data["Color"] != "Yellow")
bad_words_data = bad_words_data[index]
bad_words = bad_words_data.name.values

## Good Words
good_words_data = pd.read_csv("Data/Seeds/SAlpha_positive_seeds.csv")
index = (good_words_data["Color"] != "Red") #& (good_words_data["Color"] != "Yellow")
good_words_data = good_words_data[index]
good_words = good_words_data.name.values

## Read embeddings
w_embed = pd.read_csv("Data/Embeddings/SeekingAlphaPreTrained.csv")
w_embed.drop('Unnamed: 0', inplace = True, axis = 1)
words = w_embed.columns.values
embeddings = w_embed.T.values

## Check
np.sum([wrd in words for wrd in bad_words])

##---------------------------------------------------------------------------##
## This is only needed for GLOVE 

# keep_id =np.isnan(w_embed).sum(axis=0) < 50
# embeddings = w_embed[words[keep_id]].T.values
# words = words[keep_id]

# ## Refilter words in seeds 
# bad_words = bad_words[np.array([wrds in words for wrds in bad_words])]
# good_words = good_words[np.array([wrds in words for wrds in good_words])]

##---------------------------------------------------------------------------##

## Normalize
normalize(embeddings, copy=False)

## Sentiprop
polarities_pos, polarities_neg = random_walk(words, embeddings,
                                             good_words, bad_words,
                                             beta=0.95, nn=50, arccos=True)


## Polarities DF - 
polarities_df = pd.DataFrame(polarities_pos.items())
polarities_df.columns = ["words", "polarityPos"]
polarities_df.sort_values("polarityPos",inplace = True)

polarities_dfneg = pd.DataFrame(polarities_neg.items())
polarities_dfneg.columns = ["words", "polarityNeg"]
polarities_dfneg.sort_values("polarityNeg",inplace = True)

## Join
df = polarities_df.join(polarities_dfneg.set_index("words"), on = "words")

## To Csv
df.to_csv("Data/InducedDicts/PropPolaritiesSeekingAlphaPTFilteredFull.csv", index = False)
