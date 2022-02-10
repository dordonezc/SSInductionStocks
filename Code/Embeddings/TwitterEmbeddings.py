## Generate twitter Pre-Trained Word2Vec and trained Word2Vec
## Word2Vec
import os 
os.chdir("C:/Users/dordo/Dropbox/Capstone Project")
import pandas as pd
import pickle
from gensim import corpora
from gensim.models import Word2Vec
import gensim.downloader as api

##---------------------------------------------------------------------------##
## Define function to get embeddings from memory
def get_wv(model, dicts):
    """ Get word embeddings in memory"""
    w2v_embed = {}
    missing = []
    for val in dicts.values():
        try: 
            it = model.wv[val]
        except:
            missing.append(val)
            it = None 
        w2v_embed[val] = it
    return w2v_embed, missing
##---------------------------------------------------------------------------##

## Reading in pre processed data
with open('Data/Twitter/ProcessedTwitter.pkl', 'rb') as input:
    txt_end = pickle.load(input)

## Create dictionary
dicts = corpora.Dictionary(txt_end)
len(dicts)

## Filter by appeareance in documents
dicts.filter_extremes(no_below=40, no_above=0.5, keep_n=None, keep_tokens=None)
len(dicts)

##--------------------------------------------------------------------------##
## PreTrained Word2vec
path = "C:/Users/dordo/Documents/Daniel/LSE/Capstone/Modelo/GoogleNews-vectors-negative300.bin"

model = Word2Vec(txt_end, size = 300, min_count = 40)
model.intersect_word2vec_format(path,
                                lockf=1.0,
                                binary=True)

model.train(txt_end, total_examples=model.corpus_count, epochs=25)
embeds_1 = get_wv(model, dicts)

## How many word of our corpus appear in the pre trained?

##---------------------------------------------------------------------------##
## Self Trained Word2Vec

model_t = Word2Vec(txt_end, window=5, min_count=40, workers=4, size = 50)
model_t.train(txt_end, epochs=50, total_words = model_t.corpus_total_words,
              total_examples = model_t.corpus_count)
embeds_2 = get_wv(model_t, dicts)

##---------------------------------------------------------------------------##
## Pre Trained GLOVE
model_g = api.load("glove-twitter-50")
embeds_3 = get_wv(model_g, dicts)
embeds_3df = pd.DataFrame(embeds_3[0])

## This are the embeddings that are really available in GLOVE
embeds_3df.T[~embeds_3df.T[1].isnull()]

##---------------------------------------------------------------------------##
## Saving
pretrained_embed = pd.DataFrame(embeds_1[0])
trained_embed = pd.DataFrame(embeds_2[0])
glove_embed = pd.DataFrame(embeds_3df)

pretrained_embed.to_csv("Data/Embeddings/TwitterPreTrained.csv")
trained_embed.to_csv("Data/Embeddings/TwitterTrained.csv")
glove_embed.to_csv("Data/Embeddings/TwitterGlove.csv")

#{item:value for item, value in embeds_3.items() if value is not None}
## Check model keys
# for index, word in enumerate(model.index2word):
#     if index == 50:
#         break
#     print(f"word #{index}/{len(model.index2word)} is {word}")
    
