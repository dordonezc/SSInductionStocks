## Generate Seeking Alpha Doc2Vec

import os 
os.chdir("C:/Users/dordo/Dropbox/Capstone Project")
import pandas as pd
import numpy as np
import pickle 
import re
import random
from gensim.models.doc2vec import Doc2Vec, TaggedDocument
##---------------------------------------------------------------------------##

random.seed(10)
## Load Seeking Alpha pre cleaned data
with open('Data/SeekingAlphaData/ProcessedSeekingAlpha.pkl', 'rb') as file:
    data = pickle.load(file)

## Create Model
documents = [TaggedDocument(doc, [i]) for i, doc in enumerate(data)]
model = Doc2Vec(vector_size=50, min_count=5, epochs = 40)
model.build_vocab(documents)
model.train(documents, total_examples=model.corpus_count, epochs=model.epochs)

## Obtain embeddings
doc_embed = []
for i in range(len(data)):
    doc_embed.append(model.infer_vector(data[i]))

## Save embeddings
doc_embed = np.array(doc_embed)
np.savetxt("Data/SeekingAlphaData/DocumentEmbeddings.csv", doc_embed, delimiter=",")
