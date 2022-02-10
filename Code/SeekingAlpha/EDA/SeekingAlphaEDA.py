import spacy
import os 
os.chdir("C:/Users/dordo/Dropbox/Capstone Project")
import pandas as pd
#import numpy as np
#import nltk as nl
import re
#from sklearn.feature_extraction.text import CountVectorizer
#nltk.download('punkt')

## This script can be run in R using reticulate... 


## Original Data1
data = pd.read_csv("Data/SeekingAlphaData/SeekingAlphaUSMarkets.csv")

##----##
nlp = spacy.load("en_core_web_md")

## Removing stop words and punctuation
stopwords = spacy.lang.en.stop_words.STOP_WORDS

def get_content(data):
    doc = nlp(data)
    ## Lemmas
    lemmas = [token.lemma_ for token in doc]
    
    ## POS 
    pos_tags = [token.pos_ for token in doc]
    
    ## People
    ppl = [ent.text for ent in doc.ents if ent.label_ == "PERSON"]
    
    ## Parse
    cl_lemmas, cl_tags = [], []
    for lemma, word, tag in zip(lemmas, doc, pos_tags):
        if lemma.isalpha() and lemma not in stopwords:
            cl_lemmas.append(lemma)
            cl_tags.append(tag)
        else:
            aux = re.search("[+-]?[0-9]{1}[0-9\\.]+\\%?", str(word))
            if  aux != None:
                cl_lemmas.append("NUM")
                cl_tags.append("NUM")
    
    ## Search for Stock mentions
    ents = [re.search("[A-Z]{2}[A-Z]+", str(token)) for token in doc]
    entsp = [ent.group() for ent in ents if ent != None]
    
    ## Search for numeric info
    nums = re.findall("[+-]?[0-9]{1}[0-9\\.]+\\%?", data)

    ## New list of words
    bg = [lemma.lower() if lemma not in entsp else "ENT" for lemma in cl_lemmas]
    return bg, cl_tags, ppl, ents, nums


##---------------------------------##

# Example
# get_content(data.Text[20])

# Run
saved, res_all = [], []
for elem in data.Text:
    res = get_content(elem)
    res_all.append(res)
    saved.append(" ".join(res[0]))

## Bag of words
#cv = CountVectorizer()
#bow = cv.fit_transform(saved)

##-------------------------------------------------------------------------##
# tokens = nl.word_tokenize(data.Text[1])
# doc = nl.Text(tokens)

# ## Measures
# doc.concordance("mortgage")
# doc.similar("low")
# doc.common_contexts(["house", "decline"])

# ## Count by word
# doc.count("mortgage")
# len(set(doc))/len(doc)

# ## General count
# freq_c = nl.FreqDist(doc)
# freq_c

# ## Bigrams
# nl.bigrams(doc)

# ## Collocation
# doc.collocations()

# ## Stop words
# stopwords = nltk.corpus.stopwords.words('english')

##--------------------------------------------------------------------------##


