# -*- coding: utf-8 -*-
"""Beautifultools  module.

Beautifultools is a module containing all the functions related to a
scraping project carried out within Siemens Italy Digital Industries.

The module contains several functions aiming at implementing the 
pipeline pinned down by Kelly et al. paper "Predicting Returns with 
Text Data", plus some functions for data gathering and processing.

Is possible to test the algorithm with Dante's Divine Comedy dataset
provided by default.

Example
-------
When in a script or a notebook:

    import beautifultools as bt


Notes
-----
The module uses extensively several python modules: sklearn, pandas, 
nltk, numpy, scipy, and many more... 


Author
------
Marco Repetto: <marco.repetto@siemens.com>

Git-hub
-------
https://github.com/mrepetto94/sentiment_modelling
"""

import statistics
import re
import string
import sys
import os
import nltk
import requests

import numpy as np
import pandas as pd
import plotly.express as px
import matplotlib.pyplot as plt
import nltk.stem as stem


from wordcloud import WordCloud, ImageColorGenerator
from PIL import Image
from io import StringIO
from sklearn.preprocessing import normalize
from requests.packages.urllib3.exceptions import InsecureRequestWarning
from bs4 import BeautifulSoup
from nltk.corpus import stopwords
from sklearn.base import BaseEstimator
from sklearn.utils.multiclass import unique_labels
from sklearn.linear_model import LinearRegression
from sklearn.feature_extraction.text import CountVectorizer
from scipy.optimize import fminbound


class HiddenPrints:
    """Hide printing from the system.

    It is used only in marginal screening since scipy prints when a
    solution to a linear regression is exactly 0.
    """

    def __enter__(self):
        self._original_stdout = sys.stdout
        sys.stdout = open(os.devnull, "w")

    def __exit__(self, exc_type, exc_val, exc_tb):
        sys.stdout.close()
        sys.stdout = self._original_stdout


def verses_cleaner(verse):
    """Clean a string/verse.
    
    The function returns a string/verse processed for topic modelling.
    The function strips punctation and stopwords moreover lowers each 
    character.

    Parameters
    ----------
    verse: string
           String of a verse

    Returns
    -------
    verse : string
            Returns a cleaned verse
    """

    # Make string lowercase
    verse = verse.lower()

    # Punctation and other amenities removal
    verse = verse.translate(
        str.maketrans({i: "" for i in string.punctuation + "”" + "“" + "»" + "«" + "‘"})
    )
    verse = verse.translate(str.maketrans({i: " " for i in "’" + "—"}))

    # Remove italian stopwords
    verse = " ".join(
        [
            w
            for w in nltk.word_tokenize(verse)
            if not w in stopwords.words("italian")
            if len(w) > 2
        ]
    )

    return verse


def webpage_text_tokenizer(
    url, body=True, blackL=[""], verbose=False, sep=" ", **kwargs
):
    """Scrape the text of a web-page, either body or just keywords.
    
    The function request and parse the HTML of a given url.
    Returns a string of words separated by sep, normalized and without
    blacklisted words, if provided.
    
    
    Parameters
    ----------
    url : string
        a string pointing to the web-page
    
    body : boolean, default= True
        a boolean stating whether the whole body should be parsed 
    
    blackL : list, default=['']
        a list containing the blacklisted words 
    
    verbose : boolean, default= False
        a boolean stating whether the function should print the 
        progresses
    
    sep : string, default=  ' '
        the separator for the list of keywords
    

    Returns
    -------
    A string containing words separated by sep
    """

    if verbose:
        print("Checking: " + url)

    # Try the extraction, if fail or timeout, return an empty list
    try:
        # Get html of the url
        html = requests.get(url, timeout=10, **kwargs)
        soup = BeautifulSoup(html.text, "html.parser")

        # Get webp-age Title
        try:
            title = soup.find(name="title").get_text()
        except AttributeError:
            title = ""

        # Get web-page og Title
        try:
            ogTitle = soup.find(name="meta", attrs={"property": "og:title"}).get(
                "content"
            )
        except AttributeError:
            ogTitle = ""

        # Get web-page Keywords
        try:
            keywords = soup.find(name="meta", attrs={"name": "keywords"}).get("content")
        except AttributeError:
            keywords = ""

        # Get web-page Description
        try:
            description = soup.find(name="meta", attrs={"name": "description"}).get(
                "content"
            )
        except AttributeError:
            description = ""

        # Get web-page og Description
        try:
            ogDescription = soup.find(
                name="meta", attrs={"property": "og:description"}
            ).get("content")
        except AttributeError:
            ogDescription = ""

        # If flag body is true then extract all the text in the web-page
        if body:
            # Get web-page Body
            try:
                body = soup.get_text()
            except AttributeError:
                body = ""
        else:
            body = ""

        # Wrap and tokenize all the information
        mainCorpora = (
            title
            + " "
            + ogTitle
            + " "
            + keywords
            + " "
            + description
            + " "
            + ogDescription
            + " "
            + body
        )
        token = nltk.word_tokenize(mainCorpora)

        # Make all the tokens lower then trim non words as well as
        # stopwords, Italian and English

        tokenFiltered = [
            i.lower()
            for i in token
            if not re.search("[" + string.punctuation + "0-9" + "]", i)
            and len(i) >= 3
            and i.lower() not in stopwords.words("english")
            and i.lower() not in stopwords.words("italian")
            and i.lower() not in blackL
        ]

        if verbose:
            print("Number of words found: " + str(len(tokenFiltered)))
        return sep.join(tokenFiltered)

    except:
        if verbose:
            print("Internalerror")
        return ""


def urlize_string(url, warning=True, verbose=False, **kwargs):
    """Standardize url before requesting.

    The function normalize the url provided and test whether it can be
    reached with requests. Returns a string with the corrected url.
    
    
    Parameters
    ----------
    url : string
        a string pointing to the web-page
    
    warning : boolean
        a boolean stating whether the function should print the warning

    
    Returns
    -------
    A string with the corrected url 
    """

    # Check whether to shutdown warnings
    if not (warning):
        requests.packages.urllib3.disable_warnings(InsecureRequestWarning)

    url = url.split(sep=";")

    for i in url:
        if verbose:
            print("Processing: ", i)
        try:
            requests.get(i, timeout=10, **kwargs)
            return i
        except requests.exceptions.ConnectionError:
            if verbose:
                print("Connection error with: ", i)
            return np.nan
        except requests.exceptions.ReadTimeout:
            if verbose:
                print("Read timeout error with: ", i)
            return np.nan
        except requests.exceptions.TooManyRedirects:
            if verbose:
                print("Too many redirects error with: ", i)
            return np.nan
        except requests.exceptions.InvalidSchema:
            if verbose:
                print("Invalid schema error with: ", i)
            return np.nan
        except requests.exceptions.MissingSchema:
            newUrl = "HTTP://" + i
            try:
                requests.get(newUrl, timeout=10, **kwargs)
                return newUrl
            except requests.exceptions.ConnectionError:
                if verbose:
                    print("Connection error with: ", newUrl)
                return np.nan
            except requests.exceptions.ReadTimeout:
                if verbose:
                    print("Read timeout error with: ", newUrl)
                return np.nan
            except requests.exceptions.TooManyRedirects:
                if verbose:
                    print("Too many redirects error with: ", newUrl)
                return np.nan
            except requests.exceptions.InvalidSchema:
                if verbose:
                    print("Invalid schema error with: ", newUrl)


def word_cloud(corpora, maskPath, **kwargs):
    """Plot wordcloud.
 
    The function plot a wordcloud provided text and a mask in 
    rasterized formats.
    
    
    Parameters
    ----------
    corpora : list
        a list containing strings with space as separator
    
    maskPath : string
        path to a raster image


    Returns
    -------
    A matplotlib object
    """

    mask = np.array(Image.open(maskPath))

    # create coloring from image
    imageColors = ImageColorGenerator(mask)
    fig, axs = plt.subplots(1, len(corpora), **kwargs)

    for i in range(len(corpora)):

        wc = WordCloud(
            background_color="white",
            mask=mask,
            random_state=1994,
            repeat=False,
            collocations=False,
        )

        # Generate word cloud from text
        wc.generate(corpora[i])

        # Overlap image and word-cloud and remove axes
        axs[i].imshow(wc.recolor(color_func=imageColors), interpolation="bilinear")
        axs[i].imshow(mask, cmap=plt.cm.gray, interpolation="bilinear", alpha=0.3)

        axs[i].set_axis_off()

    return plt.show()


def plot_word_frequencies(corpus, **kwargs):
    """Plot the histogram of the words contained into a corpus.
    
    
    Parameters
    ----------
    corpus : a string containing words separated by spaces
    """

    corpus = nltk.tokenize.word_tokenize(corpus)
    corpus = pd.DataFrame({"Word": corpus})

    return px.histogram(corpus, x="Word", **kwargs).update_xaxes(
        categoryorder="total descending"
    )


def drop_duplicates(row, sep=" "):
    """Drop duplicates in a string. 

    The function provided a string of words and a separator returns a
    string containing only unique words separated by the same 
    separator.
    
    
    Parameters
    ----------
    row : string 
        a string containing words separated by some separator.
    
    sep : string, default=  ' '
        the separator of words


    Returns
    -------
    A string of unique words separated by sep
    """

    words = row.split(sep)
    return sep.join(np.unique(words).tolist())


def drop_non_sentiment_words(row, sentiment_words, sep=" "):
    """Drop non sentiment charged words in a string. 
    
    The function returns the intersection of row and sentiment words, 
    leaving row with only sentiment charged words. It's possible to 
    provide a separator.
    
    
    Parameters
    ----------
    row : string 
        a string containing words separated by some separator
    
    sentiment_words : list
        a list containing the sentiment charged words
    
    sep : string, default=  ' '
        the separator of words 


    Returns
    -------
    A string with only sentiment charged words separated by sep
    """

    words = row.split(sep)
    return sep.join([i for i in words if i in sentiment_words])


def stem_words(string, sep=" "):
    """Stem words in a string. 
    
    The function tries first to stem the word in Italian if nothing 
    happens then switch to English.
    
    Parameters
    ----------
    string : string
        a string containing words separated by some separator

    sep : string 
        a separator (default ' ')


    Returns
    -------
    A string stemmed
    """

    stemmerIta = stem.SnowballStemmer("italian")
    stemmerEng = stem.SnowballStemmer("english")

    string = string.split(sep)
    string = [
        stemmerIta.stem(i) if stemmerIta.stem(i) != i else stemmerEng.stem(i)
        for i in string
    ]

    return sep.join(string)


class SSESTM(BaseEstimator):
    """ Supervised Sentiment Extraction Screening Topic Modelling.

    The estimator implements the Supervised Sentiment Extraction via 
    Screening and Topic Modelling (aka SSESTM procedure) as posed by 
    Kelly et al. 2019 article 'Predicting Returns with Text Data'. 
    
    The procedure consist of:

    1. Marginal screening: fitting a linear regression for each 
    element of a document word matrix;
    2. Topic modelling: fitting a linear regression on the screened 
    words frequencies passing ranked labels;
    3. Prediction: by maximizing the log likelihood of a multinomial 
    distribution with penalty.
 
    The parameters are controls on the coefficients, upper and lower 
    bound (alpha_plus, alpha_minus), on the frequency of the words and
    on the penalty applied in prediction.   

    Parameters
    ----------
    alpha_plus : float, default= 0.5
        a parameter for trimming the coefficients downwards

    alpha_minus : float, default= 0.5
        a parameter for trimming the coefficients upwards.

    kappa : float, default= 1
        lower limit on words frequency.

    l : float, default= 0
        maximum likelihood penalty term.

    """

    def __init__(self, alpha_plus=0.5, alpha_minus=0.5, kappa=1, l=0.0):

        self.alpha_plus = alpha_plus
        self.alpha_minus = alpha_minus
        self.kappa = kappa
        self.l = l

    def fit(self, X, y):
        """Implements the first two steps, namely:
        
        1. Marginal Screening;
        2. Topic Modelling.
.
        Parameters
        ----------
        X : pandas series 
            a series containing the keywords

        y : pandas series
            a series containing labels 

        Returns
        -------
        self : object
            returns self
        """

        # MARGINAL SCREENING #
        # Init the document term matrix
        cvStep1 = CountVectorizer(binary=True)

        cvX = cvStep1.fit_transform(X)

        # Binarize the order to label whether an observation belongs
        # to a client 1 or 0 a prospect
        y = y.copy()
        ybin = y
        ybin[ybin > 0] = 1
        ybin[ybin <= 0] = 0

        wordfreq = sum(cvX)
        wordfreq = wordfreq.toarray()[0, :]

        coef = np.array([])

        # Suppress print to avoid scipy returning "The exact solution
        # is x=0"
        with HiddenPrints():

            # Loop for every column in the matrix
            for i in cvX.T:
                coefficient = LinearRegression(fit_intercept=False).fit(i.T, ybin).coef_
                coef = np.concatenate((coef, coefficient))

        # Filter the coefficients based on the parameters
        coef[(coef < self.alpha_plus) * (coef > self.alpha_minus)] = np.nan
        coef[wordfreq < self.kappa] = np.nan

        self.marginal_screening = pd.DataFrame(
            ({"term": cvStep1.get_feature_names(), "score": coef})
        ).dropna()

        # TOPIC MODELLING #
        # Create a column with only sentiment charged keywords
        X = X.apply(
            drop_non_sentiment_words,
            sentiment_words=self.marginal_screening["term"].to_list(),
        )

        # Remove entries without sentiment charged words
        y = y[X != ""]
        X = X[X != ""]

        # Define p-hat as the normalized rank
        y = y.rank(pct=True)

        # Initialize weight matrix
        W = np.matrix([y, 1 - y]).T

        # Compute count of sentiment charged words for each web-page
        s = X.apply(lambda row: len(row.split(" ")))

        # Create document keyword matrix
        cvStep2 = CountVectorizer(vocabulary=self.marginal_screening["term"].to_list())
        dS = cvStep2.fit_transform(X)

        # Get sentiment word frequency per document
        tildeD = dS / s[:, None]

        # Fit the linear regression to estimate O matrix
        O = LinearRegression(fit_intercept=False).fit(X=W, y=tildeD).coef_

        # Set negative coefficients to 0
        O[O <= 0] = 0

        # Normalize result to l1
        normalize(O, norm="l1", axis=0, copy=False, return_norm=False)

        self.topic_coefficients = O

        return self

    def predict(self, X):
        """The function implements scoring on new set of keywords as 
        posed by Kelly et al.

            Parameters
            ----------
            X : pandas series
                a series containing the keywords.

            Returns
            -------
            y : array
                an array of sentiments.


        """

        def mle(x, s, dS, O):
            """ The function implements the log-likelihood of a 
            multinomial with penalty as posed by Kelly et al.


            Parameters
            ----------
            x : float
                the sentiment score

            s : int
                the number of sentiment charged words per web-page

            dS : pandas series
                a series containing sentiment charged words frequencies
            
            O : array-like
                Matrix containing word positiveness or negativeness.
                

            Returns
            -------
            v : float
                Return the log-likelihood value given x
            """
            return -(
                (float(s) ** (-1))
                * np.sum(
                    np.multiply(
                        dS.toarray().T,
                        (np.log(0.00000001 + x * O[:, 0] + (1 - x) * O[:, 1]))[:, None],
                    )
                    + self.l * np.log(x * (1 - x))
                )
            )

        # Create a column with only sentiment charged keywords
        X = X.apply(
            drop_non_sentiment_words,
            sentiment_words=self.marginal_screening["term"].to_list(),
        )

        # Compute count of sentiment charged words for the web-page
        s = X.apply(lambda row: len(row.split(" ")))

        # Create document keyword matrix
        cvStep3 = CountVectorizer(vocabulary=self.marginal_screening["term"].to_list())
        dS = cvStep3.fit_transform(X)

        # Get sentiment word frequency per document
        D = dS / s[:, None]

        p = []
        for i in range(len(s)):
            p.append(
                fminbound(
                    mle,
                    x1=0.001,
                    x2=0.999,
                    args=(s.iloc[i], dS[i, :], self.topic_coefficients),
                )
            )

        # Maximize the log-likelihood
        self.y = np.array(p)

        return self.y


class testData:
    """ The class contains (at the time) one dataset, namely Dantes's 
    Divine Comedy gently offered by druntime repository.

    https://raw.githubusercontent.com/dlang/druntime/master/benchmark/extra-files/dante.txt
    """

    def dante(path="", granularity="canto", clean=True):
        """The function returns a pandas containing Dante's Divine Comedy.


        Parameters
        ----------
        path : string
            a string with the local path to the txt of the Comedy

        granularity : string, default= 'canto'
            a string containing the way in which the dataframe should 
            be indexed
        
        clean : boolean, default= True
            specify whether the text should be cleaned or not
       

        Returns
        -------
        dante : pandas DataFrame
            returns dataframe containing Dante's Divine Comedy
        """

        if path == "":
            # Get the repository containing the relevant text
            url = "https://raw.githubusercontent.com/dlang/druntime/master/benchmark/extra-files/dante.txt"

            # Request the raw .txt and decode it
            response = requests.get(url)
            dante = response.text
        else:
            f = open(path)
            dante = f.read()

        # String processing starts here
        # Make string lowercase
        dante = dante.lower()

        # Remove headings
        dante = dante.replace(
            "la divina commedia\ndi dante alighieri\n\n\n\n\n\ninferno\n\n\n\n\n", ""
        )
        dante = dante.replace("\n\n\n\n\n\npurgatorio\n", "")
        dante = dante.replace("\n\n\n\n\n\nparadiso\n", "")

        # Punctation and other amenities removal
        dante = dante.translate(
            str.maketrans(
                {i: "" for i in string.punctuation + "”" + "“" + "»" + "«" + "‘"}
            )
        )
        dante = dante.translate(str.maketrans({i: " " for i in "’" + "—"}))

        # Make string  csv-like
        if granularity == "verso":
            dante = (
                dante.replace("\n\n\n", ",")
                .replace(",\n", ";")
                .replace("\n\n", "\n")
                .replace("\n", ":")
                .replace(";", "\n")
                .replace(" · ", ",")
            )
        elif granularity == "terzina":
            dante = (
                dante.replace("\n\n\n", ",")
                .replace(",\n", ";")
                .replace("\n\n", ":")
                .replace("\n", " ")
                .replace(";", "\n")
                .replace(" · ", ",")
            )
        else:
            dante = (
                dante.replace("\n\n\n", ",")
                .replace(",\n", ";")
                .replace("\n\n", " ")
                .replace("\n", " ")
                .replace(";", "\n")
                .replace(" · ", ",")
            )

        # Create the dataframe
        dante = pd.read_csv(
            StringIO(dante),
            header=None,
            names=["label", "canto", "content"],
            index_col=[0, 1],
        )

        if granularity != "canto":
            # Augment granulairity of the df splitting verses
            dante = dante.content.str.split(":", expand=True)

            # Rename splitted columns to apply wide_to_long
            dante = dante.rename(
                {i: "content" + str(i) for i in dante.columns if str(i).isnumeric()},
                axis=1,
            )

            # Apply wide to long need to reset index for reindexing
            dante = dante.reset_index()
            dante = pd.wide_to_long(
                dante, stubnames="content", i=["label", "canto"], j=granularity
            )

            # drop na and empty strings
            dante = dante[dante["content"].astype(bool)]

        if clean:
            # Remove italian stopwords
            dante.loc[:, "content"] = dante.loc[:, "content"].apply(
                lambda x: " ".join(
                    [
                        w
                        for w in nltk.word_tokenize(x)
                        if not w in stopwords.words("italian") and len(w) > 2
                    ]
                )
            )

        return dante
