
# coding: utf-8

# ## Sentiment Analysis_Helperfunctions

# #### Load the all packages

# In[1]:


from __future__ import print_function

get_ipython().magic('matplotlib inline')

import tensorflow
import keras
import nltk
import os
import re
import itertools
import wordcloud
import csv
import testfixtures
import statsmodels
import locale
import glob
import os.path
import requests
import tarfile
import sys
import codecs
import smart_open

import numpy as np
import _pickle as pickle
import pandas as pd

from IPython.display import SVG
from IPython.display import Image

from keras.datasets import imdb
from keras.preprocessing import sequence
from keras.models import Sequential
from keras.layers import Dense, Dropout, Activation
from keras.layers import Embedding, SpatialDropout1D
from keras.layers import LSTM
from keras.layers import Conv1D, GlobalMaxPooling1D
from keras.layers import Flatten
from keras.utils import plot_model
from keras.utils.vis_utils import model_to_dot
from keras.utils.data_utils import get_file
from imp import reload
from PIL import Image
from itertools import chain
from matplotlib import pyplot as plt
from numpy.random import normal
from wordcloud import WordCloud, STOPWORDS


# ___
# #### Save the dataset to a CSV file

# In[3]:


# save the dataset to a .csv file 

# create an index/word mapping, get full dataset copied from Keras and separate features from labels
index = imdb.get_word_index()
index2word = {v: k for k, v in index.items()}
path = get_file('imdb_full.pkl',
                origin='https://s3.amazonaws.com/text-datasets/imdb_full.pkl',
                md5_hash='d091312047c43cf9e4e38fef92437263')

# split into train and test 
f = open(path, 'rb')
(x_train, labels_train), (x_test, labels_test) = pickle.load(f)

# move the train data to csv
with open('train.csv', 'w', encoding='utf-8') as f:
    writer = csv.writer(f)
    for i in range(0, len(x_train)):
        label = labels_train[i]
        review = ' '.join([index2word[o] for o in x_train[i]])
        writer.writerow([review, label])
        
# move the test data to csv
with open('test.csv', 'w', encoding='utf-8') as f:
    writer = csv.writer(f)
    for i in range(0, len(x_test)):
        label = labels_test[i]
        review = ' '.join([index2word[o] for o in x_test[i]])
        writer.writerow([review, label])


# In[4]:


train_data = pd.read_csv('train.csv', header=None)
test_data = pd.read_csv('test.csv', header=None)


# In[ ]:


import io
import pandas as pd
import numpy as np

with io.open('aclImdb/train-pos.txt', encoding='utf-8') as f:
    train_pos = pd.DataFrame({'review': list(f)})    
with io.open('aclImdb/train-neg.txt', encoding='utf-8') as f:
    train_neg = pd.DataFrame({'review': list(f)}) 
train_reviews = pd.concat([train_neg, train_pos], ignore_index=True)

with io.open('aclImdb/test-pos.txt', encoding='utf-8') as f:
    test_pos = pd.DataFrame({'review': list(f)})
with io.open('aclImdb/test-neg.txt', encoding='utf-8') as f:
    test_neg = pd.DataFrame({'review': list(f)})    
test_reviews = pd.concat([test_neg, test_pos], ignore_index=True)
  
X_train = train_reviews['review']
X_test = test_reviews['review']

y_train = np.append(np.zeros(12500), np.ones(12500))
y_test = np.append(np.zeros(12500), np.ones(12500))


# #### Logistic regression to classify which features are important
# * ##### Results are shown in the Preprocessing-File

# In[6]:


from nltk.corpus import stopwords
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.linear_model import LogisticRegression

stopwords_nltk = set(stopwords.words("english"))
relevant_words = set(['not', 'nor', 'no', 'wasn', 'ain', 'aren', 'very', 'only', 'but', 'don', 'isn', 'weren'])
filtered_stopwords = list(stopwords_nltk.difference(relevant_words))
vectorizer = CountVectorizer(stop_words =  filtered_stopwords, max_features = 10000, ngram_range = (1,2))
X_train_features = vectorizer.fit_transform(X_train)
X_test_features = vectorizer.transform(X_test)

logistic_model = LogisticRegression(C=0.03) 
logistic_model.fit(X_train_features, y_train)

