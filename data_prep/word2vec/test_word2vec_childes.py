from __future__ import absolute_import, division, print_function, unicode_literals
import io
import os
from gensim import utils
import gensim.models
import gensim.models.word2vec
from gensim.test.utils import datapath
import logging
logging.basicConfig(format='%(asctime)s : %(levelname)s : %(message)s', level=logging.INFO)
from sklearn.decomposition import IncrementalPCA    
from sklearn.manifold import TSNE                   
import numpy as np
from numpy import mean
import csv
import itertools

model = gensim.models.Word2Vec.load("childes_word2vec.model")

words = ['doggy', 'dog', 'kitty', 'cat', 'tummy', 'stomach',
         'daddy', 'dad', 'mommy', 'mom', 'bunny', 'rabbit',
         'duckie', 'duck', 'blankie', 'blanket', 'froggy', 'frog',
         'potty', 'bathroom', 'night_night', 'goodnight',
         'dolly', 'doll', 'piggy', 'pig', 'birdie', 'bird']

pairs = [
  ('doggy', 'dog'),
  ('kitty', 'cat'),
  ('tummy', 'stomach'),
  ('daddy', 'dad'),
  ('mommy', 'mom'),
  ('bunny', 'rabbit'),
  ('duckie', 'duck'),
  ('blankie', 'blanket'),
  ('froggy', 'frog'),
  ('potty', 'bathroom'),
  ('night_night', 'goodnight'),
  ('dolly', 'doll'),
  #('horsey', 'horse'),
  ('piggy', 'pig'),
  ('birdie', 'bird'),
]

for w1, w2 in pairs:
  print('%r\t%r\t%.2f' % (w1, w2, model.wv.similarity(w1, w2)))

# get chance-level similarity by scrambling pairs
random_pairs = [*itertools.permutations(words,2)]

random_scores = []

for w1, w2 in random_pairs:
  cosine_sim = model.wv.similarity(w1, w2)
  random_scores.append(cosine_sim)

random_mean = mean(random_scores)
print("Chance-level similarity is ", random_mean)