from __future__ import absolute_import, division, print_function, unicode_literals
import io
import os
from gensim import utils
import gensim.models
import gensim.models.word2vec
from gensim.test.utils import datapath
import logging
logging.basicConfig(format='%(asctime)s : %(levelname)s : %(message)s', level=logging.INFO)

exclude = set('@#1234567890()*')

class corpus(object):
  def __iter__(self):
    corpus_path = "childes_utterances_older.txt"
    for line in open(corpus_path):
      line = line.lower().replace('.', ' . ').replace('/', ' ').replace(',',' ').replace('-',' ')
      for word in line:
        if any ((c in exclude) for c in word):
          line.replace(word, '')
      yield utils.simple_preprocess(line)

sentences = corpus()
model = gensim.models.Word2Vec(sentences=sentences)

model.save("childes_word2vec_older.model")
