#!/usr/bin/env python3

import spacy, benepar
import pandas as pd

nlp = spacy.load("en_core_web_md")

if spacy.__version__.startswith('2'):
    nlp.add_pipe(benepar.BeneparComponent("benepar_en3"))
else:
    nlp.add_pipe("benepar", config={"model": "benepar_en3"})

utts_df = pd.read_csv("test.csv", low_memory = False)

pos_tags = []

for i in range(len(utts_df)):
    gloss = utts_df.values[i][2]
    doc = nlp(gloss)
    sent = list(doc.sents)[0]
    pos = sent._.parse_string
    pos_tags.append(pos)
    
utts_df["parsed_gloss"] = pos_tags
utts_df.to_csv('output.csv')  