#!/usr/bin/env python3
import spacy, benepar
import pandas as pd

target_words = ["doggy", "dog", 
                "kitty", "cat", 
                "tummy", "stomach", 
                "daddy", "dad", 
                "mommy", "mom", 
                "bunny", "rabbit",
                "duckie", "duck", 
                "blankie", "blanket", 
                "froggy", "frog", 
                "potty", "bathroom", 
                "night-night", "goodnight", 
                "dolly", "doll", 
                "horsey", "horse", 
                "piggy", "pig", 
                "birdie", "bird"]

nlp = spacy.load("en_core_web_md")

if spacy.__version__.startswith('2'):
    nlp.add_pipe(benepar.BeneparComponent("benepar_en3"))
else:
    nlp.add_pipe("benepar", config={"model": "benepar_en3"})


# CHILDES
for t in target_words:
    utts_df = pd.read_csv("../../data/childes-byword/" + t + ".csv", 
    low_memory = False, encoding='latin-1')
    pos_tags = []

    for i in range(len(utts_df)):
        gloss = str(utts_df.values[i][2])
        doc = nlp(gloss)
        sent = list(doc.sents)[0]
        pos = sent._.parse_string
        pos_tags.append(pos)
    
    utts_df["parsed_gloss"] = pos_tags
    utts_df.to_csv(t + "-parsed.csv")

# LDP
for t in target_words:
    utts_df = pd.read_csv("~/Desktop/secure/ldp-byword/" + t + ".csv", 
    low_memory = False, encoding='latin-1')
    pos_tags = []

    for i in range(len(utts_df)):
        gloss = str(utts_df.values[i][2])
        doc = nlp(gloss)
        sent = list(doc.sents)[0]
        pos = sent._.parse_string
        pos_tags.append(pos)
    
    utts_df["parsed_gloss"] = pos_tags
    utts_df.to_csv("~/Desktop/secure/ldp-parsed/" + t + "-parsed.csv")