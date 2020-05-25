import sys
vec_size = int(sys.argv[1])
window = int(sys.argv[2])
min_count = int(sys.argv[3])
input_file = sys.argv[4]
output_file = sys.argv[5]
log_file = sys.argv[6]

import logging
logging.basicConfig(filename=log_file)

import numpy as np
from gensim.models.doc2vec import Doc2Vec, TaggedDocument
from nltk.tokenize import word_tokenize

i = open(input_file, 'r')
data = i.readlines()
i.close()

tagged_data = [TaggedDocument(words=word_tokenize(_d.lower()),
               tags=[str(i)]) for i, _d in enumerate(data)]

max_epochs = 100
alpha = 0.025

model_dbow = Doc2Vec(vector_size=vec_size,
                window=window,
                min_count=min_count,
                alpha=alpha,
                min_alpha=alpha,
                dm=0)
model_dm = Doc2Vec(vector_size=vec_size,
                window=window,
                min_count=min_count,
                alpha=alpha,
                min_alpha=alpha,
                dm=1)
                
model_dbow.build_vocab(tagged_data)
model_dm.build_vocab(tagged_data)

for epoch in range(max_epochs):
    print('iteration {0}'.format(epoch))
    model_dbow.train(tagged_data,
                total_examples=model_dbow.corpus_count,
                epochs=model_dbow.iter)
    model_dm.train(tagged_data,
                total_examples=model_dm.corpus_count,
                epochs=model_dm.iter)
    # decrease the learning rate
    model_dbow.alpha -= 0.0002
    model_dm.alpha -= 0.0002
    # fix the learning rate, no decay
    model_dbow.min_alpha = model_dbow.alpha
    model_dm.min_alpha = model_dm.alpha

with open( output_file, 'w' ) as outfile:
    for i in range(len(model_dbow.docvecs)):
        mdbowi = model_dbow.docvecs[i].reshape((1,vec_size))
        mdmi = model_dm.docvecs[i].reshape((1,vec_size))
        mi = np.concatenate((mdbowi,mdmi)).reshape((1,2*vec_size))
        np.savetxt(outfile, mi)
