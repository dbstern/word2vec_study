import sys
import pandas as pd 
import nltk
import nltk.corpus

db = sys.argv[1]
output_path = sys.argv[2]
output_file = sys.argv[3]

nltk.data.path.append(output_path)
nltk.download(db, download_dir=output_path)

data = getattr(nltk.corpus, db)
dd = pd.DataFrame(index=data.fileids())
dd["txt"] = [data.raw(id).encode('utf-8').replace('\n', ' ').replace('"', '') for id in dd.index]

labels = pd.DataFrame(0, index=data.fileids(), columns=data.categories())
for label in data.categories():
  labels.loc[data.fileids(label),label] = 1

dd = pd.concat([dd,labels],axis=1)

dd = dd.index.to_series().str.split("/", expand=True).rename(columns={0: 'train', 1: 'idx'}).join(dd)
dd["train"] = dd["train"].isin(["training"])
dd.to_csv(output_file, index=None, sep=' ')

