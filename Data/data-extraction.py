import metapub
import pandas as pd

from metapub import FindIt

# Read file
dat = pd.read_excel('jpai_rand1000.xlsx')

# PMIDs without PMCID
not_open_ind = pd.isna(dat['PMCID'])
pmid = dat['PMID'].loc[not_open_ind]

for column in pmid:
    src = FindIt('18381613')
    print(src.url)
