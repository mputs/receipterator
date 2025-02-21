import pandas as pd
import numpy as np

#dat = pd.read_csv('data_adj.tsv', sep = '\t')
dat6 = pd.read_csv('data_adj6.tsv', sep = '\t')


dat6b = dat6[dat6['text'].notna()]

# dat6.to_csv()
# dat6.to_parquet()



# Group by 'line_num' and compute required aggregations
dat6c = dat6b.groupby('line_num', as_index=False).agg(
    left_max=('left', 'max'),  # Max of 'left'
    id=('left', lambda x: x.idxmax()),  # Index of max 'left'
)




# Extract 'text' using the computed 'id'
dat6c['text'] = dat6b.loc[dat6c['id'], 'text'].values

# Compute the median of 'left_max'
left_med = np.median(l_grouped['left_max'])

# Add 'last_num' column
l_grouped['last_num'] = (abs(l_grouped['left_max'] - left_med) < 50)

print(l_grouped)
