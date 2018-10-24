# Source: https://www.kaggle.com/julian3833/1-quick-start-read-csv-and-flatten-json-fields #
import os
import json
import numpy as np
import pandas as pd
from pandas.io.json import json_normalize

os.chdir('/Users/nickbruno/SYS6018/sys6018-competition-google') # set working directory

def load_df(csv_path='/Users/nickbruno/SYS6018/sys6018-competition-google/train.csv', nrows=None):
    JSON_COLUMNS = ['device', 'geoNetwork', 'totals', 'trafficSource']
    
    df = pd.read_csv(csv_path, 
                     converters={column: json.loads for column in JSON_COLUMNS}, 
                     dtype={'fullVisitorId': 'str'}, # Important!!
                     nrows=nrows)
    
    for column in JSON_COLUMNS:
        column_as_df = json_normalize(df[column])
        column_as_df.columns = [f"{column}.{subcolumn}" for subcolumn in column_as_df.columns]
        df = df.drop(column, axis=1).merge(column_as_df, right_index=True, left_index=True)
    print(f"Loaded {os.path.basename(csv_path)}. Shape: {df.shape}")
    return df

print(os.listdir("/Users/nickbruno/SYS6018/sys6018-competition-google"))
	# should read "Loaded train.csv. Shape: (903653, 55)
	# 			   Loaded test.csv. Shape: (804684, 53)"

# Load in the new dataframes with each row correctly
df_train = load_df()
df_test = load_df("/Users/nickbruno/SYS6018/sys6018-competition-google/test.csv")

# Create new datasets with each column expanded
df_train.to_csv("train-flattened.csv", index=False)
df_test.to_csv("test-flattened.csv", index=False)