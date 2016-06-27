#Uses the entire dataset to run and compute the required medians.

# This Python 3 environment comes with many helpful analytics libraries installed
# It is defined by the kaggle/python docker image: https://github.com/kaggle/docker-python
# For example, here's several helpful packages to load in 

import numpy as np # linear algebra
import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

df_train = pd.read_csv('./train.csv')
# Any results you write to the current directory are saved as output.
# In case the pair of product/client pair has a median available, we use that as the predicted value. If not, then the product median is checked. If a value is not found, then the global median is used.
df_train.columns = ['WeekNum', 'DepotId', 'ChannelId', 'RouteId', 'ClientId',\
                    'ProductId', 'SalesUnitsWeek', 'SalesPesosWeek',\
                    'ReturnsUnitsWeek', 'ReturnsPesosWeek', 'AdjDemand']

df_train = df_train.loc[df_train['WeekNum'] < 9]
print "Passed error"
# Computing the medians by grouping the entire data on ProductId and then grouping on both ProductId and the ClientId.
prod_median_tab = df_train.groupby('ProductId').agg({'AdjDemand': np.median})
prod_median_tab2 = df_train.groupby(['ProductId', 'ClientId']).agg({'AdjDemand': np.median})
global_median = np.median(df_train['AdjDemand'])
prod_median_dict2 = prod_median_tab2.to_dict()
prod_median_dict = prod_median_tab.to_dict()

def gen_output(key):
    key = tuple(key)
    try:
        val = prod_median_dict2['AdjDemand'][key]
        try:
            val = prod_median_dict['AdjDemand'][key[0]]
        except:
            val = global_median
    except:
        val = global_median
    return val
    
df_test = pd.read_csv('./train.csv')
df_test.columns = ['WeekNum', 'DepotId', 'ChannelId', 'RouteId', 'ClientId',\
                    'ProductId', 'SalesUnitsWeek', 'SalesPesosWeek',\
                    'ReturnsUnitsWeek', 'ReturnsPesosWeek', 'AdjDemand']
df_test = df_test.loc[df_test['WeekNum'] == 9]                  
#Generating the output
df_test['Demanda_uni_equil'] = df_test[['ProductId', 'ClientId']].\
                apply(lambda x:gen_output(x), axis=1)
df_submit = df_test[['Demanda_uni_equil']]
#df_submit = df_submit.set_index('id')
df_submit.to_csv('naive_product_client_median_train_week_9.csv')