from selenium import webdriver
from selenium.webdriver.common.by import By
from bs4 import BeautifulSoup
import pandas as pd
import json
import string
import re
import bs4
import numpy as np
import os

browser = webdriver.Firefox()
browser.get('http://streeteasy.com/for-rent/manhattan/price:-10000')
pattern = re.compile('bed|ft|bath')
#f = open('streeteasy.csv', 'ab')
apt_dict = {}

for i in range(100):

    #Create a BeautifulSoup object of the browser.page_source code object
    soup = BeautifulSoup(browser.page_source)
    #On each age, find all the individual apartments
    apt_list = soup.findAll("div", {"class": "item" or "item featured" or "item selected"})
    #This loop is the heart of the web-scraping. For each apt in the apt_list, search the branch
    #and find the necessary text/variables

    for apt in apt_list:
        try:
            apt_dict[apt['data-id']] = {'Coordinate': apt['se:map:point'],
                                        'Price': apt.find_all('span', {'class': 'price'})[0].text,
                                        'Neighborhood':
                                            apt.find_all('div', {'class': 'details_info'})[1].text.split(' in ')[1].strip(),
                                        'Address': apt.find_all('a', href=True)[1].text}
            for detail in apt.find_all('div', {'class': 'details_info'})[0].strings:
                if re.search(pattern, detail):
                    apt_dict[apt['data-id']][re.search(pattern, detail).group(0)] = detail
        except:
            pass
    browser.find_element_by_class_name('next').click()

#print apt_dict.values()
df = pd.DataFrame(apt_dict.values(), index=apt_dict.keys())
#print df
#Convert the price column to type float
df['Price'] = df['Price'].map(lambda x: x.replace(',', ""))
df['Price'] = df['Price'].map(lambda x: x.replace('$', ""))
df['Price'] = df['Price'].astype(float)
#
# Recreate the bed, bath & sq ft column
df['bed_num'] = df['bed']  # Copy the 'bed' column
df.loc[df['bed_num'].isnull(), 'bed_num'] = '1'  # impute the value '1' for each NaN
df['bed'] = df['bed'].fillna('studio')
df['bed_num'] = df['bed_num'].map(lambda x: float(string.replace(x.split()[0], ',', '')) \
                               if isinstance(x, bs4.element.NavigableString) else 1.0)  # Delete everything after the space
df['bed_num'] = df['bed_num'].astype(float)  # Convert to float
# Replicate the same exact steps for the number of bathrooms
df['bath_num'] = df['bath']  # Copy the 'bed' column
df['bath_num'] = df['bath_num'].map(lambda x: float(string.replace(x.split()[0], ',', '')) \
                                if isinstance(x, bs4.element.NavigableString) else np.nan)  # Delete everything after the space
df['bath_num'] = df['bath_num'].astype(float)  # Convert to float

# Replicate for square footage
df['sq_feet'] = df['ft']  # Copy the 'bed' column
df['sq_feet'] = df['sq_feet'].map(lambda x: float(string.replace(x.split()[0], ',', '')) \
                             if isinstance(x, bs4.element.NavigableString) else np.nan)
#print df
df1 = df.drop('ft', axis = 1)
#print df1
path = r'/Users/zacharyescalante/Desktop/'
df1.to_csv(os.path.join(path,r'test.csv'))







