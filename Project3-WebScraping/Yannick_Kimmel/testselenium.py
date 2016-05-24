from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import TimeoutException
import scrapy
from sys import argv
import time
import csv
import lxml.html
import unicodedata
import time
import numpy as np
import re

def allyears(br):
	years = range(1997, 2016)
	yearurls = range(14486,14497)+[15835]+[16062]+[16322]+[17478]+[17479]+[17480]+[17481]+[17851]
	for i, year in enumerate(years):
		browser.get('https://www.allrecipes.com/recipes/'+str(yearurls[i])) 
		top20list(browser, str(year))

#http://selenium-python.readthedocs.io/locating-elements.html #function to retrieve viewingactivity list items
def top20list(br, year):
    html_list = br.find_element_by_id("grid")
    urls = html_list.find_elements(By.CLASS_NAME, "favorite")
    id = []
    for i, e in enumerate(urls):
    	id.append(e.get_attribute('data-id'))	
    	urls[i] = 'https://allrecipes.com/recipe/' + str(id[i])
    	# counter +=
    urls = np.unique(urls)
    id = np.unique(id)

    for i, url in enumerate(urls):
    	br.get(url)
    	time.sleep(3)
    	scrape_recipe(br, year, id[i])


def scrape_recipe(br, year, idnumber):
	# fill in code for recipe page
	try:
		rtitle = br.find_element_by_tag_name('h1').text
	except:
		rtitle = 'NA'
	print idnumber
	print rtitle
	
	try:
		br.find_element_by_xpath('//div[contains(@class,"ui-dialog") and @aria-describedby="dialogContent2"]//button[@title="Close"]').click()
	except:
		print 'no popup'

	try:
		starrating = br.find_element_by_class_name('rating-stars').get_attribute('data-ratingstars')
		
	except:
		starrating = 'NA'
		

	try:
		madeitcount = br.find_element_by_class_name('made-it-count').text
		
	except:
		madeitcount = 'NA'

	try:
		reviewcount = br.find_element_by_class_name('review-count').text
		reviewcount = str(re.findall('(\w+) reviews', reviewcount)[0])
		
	except:
		reviewcount = 'NA'
		

	try:
		calcount = br.find_element_by_class_name('calorie-count').text
		calcount = str(re.findall('(\w+) cals', calcount)[0])
		
	except:
		calcount = 'NA'
		
	
	try:
		prepTime = br.find_element_by_xpath('//time[@itemprop = "prepTime"]').get_attribute('datetime')
		prepTime = str(re.findall('PT(\w+)', prepTime)[0])
		
	except: 
		prepTime = 'NA'
		


	try:
		cookTime = br.find_element_by_xpath('//time[@itemprop = "cookTime"]').get_attribute('datetime')
		cookTime = str(re.findall('PT(\w+)', cookTime)[0])
		
	except:
		cookTime = 'NA'
		


	try:
		totalTime = br.find_element_by_xpath('//time[@itemprop = "totalTime"]').get_attribute('datetime')
		totalTime = str(re.findall('PT(\w+)', totalTime)[0])
		
	except:
		totalTime = 'NA'
		

	ingred = br.find_elements_by_class_name("checkList__item")

	ingredients = []
	for x in np.arange(len(ingred)-1):
		#if (str(ingred[x].text) == '')

		ingredients.append(str(ingred[x].text.encode('ascii', 'ignore')))
	
	print 'testoutput: '
	recoutput = year+'\t'+idnumber+'\t'+rtitle+'\t'+starrating+'\t'+madeitcount+'\t'+reviewcount+'\t'+calcount+'\t'+prepTime+'\t'+\
	cookTime+'\t'+totalTime+'\n'
	
	print recoutput
	listingr = []

	for ingr in ingredients:
		listingr.append(year+'\t'+idnumber+'\t'+ingr)

	ingroutput = '\n'.join(listingr)
	ingroutput = ingroutput+'\n'
	print ingroutput
	#cloeses automatically, append data to csv then open as panda data frame 
	with open('recipes.txt', 'a') as a, open('ingredients.txt', 'a') as b:
		a.write(recoutput.encode('ascii', 'ignore')); b.write(ingroutput.encode('ascii', 'ignore'))

    	

#opens browser
if __name__ == '__main__':

	browser = webdriver.Firefox() #opens Firefox browser
	allyears(browser) 
 
	
	#after2001 I changed ingredients to include tab before year 1997-2007 14486-14496
  	#2008 15835, 2009 changes to 16062, 2010 16322, 2011 17478, 2012 17479, 2013 17480, 2014 17481, 2015 17851
  	#pie vs cake vs cookies?
  	# median calories over time
  	#median time over time
  	#chicken, beef, pork
  	#gluten free over time
  	#number of comments over time 
  	#The use of best over time?
  	#do a word count see reg expression for example