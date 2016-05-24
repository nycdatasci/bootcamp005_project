
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By
import pandas as pd
import time
from random import randint
import csv
import numpy as np

if __name__ == '__main__':

    ##### LOAD MAIN CATEGORIES PAGE #####
    driver = webdriver.Firefox()
    driver.set_page_load_timeout(8)
    homePg = 'www.salary.com'
    catPgName = "http://swz.salary.com/SalaryWizard/LayoutScripts/Swzl_SelectCategory.aspx"
    driver.get(catPgName)

    ##### LOAD CATEGORIES #####
    cats = []
    getCategories = driver.find_elements(By.CLASS_NAME, "category_a")
    for cat in getCategories:
        cats = cats + [[cat.tag_name,
                       cat.get_attribute('href'),
                       cat.text]]
    catDF = pd.DataFrame(cats,columns = ['Tag','hRef','Text'])
    catDF.to_csv('Categories.csv')

    JobsHeader = pd.DataFrame(columns = ['Src', 'PgNo','ItemNo','Tag', 'hRef', 'Text','Desc'])
    JobsHeader.to_csv('JobsList.csv')                               # create blank jobs file

    for idx, cat in catDF.sample(frac=1).iterrows():                # Loop thru Random Categories
        linkText = cat['Text']
        Link = driver.find_element_by_link_text(linkText)
        Link.click()                               # Select Category

        ##### GET JOB INFO
        jobLinks = []
        pageLinks = driver.find_elements_by_class_name('vv1')
        TotalPages = len(pageLinks)
        if TotalPages == 0:
            TotalPages = 1
        for i in range(1,TotalPages):
            getJobList = driver.find_elements_by_class_name('swz_jobtitle_update')
            getJobDesc = driver.find_elements_by_class_name('swz_jobdesc')
            for j in range(0,len(getJobList)):
                job = getJobList[j]
                jobDesc = getJobDesc[j]
                jobLinks = jobLinks + [[cat['Text'],
                                        i,
                                        j,
                                        job.tag_name,
                                        job.get_attribute('href'),
                                        job.text,
                                        jobDesc.text]]

            if i < TotalPages:
                try:
                    nextPage = driver.find_element_by_xpath("//a[@class='vv1' and text()='" + str(i + 1) + "']")
                    nextPage.click()
                    time.sleep(float(randint(5, 20)) / 10)
                except:
                    print 'Break at category:' + linkText + ', page ' + str(i+1)
                    break

        ##### End of loop through job listings pages

        jobDF = pd.DataFrame(jobLinks, columns=['Src', 'PgNo','ItemNo','Tag', 'hRef', 'Text','Desc'])
        jobDF.to_csv('JobsList.csv', header=False, mode='a', quoting=csv.QUOTE_NONNUMERIC, escapechar='\\')
        driver.get(catPgName)           # go back to catalog page
        time.sleep(float(randint(10,30))/10.0)


