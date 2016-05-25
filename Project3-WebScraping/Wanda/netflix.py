from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import TimeoutException
from sys import argv
import time
import csv
import lxml.html
import unicodedata
from selenium.common.exceptions import NoSuchElementException   

def listlist(br):  #feeding in the browser viewing activity page 
    html_list = br.find_element_by_id("viewingactivity") 
    items = html_list.find_elements_by_tag_name("li")  
    nlist = [] #make two lists. one for dates. one for titles.
    mlist = []
    f = open('nflixg.csv', 'w') #missing values before 2014...6/13/14
    for item in items:
        date = item.find_element_by_xpath("./div[@class='col date nowrap']").text
        date = unicodedata.normalize('NFKD', date).encode('ascii','ignore')
        title = item.find_element_by_xpath("./div[@class='col title']").text
        title = unicodedata.normalize('NFKD', title).encode('ascii','ignore')
        nlist.append(date)
        mlist.append(title)
    	#print zip(nlist, mlist) 
    for i in xrange(len(nlist)):
    	f.write("{} , {}\n".format(nlist[i], mlist[i]))
    f.close()
   ##### own function below? not pulling
    # newlist=[]
    # newerlist = []
    # newestlist = []
    # series = []

    # urls = br.find_elements(By.CLASS_NAME, "retableRow") #html_list vs. br 
    # for i, e in enumerate(urls):
    # 	url = 'https://www.netflix.com/title/' + str(e.get_attribute('data-movieid')) #getting all the movieid's 
    # 	dataseries = str(e.get_attribute('data-series'))
    # 	series.append(dataseries)
    # 	print series
    # 	urls[i] = url   
    # for i in urls: 
    # 	time.sleep(3) 

    # 	titlle = parse_detail_page(br,i) #TITLE
    # 	newlist.append(titlle) 
    # 	print newlist 

    # 	director, genre = parse_d_page(br,i) #DIRECTOR
    # 	newerlist.append(director)
    # 	print newerlist

    # 	newestlist.append(genre)
    # 	print newestlist

    # 	h = open('netflxs.csv', 'w') #nflxx.csv' #added another x 
    # 	for i in xrange(len(newlist)):
    # 		h.write("{} , {} , {} , {}\n".format(newlist[i], newerlist[i], newestlist[i], series[i]))
    # 		#h.write("{},\n".format(newlist[i])) 
    # 	h.close()

    	 

# def parse_detail_page(br, url): #for each url it's pulling the title, need something outside function
# 	br.get(url)
# 	br.find_element_by_xpath('//*[@id="tab-ShowDetails"]/a').click()
# 	try:
# 		titl = br.find_element_by_xpath('//a[@type = "title"]').text
# 		titl = unicodedata.normalize('NFKD', titl).encode('ascii','ignore')
# 	except NoSuchElementException:
# 		return False
# 	return titl
# 	titlez = " ".join(titl.split(' ')[0:len(titl.split(' '))])
# 	return titlez

# def parse_d_page(br, url): #for each url it's pulling the title, need something outside function
# 	br.get(url)
# 	br.find_element_by_xpath('//*[@id="tab-ShowDetails"]/a').click()
# 	try:
# 		d = br.find_element_by_xpath('//a[@type = "person"]').text
# 		d = unicodedata.normalize('NFKD', d).encode('ascii','ignore')
# 		dz = " ".join(d.split(' ')[0:len(d.split(' '))])
# 		try:		
# 			g = br.find_element_by_xpath('//a[@type = "genre"]').text
# 			g = unicodedata.normalize('NFKD', g).encode('ascii','ignore')
# 			gz = " ".join(g.split(' ')[0:len(g.split(' '))])
# 			return dz, gz
# 		except:
# 			return dz, ""
# 	except NoSuchElementException:
# 		try:		
# 			g = br.find_element_by_xpath('//a[@type = "genre"]').text
# 			g = unicodedata.normalize('NFKD', g).encode('ascii','ignore')
# 			gz = " ".join(g.split(' ')[0:len(g.split(' '))])
# 			return "", gz
# 		except NoSuchElementException:
# 			return "", ""

if __name__ == '__main__':
	browser = webdriver.Firefox() #opens Firefox browser
	browser.get('https://www.netflix.com/Login') #login details
	username = browser.find_element_by_name("email")
	password = browser.find_element_by_name("password")
	username.send_keys("drscholls303@gmail.com")
	password.send_keys("capunta.6")
	browser.find_element_by_xpath('//*[@id="appMountPoint"]/div/div[2]/div/form[1]/button').click() 
	time.sleep(5)
	browser.find_element_by_xpath('//*[@id="appMountPoint"]/div/div/div[3]/div/div/ul/li[1]/a').click() #user profile
	browser.get('https://www.netflix.com/WiViewingActivity') #user viewing activity
	time.sleep(5)

	for i in range(29): #32
		print i
		time.sleep(2)
		browser.execute_script("window.scrollTo(0, document.body.scrollHeight);") #infinite scrolling #scroll and pull everything
	listlist(browser)