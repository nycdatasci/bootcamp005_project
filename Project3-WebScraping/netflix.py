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

def listlist(br):  #feeding in the browser viewing activity page 
    html_list = br.find_element_by_id("viewingactivity") 
    items = html_list.find_elements_by_tag_name("li")  
    # nlist = [] #make two lists. one for dates. one for titles.
    # mlist = []
    # f = open('nflx.csv', 'w') 
    # for item in items:
    #     text = item.text #text is type unicode
    #     text = unicodedata.normalize('NFKD', text).encode('ascii','ignore')
    #     titles = " ".join(text.split(' ')[0:len(text.split(' '))-4])
    #     date = titles[:7] # titles[:8]?
    #     title = titles[7:]  
    #     nlist.append(date)
    #     mlist.append(title)
    # #print zip(nlist, mlist) 
    # for i in xrange(len(nlist)):
    # 	f.write("{} , {}\n".format(nlist[i], mlist[i]))
    # f.close()
    
    newlist=[]
    newerlist = []
    newestlist = []

    urls = br.find_elements(By.CLASS_NAME, "retableRow") #html_list vs. br 
    for i, e in enumerate(urls):
    	url = 'https://www.netflix.com/title/' + str(e.get_attribute('data-movieid')) #getting all the movieid's 
    	urls[i] = url   
    for i in urls: 
    	time.sleep(3) #13 has error, too many?

    	parse_detail_page(br,i) #TITLE
    	newlist.append(parse_detail_page(br,i)) #appned to list each url's details
    	print newlist #write this to file [-1] , split final newlist in order to zip later

    	parse_d_page(br,i) #DIRECTOR
    	newerlist.append(parse_d_page(br, i))
    	print newerlist

    	parse_genre_page(br,i) #GENRE
    	newestlist.append(parse_genre_page(br,i))
    	print newestlist

    	h = open('nflxx.csv', 'w')
    	for i in xrange(len(newlist)):
    		h.write("{} , {} , {}\n".format(newlist[i], newerlist[i], newestlist[i]))
    		#h.write("{},\n".format(newlist[i])) 
    	h.close()

    	#if newlist element matches next element -> alert 

def parse_detail_page(br, url): #for each url it's pulling the title, need something outside function
	br.get(url)
	br.find_element_by_xpath('//*[@id="tab-ShowDetails"]/a').click()
	titl = br.find_element_by_xpath('//a[@type = "title"]').text
	titl = unicodedata.normalize('NFKD', titl).encode('ascii','ignore')
	titlez = " ".join(titl.split(' ')[0:len(titl.split(' '))])
	return titlez

def parse_d_page(br, url): #for each url it's pulling the title, need something outside function
	br.get(url)
	br.find_element_by_xpath('//*[@id="tab-ShowDetails"]/a').click()
	d = br.find_element_by_xpath('//a[@type = "person"]').text
	d = unicodedata.normalize('NFKD', d).encode('ascii','ignore')
	dz = " ".join(d.split(' ')[0:len(d.split(' '))])
	return dz

def parse_genre_page(br, url): #for each url it's pulling the title, need something outside function
	br.get(url)
	br.find_element_by_xpath('//*[@id="tab-ShowDetails"]/a').click()
	g = br.find_element_by_xpath('//a[@type = "genre"]').text
	g = unicodedata.normalize('NFKD', g).encode('ascii','ignore')
	gz = " ".join(g.split(' ')[0:len(g.split(' '))])
	return gz

# def parse_duration_page(br, url): #for each url it's pulling the title, need something outside function
# 	br.get(url)
# 	br.find_element_by_xpath('//*[@id="tab-Overview"]/a').click()
# 	dur = br.find_element_by_xpath('//span[@class = "duration"]').text
# 	dur = unicodedata.normalize('NFKD', dur).encode('ascii','ignore')
# 	durz = " ".join(dur.split(' ')[0:len(dur.split(' '))])
# 	return durz

#it's all good! 


if __name__ == '__main__':
	browser = webdriver.Firefox() #opens Firefox browser
	browser.get('https://www.netflix.com/Login') #login details
	username = browser.find_element_by_name("email")
	password = browser.find_element_by_name("password")
	username.send_keys("drscholls303@gmail.com")
	password.send_keys("capunta.6")
	browser.find_element_by_xpath('//*[@id="appMountPoint"]/div/div[2]/div/form[1]/button').click() 
	# browser.implicitly_wait(10)
	time.sleep(5)
	browser.find_element_by_xpath('//*[@id="appMountPoint"]/div/div/div[3]/div/div/ul/li[1]/a').click() #user profile
	browser.get('https://www.netflix.com/WiViewingActivity') #user viewing activity
	time.sleep(5)

	for i in range(29): #32
		#print i
		time.sleep(2)
		browser.execute_script("window.scrollTo(0, document.body.scrollHeight);") #infinite scrolling #scroll and pull everything
#taking a long time to load
	listlist(browser)