from scrapy import Spider, Request
from scrapy.http import TextResponse
from reddit.items import RedditItem
from selenium import webdriver
from selenium.webdriver.common.by import By
from scrapy import Selector
import time


class redditSpider(Spider):
    name = 'reddit'
    allowed_domains = ['reddit.com']
    start_urls = ['https://www.reddit.com/r/technology/']

    def __init__(self):
        self.driver = webdriver.Firefox()

    def parse(self, response):
        self.driver.get('https://www.reddit.com/r/technology/')
        response = TextResponse(url=response.url, body=self.driver.page_source, encoding='utf-8')

        posts = response.xpath('//div[@class="entry unvoted"]').extract()
        upvotes = response.xpath('//div[@class="score unvoted"]/text()').extract()

        for i in range(50):
            for j, post in enumerate(posts):
                comment = Selector(text=post).xpath(
                    '//ul[@class="flat-list buttons"]/li[@class="first"]/a/text()').extract()
                label = Selector(text=post).xpath(
                    '//p[@class="title"]/span[@class="linkflairlabel"]/text()').extract()
                title = Selector(text=post).xpath('//p[@class="title"]/a/text()').extract()
                date = Selector(text=post).xpath(
                    '//p[@class="tagline"]/time/@datetime').extract()
                link = Selector(text=post).xpath(
                    '//p[@class="title"]/span[@class="domain"]/a/text()').extract()
                upvote = upvotes[j]
                item = RedditItem()
                item['upvotes'] = upvote
                item['comments'] = comment
                item['label'] = label
                item['title'] = title
                item['date'] = date
                item['link'] = link
                yield item

            self.driver.find_element_by_xpath('//a[@rel="nofollow next"]').click()
            time.sleep(2)



# while True:
# 		    try:
# 		        self.driver.find_element_by_partial_link_text('next').click()
# 		        time.sleep(3)
# 		        
# 		        # fill in the code find everything here.
# 		        upvotes = response.xpath('//div[@class="score unvoted"]').extract()
# 		        comments = response.xpath('//*[@id="thing_t3_4jn0rg"]/div[2]/ul/li[1]/a').extract()
# 		        label = response.xpath('//*[@id="thing_t3_4k4gh4"]/div[2]/p[1]/span[1]').extract()
# 		        title = response.xpath('//*[@id="thing_t3_4jn0rg"]/div[2]/p[1]/a').extract()
# 		        date = response.xpath('//*[@id="thing_t3_4jn0rg"]/div[2]/p[2]/time[1]').extract()
# 		        
# 		    except:
# 		        break
#         print '*' * 10
#         print upvotes
#         print '*' * 10	
# 		for start_url in self.start_urls:
# 		for i in range(10):
# 				new_url = start_url + '?count=' + str(25*i)
# 				yield Request(new_url, callback=self.parse_main_page)			
# 	def parse_main_page(self, response):
# 		
# 		upvotes = response.xpath('//div[@class="score unvoted"]').extract()
# 		print '*' * 10
# 		print upvotes
# 		print '*' * 10
# 		print response.url
# label = response.xpath(
# links =
# for link in links:
# 				yield #Request(link, callback=self.parse_detail_page)
# def parse_detail_page(self, response):
# fill code here  # yield item
