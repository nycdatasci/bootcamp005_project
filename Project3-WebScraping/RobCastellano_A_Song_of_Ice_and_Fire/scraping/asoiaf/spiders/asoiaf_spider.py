# -*- coding: utf-8 -*-
from scrapy import Spider
from scrapy.selector import Selector
from asoiaf.items import AsoiafItem
from selenium import webdriver
from scrapy.http import TextResponse, Request
import time

class AsoiafSpider(Spider):
    name = "asoiaf"
    start_urls = ['http://towerofthehand.com/books/101/002/index.html']     #This url doesn't do anything in this program
                                                                            # but is required.

    def __init__(self):
        self.driver = webdriver.Chrome()

    def parse(self, response):
        #Read in urls. #urls.txt is all of the urls of all the chapters. I made the list of urls in urls.py
        with open('urls.txt', 'r') as f:
            urls = [line.strip('\n') for line in f]
        for url in urls:
            yield Request(url, callback=self.parse_url, dont_filter=True)    #Get item for each url

    def parse_url(self, response):
        self.driver.get(response.url)
        time.sleep(5)      #Pause so page has enough time for AJAX to load
        response = TextResponse(url=response.url, body=self.driver.page_source, encoding='utf-8')

        item = AsoiafItem()     #item consists of Appearing, POV, ChapterNum, Book, ChapterName, Summary, Blurb, Score

        #Get features from url
        item['Appearing'] = map(lambda s: str(s), response.xpath('//div[@id="appearances"]/ol/li/a/text()').extract())
        item['POV'] = str(response.xpath('//div[@class = "jumplist"]/ul/li/a/text()')[0].extract())
        item['ChapterNum'] = int(response.xpath('//span[@class = "teaser"]/b/text()').extract()[0].split()[-1])
        book_chapter = str(response.xpath('//*[@id="headline"]/h2/text()').extract()[0])
        item['Book'] = book_chapter.split()[0]
        item['ChapterName'] = ' '.join(book_chapter.split()[1:])
        item['Blurb'] = str(response.xpath('//*[@id="content"]/div[2]/div[1]/div[1]/span/text()[2]').extract()[0])

        #Score is the variable that requires selenium. It renders in AJAX.
        item['Score'] = float(response.xpath('//div[@class = "score"]/text()').extract()[0])


        if book == "ADWD" and chapternum > 14:      #Not all chapters have summaries yet (work in progress)
            item['Summary'] = " "
        else:
            #Try to get the summary. This attempts to extract the summary words that are no contained in hyperlinks.
            summary_no_href = str(''.join(response.xpath('//*[@id="content"]/div[2]/div[2]/div[1]/p/text()').extract()))

            #Some summaries are only one paragraph and the html is set up to not be in a /p/ tag. In this case,
            # summary_no_href above is empty. In this case, we get the summary without /p/ tags.
            if summary_no_href == "":
                #Get words in summary without hyperlinks.
                summary_no_href = str(
                    ''.join(response.xpath('//*[@id="content"]/div[2]/div[2]/div[1]/text()').extract()))
                #Get words in hyperlinks.
                hrefs = str(' '.join(response.xpath('//*[@id="content"]/div[2]/div[2]/div[1]/a/text()').extract()))
            else:
                #This is the case the summary is in the /p/ tag. Get words that are in hyperlinks.
                hrefs = str(' '.join(response.xpath('//*[@id="content"]/div[2]/div[2]/div[1]/p/a/text()').extract()))

            #Summary is the summary without hyperlinks and hyperlinks put together. Note: This is not in correct order,
            # but contains all words.
            item['Summary'] = summary_no_href + ' ' + hrefs
        return item