# -*- coding: utf-8 -*-
from scrapy import Spider
from scrapy.selector import Selector
from met.items import MetItem
from selenium import webdriver
from scrapy.http import TextResponse
import time

class MetSpider(Spider):
    name = 'met'
    allowed_urls = ['http://www.metmuseum.org/']
    start_urls = ['http://www.metmuseum.org/art/collection']

    def __init__(self):
        self.driver = webdriver.Chrome()

    def parse(self, response):
        self.driver.get('http://www.metmuseum.org/art/collection')

        # while True:
        #     try:
        #         show_more = self.driver.find_element_by_class_name("show-more")
        #         time.sleep(2)
        #         show_more.click()
        #     except:
        #         break

        # clicking the show more button
        for i in range(5):
            show_more = self.driver.find_element_by_class_name("show-more")
            time.sleep(3)
            show_more.click()

        response = TextResponse(url=self.driver.current_url, body=self.driver.page_source, encoding='utf-8')
        test = response.xpath('//h2[@class="card__title"]/a/@href')
        for href in response.xpath('//h2[@class="card__title"]/a/@href'):
            url = response.urljoin(href.extract())
            print url
        # scraping the urls from the first page & creating a list of links
        # card_link_list = self.driver.find_elements_by_xpath('//h2[@class="card__title"]/a')
        # card_link_list = map(lambda x: x.get_attribute('href'), card_link_list)
            self.driver.get(url)
            time.sleep(2)
            response1 = TextResponse(url=self.driver.current_url, body=self.driver.page_source, encoding='utf-8')
            item = MetItem()
            for sel in response1.xpath('//div[@class="l-component-block"]'):
                title = self.driver.find_element_by_xpath('//h1[@class="collection-details__object-title"]').text
                print title
                location = self.driver.find_element_by_xpath('//div[@class="collection-details__location"]').text
                print location
                item['title'] = title
                item['location'] = location
            artifact_detail = {}
            for detail in response1.xpath('//dl[@class="collection-details__tombstone--row"]').extract():
                key = Selector(text=detail).xpath('//dt/text()').extract()[0]
                value = Selector(text=detail).xpath('//dd/text()').extract()[0]
                artifact_detail[key] = value
            item['artifact_detail'] = artifact_detail
            yield item
    # def parse_dir_contents(self,response,browser):
    #     for sel in response.xpath('//div[@class="l-component-block"]'):
    #         item = MetItem
    #         title = browser.find_element_by_xpath('//h1[@class="collection-details__object-title"]').text
    #         print title
    #         location = browser.find_element_by_xpath('//div[@class="collection-details__location"]').text
    #         print location
    #         item['title'] = title
    #         item['location'] = location
    #
    #     for detail in response.xpath('//div[@class="collection-details__tombstone"]'):
    #         artifact_detail = {}
    #         for dl in detail:
    #            key = Selector(text=dl).xpath('//dl/dt').extract()
    #            value = Selector(text = dl).xpath('//dl/dd').extract()
    #            artifact_detail[key] = value

            # date = self.driver.find_element_by_xpath('//dd[@class="collection-details__tombstone--value"]')[0].text
            # date = self.driver.find_element_by_xpath('/html/body/div[3]/div/div/div[2]/div[1]/dl[1]/dd[0]').text
            # geography = self.driver.find_element_by_xpath('//dd[@class="collection-details__tombstone--value"]')[1].text
            # geography = self.driver.find_element_by_xpath('/html/body/div[3]/div/div/div[2]/div[1]/dl[2]/dd[1]').text
            # culture = self.driver.find_element_by_xpath('')
            # medium = self.driver.find_element_by_xpath('')
            # classification = self.driver.find_element_by_xpath('')
            # credit = self.driver.find_element_by_xpath('')

            # item['artifact_detail'] = artifact_detail


        # i know what is below works.....
        # div card card--collection which is a card for each piece
        # rows = self.driver.find_elements_by_class_name("grid-listing__item")

        # for row in rows:
        #     piece_name = Selector(text=row.get_attribute("innerHTML")).xpath('//div/h2/a/text()').extract()  # replace with proper xpath
        #     artist_name = Selector(text=row.get_attribute("innerHTML")).xpath('//div/p/span/text()').extract()
        #     piece_date = Selector(text=row.get_attribute("innerHTML")).xpath('//div/div/span[1]/text()').extract()
        #     medium = Selector(text=row.get_attribute("innerHTML")).xpath('//div/div/span[2]/text()').extract()
        #     accession_num = Selector(text=row.get_attribute("innerHTML")).xpath('//div/div/span[3]/text()').extract()
        #     on_view = Selector(text=row.get_attribute("innerHTML")).xpath('//div/div/span[4]/text()').extract()
        #     #image_urls = hxs.select('//figure/a/img/@src').extract()
        #
        #     item = MetItem()
        #     item['piece_name'] = piece_name
        #     item['artist_name'] = artist_name
        #     item['piece_date'] = piece_date
        #     item['accession_num'] = accession_num
        #     item['medium'] = medium
        #     item['on_view'] = on_view
        #     #item['image_urls'] = ['http:' + x for x in image_urls]
        #     print 'i got here'
        #     print item
        #     yield item





