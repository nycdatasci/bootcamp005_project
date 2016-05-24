from scrapy import Spider
from indeed.items import IndeedItem
from scrapy.selector import Selector
from selenium import webdriver
from scrapy.http import TextResponse
import scrapy


class IndeedSpider(Spider):
    name = 'indeed'

    allowed_domains = ["http://www.dice.com/"]

    s1 = 'https://www.dice.com/jobs/q-data_scientist-limit-30-l-New_York%2C_NY-radius-30-startPage-1-limit-30-jobs?searchid=291607343849'

    start_urls = [s1]

    def __init__(self):
        self.driver = webdriver.Chrome()

    def parse(self, response):

        self.driver.get(response.url)
        urls = []

        for i in range(1,20):

            # self.driver.get(response.url)
            response = TextResponse(url=self.driver.current_url, body=self.driver.page_source, encoding='utf-8')
            self.driver.implicitly_wait(10)

            for j in range(1, 31):
                result = response.xpath('//*[@class="col-md-9"]/div[1]/div['+str(j)+']/h3/a/@href')
                urls.extend(result)

            next_page = self.driver.find_element_by_xpath('//*[@title="Go to next page"]')
            next_page.click()


        for href in urls:
            print href
            url = href.extract()
            self.driver.get(url)
            response = TextResponse(url=self.driver.current_url, body=self.driver.page_source, encoding='utf-8')
            item = IndeedItem()

            for sel in response.xpath('//div[@class="col-md-5 col-lg-6"]'):
                item['job_title'] = sel.xpath('//div[@class="col-md-5 col-lg-6"]/h1/text()').extract()
                item['location'] = sel.xpath('//div[@class="col-md-5 col-lg-6"]/ul/li[2]/text()').extract()
                item['company_name'] = sel.xpath('//div[@class="col-md-5 col-lg-6"]/ul/li[1]/a/text()').extract()

            for sel_1 in response.xpath('//*[@id="bd"]/div/div[1]'):
                item['job_type'] = sel_1.xpath('//div[2]/div/div[2]/span/text()').extract()
                item['job_salary'] = sel_1.xpath('//div[3]/div/div[2]/span/text()').extract()


            yield item


        self.driver.close()




    # def parse_dir_contents(self, response):
    #
    #     item = IndeedItem()
    #
    #     for sel in response.xpath('//div[@class="col-md-5 col-lg-6"]'):
    #         item['job_title'] = sel.xpath('//div[@class="col-md-5 col-lg-6"]/h1/text()').extract()
    #         item['location'] = sel.xpath('//div[@class="col-md-5 col-lg-6"]/ul/li[2]/text()').extract()
    #         item['company_name'] = sel.xpath('//div[@class="col-md-5 col-lg-6"]/ul/li[1]/a/text()').extract()
    #
    #     for sel_1 in response.xpath('//*[@id="bd"]/div/div[1]'):
    #         item['job_type'] = sel_1.xpath('//div[2]/div/div[2]/span/text()').extract()
    #         item['job_salary'] = sel_1.xpath('//div[3]/div/div[2]/span/text()').extract()
    #
    #     yield item











