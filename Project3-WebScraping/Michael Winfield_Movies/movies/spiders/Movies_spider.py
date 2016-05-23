from scrapy import Spider
from scrapy.selector import Selector
from selenium import webdriver
from scrapy.http import TextResponse
from movies.items import MoviesItem


class MoviesSpider(Spider):
    name = "movies"
    allowed_urls = ['http://www.the-numbers.com/']
    start_urls = ['http://www.the-numbers.com/movie/budgets/all']

    def __init__(self):
        self.driver = webdriver.Chrome(executable_path=r"C:\chromedriver")

    def parse(self, response):
        self.driver.get('http://www.the-numbers.com/movie/budgets/all')
        response = TextResponse(url=response.url, body=self.driver.page_source, encoding='utf-8')
        rows = response.xpath('//*[@id="page_filling_chart"]/center/table/tbody/tr').extract()

        for i in range(1, 10250, 2):
            RDate = Selector(text=rows[i]).xpath('//td[2]/a/text()').extract()
            Title = Selector(text=rows[i]).xpath('//td[3]/b/a/text()').extract()
            PBudget = Selector(text=rows[i]).xpath('//td[4]/text()').extract()
            DomesticG = Selector(text=rows[i]).xpath('//td[5]/text()').extract()
            WorldwideG = Selector(text=rows[i]).xpath('//td[6]/text()').extract()

            print RDate, Title, PBudget, DomesticG, WorldwideG

            item = MoviesItem()
            item['RDate'] = RDate
            item['Title'] = Title
            item['PBudget'] = PBudget
            item['DomesticG'] = DomesticG
            item['WorldwideG'] = WorldwideG

            yield item
