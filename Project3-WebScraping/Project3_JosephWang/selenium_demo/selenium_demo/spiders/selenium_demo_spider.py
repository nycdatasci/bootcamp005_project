# -*- coding:: utf-8 -*-
from scrapy import Spider
from scrapy.selector import Selector
from selenium_demo.items import MoviesItem
from selenium import webdriver
from scrapy.http import TextResponse
from selenium.webdriver.common.by import By
import time


#class DemoSpider(Spider):
#     name = "selenium_demo"
#     allowed_urls = ['http://mlb.mlb.com']
#     start_urls = ["http://mlb.mlb.com/stats/sortable.jsp?c_id=hou#game_type='R'&season=2015&league_code='MLB'&split=&playerType=ALL&sectionType=sp&statType=pitching&elem=%5Bobject+Object%5D&tab_level=child&click_text=Sortable+Player+pitching&season_type=ANY&page=1&ts=1463491577854&sportCode='mlb'&team_id=&active_sw=&position='1'&page_type=SortablePlayer&sortOrder='desc'&sortColumn=w&results=&perPage=50&timeframe=&last_x_days=&extended=0"]

#     def parse(self, response):
#         rows = response.xpath('//*[@id="datagrid"]').extract()

#         for row in rows:
#             RDate = Selector(text=row).xpath('//tr/td[2]/a/text()').extract()
#             Title = Selector(text=row).xpath('//td[3]/b/text()').extract()
#             PBudget = Selector(text=row).xpath('//td[4]/text()').extract()
#             DomesticG = Selector(text=row).xpath('//td[5]/text()').extract()
#             WorldwideG = Selector(text=row).xpath('//td[6]/text()').extract()
#
#             item = MoviesItem()
#             item['RDate'] = RDate
#             item['Title'] = Title
#             item['PBudget'] = PBudget
#             item['Domestic'] = DomesticG
#             item['WorldwideG'] = WorldwideG
#
#             yield item

class DemoSpider(Spider):
    name = "selenium_demo"
    allowed_urls = ['http://mlb.mlb.com']
    start_urls = ["http://mlb.mlb.com/stats/sortable.jsp?c_id=hou#game_type='R'&season=2016&league_code='MLB'&split=&playerType=ALL&sectionType=sp&statType=pitching&elem=%5Bobject+Object%5D&tab_level=child&click_text=Sortable+Player+pitching&season_type=ANY&page=1&ts=1463958176146&sportCode='mlb'&team_id=&active_sw=&position='1'&page_type=SortablePlayer&sortOrder='desc'&sortColumn=w&results=&perPage=50&timeframe=&last_x_days=&extended=0"]
    def __init__(self):
        self.driver = webdriver.Chrome()

    def parse(self, response):
        self.driver.get("http://mlb.mlb.com/stats/sortable.jsp?c_id=hou#game_type='R'&season=2016&league_code='MLB'&split=&playerType=ALL&sectionType=sp&statType=pitching&elem=%5Bobject+Object%5D&tab_level=child&click_text=Sortable+Player+pitching&season_type=ANY&page=1&ts=1463958176146&sportCode='mlb'&team_id=&active_sw=&position='1'&page_type=SortablePlayer&sortOrder='desc'&sortColumn=w&results=&perPage=50&timeframe=&last_x_days=&extended=0")
        # response.url="http://mlb.mlb.com/stats/sortable.jsp?c_id=hou#game_type='R'&season=2015&league_code='MLB'&split=&playerType=ALL&sectionType=sp&statType=pitching&elem=%5Bobject+Object%5D&tab_level=child&click_text=Sortable+Player+pitching&season_type=ANY&page=2&ts=1463597259434&sportCode='mlb'&team_id=&active_sw=&position='1'&page_type=SortablePlayer&sortOrder='desc'&sortColumn=w&results=&perPage=50&timeframe=&last_x_days=&extended=1"
        #response = TextResponse(url=response.url, body=self.driver.page_source,encoding='utf-8')
    #     response = TextResponse(url=response.url, body=response.url, encoding='utf-8')
        #encoding='utf-8'

        # rows = response.xpath('//*[ @ id = "datagrid"]').extract()
        while True:


            try:

                response = TextResponse(url=response.url, body=self.driver.page_source, encoding='utf-8')
                rows = response.xpath('//tr').extract()
                next = self.driver.find_element(By.CLASS_NAME, "paginationWidget-next")
                for row in rows:
                    Player = Selector(text=row).xpath('//td[2]/a/text()').extract()
                    W = Selector(text=row).xpath('//td[6]/text()').extract()
                    L = Selector(text=row).xpath('//td[7]/text()').extract()
		    IP = Selector(text=row).xpath('//td[13]/text()').extract()
		    ERA = Selector(text=row).xpath('//td[8]/text()').extract()
		    R = Selector(text=row).xpath('//td[15]/text()').extract()
		    ER = Selector(text=row).xpath('//td[16]/text()').extract()
		    HR = Selector(text=row).xpath('//td[17]/text()').extract()
                    WHIP = Selector(text=row).xpath('//td[21]/text()').extract()
		    WPCT = Selector(text=row).xpath('//td[38]/text()').extract()
		    GO_AO = Selector(text=row).xpath('//td[39]/text()').extract()
		    OBP = Selector(text=row).xpath('//td[40]/text()').extract()
		    SLG = Selector(text=row).xpath('//td[41]/text()').extract()
		    OPS = Selector(text=row).xpath('//td[42]/text()').extract()
		    K_9 = Selector(text=row).xpath('//td[43]/text()').extract()
		    BB_9 = Selector(text=row).xpath('//td[44]/text()').extract()
		    H_9 = Selector(text=row).xpath('//td[45]/text()').extract()
		    K_BB = Selector(text=row).xpath('//td[46]/text()').extract() 
		    P_IP = Selector(text=row).xpath('//td[47]/text()').extract()

                    item = MoviesItem()
                    item['Player'] = Player
                    item['W'] = W
                    item['L'] = L
                    item['IP'] = IP
		    item['ERA']= ERA
		    item['R'] = R
		    item['ER'] = ER
		    item['HR'] = HR
                    item['WHIP'] = WHIP
                    item['WPCT'] = WPCT
		    item['GO_AO'] = GO_AO
		    item['OBP'] = OBP
		    item['SLG'] = SLG
		    item['OPS'] = OPS
		    item['K_9'] = K_9
		    item['BB_9'] = BB_9
		    item['H_9'] = H_9
		    item['K_BB'] = K_BB
		    item['P_IP'] = P_IP
		    










                    yield item

                next.click()
                time.sleep(5)

            except:
                break



