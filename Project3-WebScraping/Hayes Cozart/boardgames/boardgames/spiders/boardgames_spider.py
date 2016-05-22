import scrapy
import re
from bs4 import BeautifulSoup 
from selenium import webdriver
from scrapy import Spider, Request 
from boardgames.items import BoardgamesItem
from scrapy.http import TextResponse



class BoardgamesSpider(Spider): 
	name = 'boardgames' 
	allowed_domains = ['boardgamegeek.com'] 
	start_urls =  ['https://boardgamegeek.com/browse/boardgame/page/1']

	def __init__(self):
		self.driver = webdriver.Firefox()	


	def parse(self, response):
		urls = ['https://boardgamegeek.com/browse/boardgame/page/' + str(n) for n in range(1, 16)] + [
			'https://boardgamegeek.com/browse/boardgame/page/' + str(n) for n in range(108, 124)]
		for url in urls:
			self.driver.get(url)
			response = TextResponse(url=self.driver.current_url, body=self.driver.page_source, encoding='utf-8')


			ranks = response.xpath('//*[@id="row_"]/td[1]/a/@name').extract()
			titles = response.xpath('//tr/td[3]/div/a/text()').extract()
			years = response.xpath('//tr/td/div/span/text()').extract()
			gRatings = response.xpath('//tr[@id="row_"]/td[4]/text()').extract()
			avgRatings = response.xpath('//*[@id ="row_"]/td[5]/text()').extract()
			voterss = response.xpath('//*[@id="row_"]/td[6]/text()').extract()
			links = response.xpath('//tr/td[3]/div/a/@href').extract()



			for i, link in enumerate(links):
				item = BoardgamesItem()
				item['rank'] = str(ranks[i])
				item['title'] = titles[i]
				item['year'] = str(years[i])
				item['gRating'] = float(gRatings[i])
				item['avgRating'] = float(avgRatings[i])
				item['voters'] = int(voterss[i])

				#request = scrapy.Request(link, callback=self.parse_page2)
				#request.meta['item'] = item
				#yield request

		#def parse_page2(self, response):
				self.driver.get( "https://boardgamegeek.com/"+link)
				response = TextResponse(url=self.driver.current_url, body=self.driver.page_source, encoding='utf-8')

				title = response.xpath(
					'//*[@id="mainbody"]/div/div/div[1]/div[2]/ng-include/div/div/ui-view/ui-view/div[1]/div/div[1]/ranks-module/div/div[2]/div/div/dl/dt/text()').extract()

				value = response.xpath(
					'//*[@id="mainbody"]/div/div/div[1]/div[2]/ng-include/div/div/ui-view/ui-view/div[1]/div/div[1]/ranks-module/div/div[2]/div/div//dl/dd/a/text()').extract()
				typeg = response.xpath(
					'//*[@id="mainbody"]/div/div/div[1]/div[2]/ng-include/div/div/ui-view/ui-view/div[1]/div/div[1]/classifications-module/div/div[2]/ul/li[1]/div[2]/span/a/text()').extract()
				category = response.xpath(
					'//*[@id="mainbody"]/div/div/div[1]/div[2]/ng-include/div/div/ui-view/ui-view/div[1]/div/div[1]/classifications-module/div/div[2]/ul/li[2]/div[2]/popup-list/span/a/text()').extract()
				age = response.xpath(
					'//*[@id="mainbody"]/div/div/div[1]/div[2]/ng-include/div/div/ui-view/ui-view/div[1]/div/div[1]/gameplay-module/div/div[2]/ul/li[1]/div[1]/span/text()').extract()
				players = response.xpath(
					'//*[@id="mainbody"]/div/div/div[1]/div[2]/ng-include/div/div/ui-view/ui-view/div[1]/div/div[1]/gameplay-module/div/div[2]/ul/li[3]/div[2]/div/text()').extract()
				dificulty = response.xpath(
					'//*[@id="mainbody"]/div/div/div[1]/div[2]/ng-include/div/div/ui-view/ui-view/div[1]/div/div[1]/gameplay-module/div/div[2]/ul/li[4]/div[1]/span/span/text()').extract()
				language = response.xpath(
					'//*[@id="mainbody"]/div/div/div[1]/div[2]/ng-include/div/div/ui-view/ui-view/div[1]/div/div[1]/gameplay-module/div/div[2]/ul/li[5]/div[2]/span/span/text()').extract()
				description = response.xpath(
					'//*[@id="mainbody"]/div/div/div[1]/div[2]/ng-include/div/div/ui-view/ui-view/div[1]/div/div[2]/overview-module/description-module/div/div[2]/div/div[1]/div/article/div[2]/p[1]/text()').extract()
				price = response.xpath(
					'//*[@id="mainbody"]/div/div/div[1]/div[2]/ng-include/div/div/ui-view/ui-view/div[1]/div/div[2]/overview-module/marketplace-module/div/div[2]/div/div[2]/div/div[2]/ul/li[2]/a/div[1]/strong/text()').extract()
				timemin = response.xpath('//*[@id="mainbody"]/div/div/div[1]/div[2]/ng-include/div/div/ui-view/ui-view/div[1]/div/div[1]/gameplay-module/div/div[2]/ul/li[2]/div/span/span/span[1]/text()').extract()
				timemax = response.xpath('//*[@id="mainbody"]/div/div/div[1]/div[2]/ng-include/div/div/ui-view/ui-view/div[1]/div/div[1]/gameplay-module/div/div[2]/ul/li[2]/div/span/span/span[2]/text()').extract()


				self.driver.get("https://boardgamegeek.com/" + link +"/credits")
				response = TextResponse(url=self.driver.current_url, body=self.driver.page_source, encoding='utf-8')

				mechanisms = response.xpath(
					'////*[@id="mainbody"]/div/div/div[1]/div[2]/ng-include/div/div/ui-view/ui-view/div/div/div[2]/credits-module/ul/li[8]/div[2]/div/div/a/text()').extract()

			#item = response.meta['item']

				item['ranks'] = zip(map(lambda x: x.strip(),map(str,title)),map(lambda x: x.strip(),map(str,value)))
				item['typeg'] = map(str,typeg)
				item['category'] = map(str,category)
				item['mechanisms'] = map(str,mechanisms)
				item['age'] = map(str,age)
				#players[0] = re.sub(u"\u2013", "-", players[0])
				item['players'] = players[0].encode('ascii','ignore').strip()
				item['dificulty'] = map(str,dificulty)
				item['language'] = map(str,language)
				item['description'] = description
				item['price'] = map(str,price)
				item['timemin'] = map(str,timemin)
				item['timemax'] = map(str,timemax)


				yield item