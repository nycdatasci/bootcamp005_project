# -*- coding: utf-8 -*-
import scrapy
from scrapy.selector import Selector
from pricing.items import PricingItem


class YahooSpider(scrapy.Spider):
    name = "yahoo"
    allowed_domains = ["finance.yahoo.com"]
    start_urls = ['http://finance.yahoo.com/q/hp?s=%5ERUT&a=11&b=1&c=2015&d=04&e=31&f=2016&g=d&z=1&y=1'
    , 'http://finance.yahoo.com/q/hp?s=%5ERUT&a=11&b=1&c=2015&d=04&e=31&f=2016&g=d&z=66&y=66']

    def parse(self, response):
	tds = response.xpath('////td[@class="yfnc_tabledata1"]').extract()
	
	idx = 0
	while (idx < len(tds)):
		date = Selector(text=tds[idx]).xpath('//td[@nowrap]/text()').extract()
		if date is not "":
			
			item = PricingItem()
			item['Date' 	  ] =  date
			idx += 1
			if idx >= len(tds): break
			item['Open' 	  ] =  Selector(text=tds[idx]).xpath('///text()').extract()
			idx += 1
			if idx >= len(tds): break
			item['High' 	  ] =  Selector(text=tds[idx]).xpath('///text()').extract()
			idx += 1
			if idx >= len(tds): break
			item['Low' 	  ] =  Selector(text=tds[idx]).xpath('///text()').extract()
			idx += 1
			if idx >= len(tds): break
			item['Close' 	  ] =  Selector(text=tds[idx]).xpath('///text()').extract()
			idx += 1
			if idx >= len(tds): break
			item['Vol' 	  ] =  Selector(text=tds[idx]).xpath('///text()').extract()
			idx += 1
			if idx >= len(tds): break
			idx += 1
			if idx >= len(tds): break

			yield item
