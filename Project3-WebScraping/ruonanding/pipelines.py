# -*- coding: utf-8 -*-
from scrapy.exceptions import DropItem

class ValidateItemPipeline(object):
	def process_item(self, item, spider):
		if not all(item.values()):
			raise DropItem("Missing values!")
		else:
			return item

class WriteItemPipeline(object):
	def __init__(self):
		self.file = open('Price.txt', 'w')

	def process_item(self, item, spider):
		line = str(item['Date'][0]).strip() + '\t' + str(item['Open'][0]) + '\t' + str(item['High'][0]) + '\t' + str(item['Low'][0]) + '\t' + str(item['Close'][0]) + '\t' + str(item['Vol'][0]) + '\n' 
		self.file.write(line)
		return item
