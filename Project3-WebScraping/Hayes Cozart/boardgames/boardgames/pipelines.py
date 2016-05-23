# -*- coding: utf-8 -*-

# Define your item pipelines here
#
# Don't forget to add your pipeline to the ITEM_PIPELINES setting
# See: http://doc.scrapy.org/en/latest/topics/item-pipeline.html

from scrapy.exceptions import DropItem
from scrapy.conf import settings
from scrapy import log
import pymongo

#class ValidateItemPipeline(object):
#	def process_item(self, item, spider):
#		if not all(item.values()):
#			raise DropItem("Missing values!")
#		else:
#			return item

#class TestPipeline(object):
#	def __init__(self):
#		self.file = open('Boardgames.txt', 'w')
#
#	def process_item(self, item, spider):
#		line = str(item['rank'][0]) + '\t' + str(item['title'][0])\
#				+'\t' + str(item['year'][0]) + '\t'\
#				+ str(item['gRating'][0]) + '\t' + str(item['avgRating'][0])+ '\t'\
#				+ str(item['voters'][0]) + '\t' + str(item['ranks'][0])+ '\t'\
#				+ str(item['typeg'][0]) + '\t' + str(item['category'][0])+ '\t'\
#				+ str(item['mechanisms'][0]) + '\t' + str(item['age'][0])+ '\t'\
#				+ str(item['players'][0])+ '\t'\
#				+ str(item['dificulty'][0]) + '\t' + str(item['language'][0])+ '\t' \
#			    + str(item['description'][0]) + '\t' + str(item['price'][0]) + '\t' \
#			    + str(item['timemin'][0]) + '\t' + str(item['timemax'][0]) + '\t' \
#			    +'\n'
#		self.file.write(line)
#		return item


class MongoDBPipeline(object):
	def __init__(self):
		connection = pymongo.MongoClient(
			settings['MONGODB_SERVER'],
			settings['MONGODB_PORT']
			)
		db = connection[settings['MONGODB_DB']]
		self.collection = db[settings['MONGODB_COLLECTION']]

	def process_item(self, item, spider):
		self.collection.insert(dict(item))
		log.msg("Added to MongoDB database!",
			level=log.DEBUG, spider=spider)
		return item