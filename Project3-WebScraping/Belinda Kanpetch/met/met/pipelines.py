# -*- coding: utf-8 -*-

# Define your item pipelines here
#
# Don't forget to add your pipeline to the ITEM_PIPELINES setting
# See: http://doc.scrapy.org/en/latest/topics/item-pipeline.html

from scrapy.exceptions import DropItem
from scrapy.conf import settings
from scrapy import log
import pymongo
import scrapy
from scrapy.pipelines.images import ImagesPipeline
from scrapy.exceptions import DropItem


# class ValidateItemPipeline(object):
# 	def process_item(self, item, spider):
# 		if not all(item.values()):
# 			raise DropItem("Missing values!")
# 		else:
# 			return item

# class WriteItemPipeline(object):
# 	def __init__(self):
# 		self.file = open('test.txt', 'w')
#
# 	def process_item(self, item, spider):
# 		line = str(item['piece_name'][0]) + '\t' + str(item['artist_name'][0])\
#                + '\t' + str(item['piece_date'][0]) + '\t'\
#                + str(item['accession_num'][0]) + '\t' \
#                + str(item['medium'][0]) + '\t' \
#                + str(item['on_view'][0]) + '\n'
# 		self.file.write(line)
# 		return item



class MongoDBPipeline(object):
    def __init__(self):
        connection = pymongo.MongoClient(
            settings['MONGODB_SERVER'],
            settings['MONGODB_PORT']
        )
        db = connection[settings['MONGODB_DB']]
        self.collection = db[settings['MONGODB_COLLECTION']]

    def process_item(self, item, spider):
        print "inserting to mongo"
        self.collection.insert(dict(item))
        log.msg("Added to MongoDB database!",
                level=log.DEBUG, spider=spider)
        return item


# class MyImagesPipeline(ImagesPipeline):
#     def get_media_requests(self, item, info):
#         for image_url in item['image_urls']:
#             yield scrapy.Request(image_url)
#
#     def item_completed(self, results, item, info):
#         image_paths = [x['path']] for ok, x in results if ok]
#         if not image_paths:
#             raise DropItem('Item contains no images')
#         item['image_paths'] = image_paths
#         return item