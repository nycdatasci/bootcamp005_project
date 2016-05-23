# -*- coding: utf-8 -*-

# Define your item pipelines here
#
# Don't forget to add your pipeline to the ITEM_PIPELINES setting
# See: http://doc.scrapy.org/en/latest/topics/item-pipeline.html
from scrapy.exceptions import DropItem
from scrapy import log
from scrapy.conf import settings
import pymongo

# class ValidateItemPipeline(object):
#     def process_item(self, item, spider):
#         if not all(item.values()):
#            raise DropItem("Missing values!")
#         else:
#            return item
#
# class WriteItemPipeline(object):
#     def __init__(self):
#         self.file = open('reddit.txt', 'w')
#
#     def process_item(self, item, spider):
#        line = str(item['date']) + '\t' + str(item['title']) + '\t' + str(item['label']) + '\t' + str(item['upvotes'])+ '\t' + str(item['comments']) +'\n'
#        self.file.write(line)
#        return item

# class RedditPipeline(object):
#     def process_item(self, item, spider):
#         return item

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
