# -*- coding: utf-8 -*-

# Define your item pipelines here
#
# Don't forget to add your pipeline to the ITEM_PIPELINES setting
# See: http://doc.scrapy.org/en/latest/topics/item-pipeline.html


from scrapy.exceptions import DropItem


class ValidateItemPipeline(object):
    def process_item(self, item, spider):
        if not all(item.values()):
            raise DropItem("Missing values!")
        else:
            return item


class WriteItemPipeline(object):
    def __init__(self):
        self.file = open('MLB2015.txt', 'w')

    def process_item(self, item, spider):
          index=0
          for index in range(49):
           line = str(item["Player"][index])+' '+'\t'\
               + '\t' + str(item['W'][index])\
               + '\t' + str(item['L'][index]) \
               + '\t' + str(item['IP'][index]) \
	       + '\t' + str(item['ERA'][index]) \
	       + '\t' + str(item['R'][index]) \
               + '\t' + str(item['ER'][index]) \
	       + '\t' + str(item['HR'][index]) \
               + '\t' + str(item['WHIP'][index]) \
	       + '\t' + str(item['WPCT'][index]) \
	       + '\t' + str(item['GO_AO'][index]) \
	       + '\t' + str(item['OBP'][index]) \
	       + '\t' + str(item['SLG'][index]) \
	       + '\t' + str(item['OPS'][index]) \
	       + '\t' + str(item['K_9'][index]) \
	       + '\t' + str(item['BB_9'][index]) \
	       + '\t' + str(item['H_9'][index]) \
	       + '\t' + str(item['K_BB'][index]) \
	       + '\t' + str(item['P_IP'][index]) \
               + '\n'
           self.file.write(line)
           return item



        # class ValidateItemPipeline(object):
        #	def process_item(self, item, spider):
        #		if not all(item.values()):
        #			raise DropItem("Missing values!")
        #		else:
        #			return item


        # class MongoDBPipeline(object):
        #    def __init__(self):
        #        connection = pymongo.MongoClient(
        #            settings['MONGODB_SERVER'],
        #            settings['MONGODB_PORT']
        #        )
        #        db = connection[settings['MONGODB_DB']]
        #        self.collection = db[settings['MONGODB_COLLECTION']]

        #    def process_item(self, item, spider):
        #        self.collection.insert(dict(item))
        #        log.msg("Added to MongoDB database!",
        #                level=log.DEBUG, spider=spider)
        #        return item
