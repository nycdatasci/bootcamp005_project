# -*- coding: utf-8 -*-

# Scrapy settings for met project



BOT_NAME = 'met'

SPIDER_MODULES = ['met.spiders']
NEWSPIDER_MODULE = 'met.spiders'

DOWNLOAD_DELAY = 3

#ITEM_PIPELINES = {"met.pipelines.WriteItemPipeline":200}

ITEM_PIPELINES = {"met.pipelines.MongoDBPipeline":100}
#'scrapy.pipelines.images.ImagesPipeline':200


#IMAGES_STORE = '/Users/belinda/DS/bootcamp/prj3/met/imgs'

MONGODB_SERVER = "localhost"
MONGODB_PORT = 27017
MONGODB_DB = "met_museum"
MONGODB_COLLECTION = "met_collection"
