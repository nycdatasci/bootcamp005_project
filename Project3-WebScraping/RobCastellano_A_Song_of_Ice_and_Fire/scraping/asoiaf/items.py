# -*- coding: utf-8 -*-

# Define here the models for your scraped items
#
# See documentation in:
# http://doc.scrapy.org/en/latest/topics/items.html

from scrapy import Item, Field


class AsoiafItem(Item):
    # define the fields for your item here like:
    # name = scrapy.Field()

    Appearing = Field()
    POV = Field()
    ChapterNum = Field()
    Book = Field()
    ChapterName = Field()
    Summary = Field()
    Blurb = Field()
    Score = Field()
