# -*- coding: utf-8 -*-

# Define here the models for your scraped items
#
# See documentation in:
# http://doc.scrapy.org/en/latest/topics/items.html

from scrapy import Item, Field


class MoviesItem(Item):
    RDate = Field()
    Title = Field()
    PBudget = Field()
    DomesticG = Field()
    WorldwideG = Field()
