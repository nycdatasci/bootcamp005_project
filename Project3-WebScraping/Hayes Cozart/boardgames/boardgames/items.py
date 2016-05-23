# -*- coding: utf-8 -*-

# Define here the models for your scraped items
#
# See documentation in:
# http://doc.scrapy.org/en/latest/topics/items.html

import scrapy
from scrapy import Item, Field

class BoardgamesItem(scrapy.Item):
    rank = Field()
    title = Field()
    year = Field()
    gRating = Field()
    avgRating = Field()
    voters = Field()
    ranks = Field()
    typeg = Field()
    category = Field()
    mechanisms = Field()
    age = Field()
    players = Field()
    dificulty = Field()
    language = Field()
    description = Field()
    price = Field()
    timemin = Field()
    timemax = Field()

