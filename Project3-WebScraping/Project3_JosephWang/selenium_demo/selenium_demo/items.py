# -*- coding: utf-8 -*-

# Define here the models for your scraped items
#
# See documentation in:
# http://doc.scrapy.org/en/latest/topics/items.html

from scrapy import Item, Field


class MoviesItem(Item):
    # define the fields for your item here like:
    # name = scrapy.Field()

    Player = Field()
    W = Field()
    L = Field()
    IP = Field()
    ERA = Field()
    R  = Field()
    ER = Field()
    HR = Field()
    WHIP = Field()
    WPCT = Field()
    GO_AO = Field()
    OBP = Field()
    SLG = Field()
    OPS = Field()
    K_9 = Field()
    BB_9 = Field()
    H_9 = Field()
    K_BB = Field()
    P_IP = Field()




