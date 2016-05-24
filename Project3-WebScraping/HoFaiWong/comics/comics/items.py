# -*- coding: utf-8 -*-
from scrapy import Item, Field

class ComicsItem(Item):
    series = Field()
    publisher = Field()
    release = Field()
    issues_count = Field()
    issues_list = Field()
    avg_rating_critic = Field()
    avg_rating_user = Field()
    series_reviews_critic = Field()
    series_reviews_user = Field()
