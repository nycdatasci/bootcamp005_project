# -*- coding: utf-8 -*-

# Define here the models for your scraped items
#
# See documentation in:
# http://doc.scrapy.org/en/latest/topics/items.html

from scrapy import Item, Field


class RedditItem(Item):
	link = Field()
	title = Field()
	date = Field()
	upvotes = Field()
	label = Field()
	comments = Field()


