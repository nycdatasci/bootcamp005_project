# -*- coding: utf-8 -*-

# Define here the models for your scraped items
#
# See documentation in:
# http://doc.scrapy.org/en/latest/topics/items.html

from scrapy import Item, Field

class IndeedItem(Item):

    job_title = Field()
    company_name = Field()
    location = Field()
    job_type = Field()
    job_salary = Field()
   

