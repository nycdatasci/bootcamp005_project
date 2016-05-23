# -*- coding: utf-8 -*-
import scrapy
from scrapy import Item, Field

class MetItem(Item):
    # piece_name = Field()
    # artist_name = Field()
    # piece_date = Field()
    # accession_num = Field()
    # medium = Field()
    # on_view = Field()
    # img_title = Field()
    # image_urls = Field()
    # images = Field()
    title = Field()
    location = Field()
    artifact_detail = Field()