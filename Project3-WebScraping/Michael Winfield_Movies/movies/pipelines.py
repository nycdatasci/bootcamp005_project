# -*- coding: utf-8 -*-

# Define your item pipelines here
#
# Don't forget to add your pipeline to the ITEM_PIPELINES setting
# See: http://doc.scrapy.org/en/latest/topics/item-pipeline.html
#simply crawl with -o csv, like:
#scrapy crawl <spider name> -o file.csv -t csv

class MoviesPipeline(object):
    def __init__(self):
        self.file1 = open('5000films.txt', 'w')
        self.test = 1

    def process_item(self, item, spider):
        line = str(item['RDate'][0]) + '\t' + str(item['Title'][0]) + '\t' + str(item['PBudget'][0]) + '\t' + str(item['DomesticG'][0]) + '\t' + str(item['WorldwideG'][0]) + '\n'
        self.file1.write(line)
        return item