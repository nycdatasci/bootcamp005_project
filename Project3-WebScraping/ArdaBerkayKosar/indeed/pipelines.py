# -*- coding: utf-8 -*-

# Define your item pipelines here
#
# Don't forget to add your pipeline to the ITEM_PIPELINES setting
# See: http://doc.scrapy.org/en/latest/topics/item-pipeline.html


class WriteItemPipeline(object):
    def __init__(self):
        self.file = open('job_titles.txt', 'w')

    def process_item(self, item, spider):

        line = str(item['job_description']) + " " \
               #  str(item['job_title']) + "," + str(item['company_name'])  + "," + \
               # str(item['location']) + "," + str(item['job_type']) + "," + \
               # str(item['job_salary']) + "\n"
        self.file.write(line)

        return item
