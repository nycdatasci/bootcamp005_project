# -*- coding: utf-8 -*-
from scrapy import Spider, Request
from scrapy.selector import Selector
from comics.items import ComicsItem


class ComicsSpider(Spider):
    name = 'comics'
    allowed_urls = ['comicbookroundup.com']

    f = open("urls.txt")
    start_urls = [url.strip() for url in f.readlines()]
    f.close()

    def parse(self, response):
        rows = response.xpath('//*[@id="all-series"]/div/table[2]/tr').extract()
        for row in rows:
            series_url = Selector(text=row).xpath('//td[2]/a/@href').extract()
            yield Request('http://comicbookroundup.com'+series_url[0], callback=self.parse_series_contents)

    def parse_series_contents(self, response):
        item = ComicsItem()

        # Scrape data from the top of the series page
        try:
            item['series'] = str(response.xpath('//span[@itemprop="itemreviewed"]/text()').extract()[0])
        except Exception as e:
            item['series'] = None

        try:
            item['publisher'] = str(response.xpath('//div[@itemprop="description"]/span[1]/a/text()').extract()[0])
        except Exception as e:
            item['publisher'] = None

        try:
            item['release'] = str(response.xpath('//div[@itemprop="description"]/span[2]/text()').extract()[0]).strip()
            if item['release'] == '':
                item['release'] = None
        except Exception as e:
            item['release'] = None

        try:
            item['issues_count'] = int(response.xpath('//div[@itemprop="description"]/span[3]/text()').extract()[0])
        except Exception as e:
            item['issues_count'] = 0

        try:
            item['series_reviews_critic'] = int(response.xpath("//span[@itemprop='votes']/text()").extract()[0])
        except Exception as e:
            item['series_reviews_critic'] = 0

        try:
            item['series_reviews_user'] = int(response.xpath("//span[@itemprop='votes']/../text()").extract()[-1])
        except Exception as e:
            item['series_reviews_user'] = 0

        try:
            item['avg_rating_critic'] = float(response.xpath('//span[@itemprop="average"]/text()').extract()[0])
        except Exception as e:
            item['avg_rating_critic'] = None

        try:
            item['avg_rating_user'] = float(response.xpath('//span[@class="rating-title"]/../text()').extract()[1])
        except Exception as e:
            item['avg_rating_user'] = None

        # Scrape data from table of issues in the series page
        rows = response.xpath('//*[@id="issues"]/div[1]/table[2]/tr').extract()
        item['issues_list'] = {}
        for row in rows:
            try:
                rating_critic = float(Selector(text=row).xpath('//div[@class="CriticRatingList"]/div/text()').extract()[0])
            except Exception as e:
                rating_critic = None

            try:
                rating_user = float(Selector(text=row).xpath('//div[@class="UserRatingList"]/div/text()').extract()[0])
            except Exception as e:
                rating_user = None

            issue = Selector(text=row).xpath('//td[@class="issues"]/a/text()').extract()
            issue = str(issue).replace('.', '-').replace('#','')

            try:
                writer = str(Selector(text=row).xpath('//td[@class="writer"]/a/text()').extract()[0])
            except Exception as e:
                writer = None

            try:
                artist = str(Selector(text=row).xpath('//td[@class="artist"]/a/text()').extract()[0])
            except Exception as e:
                artist = None

            reviews_critic_count = int(Selector(text=row).xpath('//div[@class="CriticReviewNumList"]/a/text()').extract()[0])
            reviews_user_count = int(Selector(text=row).xpath('//div[@class="UserReviewNumList"]/a/text()').extract()[0])

            item['issues_list'][str(issue)] = {'rating_critic': rating_critic,
                                               'rating_user': rating_user,
                                               'writer': writer,
                                               'artist': artist,
                                               'reviews_critic_count': reviews_critic_count,
                                               'reviews_user_count': reviews_user_count}
        yield item