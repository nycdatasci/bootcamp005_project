from scrapy import Request, Spider
from scrapy.selector import Selector
from demo.items import DemoItem


class DemoSpider(Spider):
    name = 'demo'
    allowed_urls = ['http://www.onthesetofnewyork.com/']
    start_urls = ['http://onthesetofnewyork.com/filmlocations-a-z.html']

    def parse(self, response):
        movies_1 = response.xpath('//td[@width="313"]').extract()[0]
        movie_links1 = Selector(text=movies_1).xpath('//a/@href').extract()

        movies_2 = response.xpath('//td[@width="252"]').extract()[0]
        movie_links2 = Selector(text=movies_2).xpath('//a/@href').extract()

        movie_links1.extend(movie_links2)
        for link in movie_links1:
            next_link = 'http://www.onthesetofnewyork.com/' + str(link)
            yield Request(next_link, callback=self.other_parse_meth)

    # for movie in movies_1:
    #	print Selector.xpath('//td[@width="313"]/a/@href').extract()

    def other_parse_meth(self, response):
        title = response.xpath('//h2/text()').extract()[0]
        #print title
        location = response.xpath('//img[@width="10"]/../b/text()').extract()
        #print location

        item = DemoItem()
        item['title'] = title
        item['location'] = location
        yield item

