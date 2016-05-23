num_chapters = [73, 70, 82, 46, 73]       #number of chapters in each book

#urls are of the form CHAPTER_NUM/BOOK_NUM
#CHAPTER_NUMS are 001, 002, ..., 009, 010, 011, etc.
#First form CHAPTER_NUMS
chapters = [['00' + str(j) for j in range(1,10)] + ['0' + str(j) 
            for j in range(10, num_chapters[i] + 1)] for i in range(5)]
            
#Then form urls by book
urls = [map(lambda x: 'http://towerofthehand.com/books/10' + str(i + 1) + '/' + x + '/index.html', 
     chapters[i]) for i in range(5)]
urls = [url for sublist in urls for url in sublist]  #unpack list

#Write to urls.txt
with open('urls.txt', 'w') as f:
    for url in urls:
        f.write(url + '\n')