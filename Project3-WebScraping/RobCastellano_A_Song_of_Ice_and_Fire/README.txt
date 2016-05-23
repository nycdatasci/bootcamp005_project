This project is based on the A Song of Ice and Fire books.
I scraped data from towerofthehand.com, which contains information on each chapter of the book. This information includes:
  -POV characters
  -Community score
  -Summary
  -Quick blurb
  -Characters appearing
  -Book
  -Chapter name
  -More that I did not scrape
  
The folder scraping/ contains all of the code to scrape. I used scrapy and also used selenium because the community score of 
each chapter is fetched and rendered using javascript.

all_chapters.csv is the data above for every chapter

urls.txt is a list of all of the urls for the series.
urls.py produced the list urls.txt

Presentation/ contains all of the files for my presentation

Project3VisualizationCode.R is the analysis and code to produce graphs

VisualizationGraphs/ contains the graphs produced from Project3VisualizationCode.R

word_clouds/ contains the code to produce wordclouds from the chapter summaries. The code is in WordClouds.ipynb. The word clouds produced are also contained in here. Stencils to make the shapes of these word clouds are also in this folder.

Shiny/ contains a shiny app to visualize all the chapters of a song of ice and fire. You can choose POV characters to see the scores of their chapters.

