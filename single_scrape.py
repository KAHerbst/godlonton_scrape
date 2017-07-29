### Konnor Herbst
### Williams College
### A collection of scraping functions for different news sources
### Copyright 2017

#import the library used to query a website
from urllib.request import Request, urlopen
from bs4 import BeautifulSoup
import re
from textblob import TextBlob
import pandas as pd


#our specified search terms
#pair ("domestic" and "violence") along with ("mental" and "health")
termSet = ["domestic","violence", "abuse", "suicide", "mental", "health","depression"]

#provides us a convenient way to index through our termSet to check for inclusion
#and a way to programmatically keep track of counts
def term_counts(terms):
    termSet_indices = []
    for i in range((len(termSet))):
        termSet_indices.append(i)
    counts = []
    for i in range(len(termSet_indices)):
        counts.append(0)

    return(list(zip(termSet_indices,counts)))

print(term_counts(termSet)[1][0])
#the justification for breaking them up into seperate functions is performance. We won't need to check every if
#statement if we know the publisher off the bat, which we will!

#Arab News source
def single_scrape_arab_news(url,terms,migrant):
    """Scrapes the specified HTML file (built to search news articles) specified by the url argument and checks for the any of the specified terms AND the migrant terms and None or returns the publisher, date, title and author or the article in array form"""
    
    #opens the url specified and renders it into a python tree (with BeautifulSoup)for easy extraction
    req = Request(url, headers={'User-Agent': 'Mozilla/5.0'})
    page = urlopen(req).read()
    soup = BeautifulSoup(page,'lxml')

    enumerated_terms = term_counts(terms)
#tuples are immutable in python so we replace it each time with an updated one
    if(migrant == False):
        article_text = TextBlob(soup.get_text()).sentences
        for text in article_text:
            for index in range(len(enumerated_terms)):
                #I want to find a way to combiune the searches of domestic and violence AND mental and health
                if(terms[enumerated_terms[index][0]] in str(text)):
                    enumerated_terms[index][1] = enumerated_terms[index][1] + 1

                    title = soup.title.string
                    
                    date_raw = str(soup.find_all("time"))
                    date_span = re.search("\d{1}\s{1}\w+\s{1}\d{4}",date_raw).span()
                    date = date_raw[date_span[0]:date_span[1]]
                    return(["Arab News",date,url,title,enumerated_terms[0][1], enumerated_terms[1][1], enumerated_terms[2][1], enumerated_terms[3][1],enumerated_terms[4][1],enumerated_terms[5][1],enumerated_terms[6][1]])
                else:
                    continue
            return(None)
    else:
        print("entered migrants")
        #opens the url specified and renders it into a python tree (with BeautifulSoup)for easy extraction
        req = Request(url, headers={'User-Agent': 'Mozilla/5.0'})
        page = urlopen(req).read()
        soup = BeautifulSoup(page,'lxml')
    
        article_text = TextBlob(soup.get_text()).sentences
        for text in article_text:
            if(any(term in str(text) for term in terms) and ("migrant" in text)):
                title = soup.title.string
                
                date_raw = str(soup.find_all("time"))
                date_span = re.search("\d{1}\s{1}\w+\s{1}\d{4}",date_raw).span()
                date = date_raw[date_span[0]:date_span[1]]
                return(["Arab News",date,title])
            else:
                continue
            return(None)


print(single_scrape_arab_news(url = "http://www.arabnews.com/node/1076526/food-health", terms = termSet, migrant = False))

#So we want to make it so that those arrays are stored by row in a data frame using pandas eventually, below is the code
#headers = ["Publisher", "Date", "Title","Author"]
#df = pd.DataFrame([[columns = headers)
#test = pd.DataFrame([[1,2,3,4]], columns = headers)
#df.append(test)

#unfortunately, if the page happens to link to an article and contains these terms we are fucked
