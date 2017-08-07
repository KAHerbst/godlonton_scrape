#in order for this to run we need to have the chrome driver in our directory
#we do this simply moving the exe into the folder and b/c we are using a venv
#we modify our temporary path to be able to search the directory outside the venv
# PATH="$PATH:/Users/konnorherbst/williams/godlonton_scrape/"
import sys
import time
from bs4 import BeautifulSoup
from selenium import webdriver

#how to deal with paywalls

def get_soup(url):
    """
    This opens the selenium browser to the url that we want and builds
    a beautifulsoup tree off this. It should make it so that we are not
    blocked from scraping
    """
    #we want to make sure that we connected to the url fine
    try:
        driver.get(url)
    #there was a problem loading the page so we retur
    except:
        return(None)
    html = driver.page_source
    soup = BeautifulSoup(html)
    return soup



def find_internal_urls(url_root, urls):
    """
    @arg urls must be in list form!
    catalogs all urls associated with the url_root from url
    @return a list of all urls that are associated with url_root in urls
    """
    all_matched_urls = []
    #we build our soup object off url and
    #find all the html links in the page
    for url in urls:
        soup = get_soup(url)
        #soup didn't run into problems loading
        if soup != None:
            a_tags = soup.findAll("a", href=True)
            #we filter our tag listing to sites associated with url_root
            for a_tag in a_tags:
                #if our url_root is in the href we want to record that
                #but we don't want to record links to the url that we are on
                if url_root in str(a_tag["href"]) and str(a_tag["href"]) != url:
                    url_found =  a_tag['href']
                    #.encode(originalEncoding)
                    all_matched_urls.append(url_found)
                else:
                    continue
        else:
            return None
    return all_matched_urls

def site_associated_links(url_root):
    """
    finds all associated links (using recursion) for a url_root.
    Effectively traversing the entire site and cataloging the urls
    """
    #the container for our urls of a site
    associated_url_list = []
    #we find all the links on the url_root that have tags like the url_root
    urls = find_internal_urls(url_root,[url_root])
    level = 1
    #I don't want to go too far in testing
    while urls != None and level < 10:
        print(level)
        associated_url_list.append(urls)
        time.sleep(.75)
        urls = find_internal_urls(url_root, urls)
        level += 1
    return associated_url_list


if __name__ == "__main__":
    #we want to initialize the driver
    driver = webdriver.Firefox()
    print(sum(site_associated_links("https://www.washingtonpost.com/"), []))
#this entire process has a tree like structure, so it can get slow quite fast
#even in the most basic case, where each page has 10 urls that match on it
#the problem can require 10^(i) computations to finish the ith level

#i do no think this will work because of a variety of problems
#(1) most sites won't let you go to more than a certain number of articles unless you pay
#unless I login for all of these sites then it is impossible to get around this
#(2) the linking would never be complete
#(3) the process would take a stupid long time
#(4) beautifulsoup is returing unicode values and I haven't figured out how to work around that