# godlonton_scrape
We have a list of news sources and we want to scrape them and all their associated urls for a list of predefined keywords.

This is a tricky problem and I broke it up into two main steps.
# PROCESS
# STEP (1) Iterating through all associated urls
We wanted to go through all the urls for a site, but without a sitemap this is not possible(at least with my knowledge)

I am trying to do this by using selenium to open pages and beautifulsoup4 to parse the pages for links. Through recursion I followed these links for links of their own and so on. However, I am running into some problems with text encodings and using them with beautifulsoup. THIS APPROACH DID NOT WORK SO I MOVED TO R AND RVEST

Instead I used R and the rvest package to select the HTML tags w/href similar to our url root(the base site for the news source) and this worked.

even though this url_traversing and listing worked in R we still have a few issues that we can't get around. 
# url traversal issue #1 
It is slow b/c it grows fast b/c it is kind of a tree structure with the root at the first url and each url from that having it's own subtree that we must iterate through.
# url traversal issue #2
It is likely to be an incomplete listing for the site b/c a site might not link top every one of its pages in its own pages  
# url traversal issue #3
There may be paywalls for some of our sources and unless we get logins for those then we are stuck. 

# STEP (2) Using our associated Urls from (1) and finding/counting all occurences of our keywords
I used the CSS selectors from article sources for every source and in tandem with rvest was able to extract the information we needed in a fairly straightforward way. So now we are returning title, source, url, published date and counts for our specified terms which is exactly what we wanted. I also spent a pretty long time on getting all the CSS selectors for the english sources and formatting the url_roots so that we can actually traverse from the url_root up for all sources.
