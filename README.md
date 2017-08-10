# godlonton_scrape
We have a list of news sources and we want to scrape them and all their associated urls for a list of predefined keywords.

This is a tricky problem and I broke it up into two main steps.
# PLAN
# (1) Iterating through all associated urls
We wanted to go through all the urls for a site, but without a sitemap this is not possible(at least with my knowledge)

I am trying to do this by using selenium to open pages and beautifulsoup4 to parse the pages for links. Through recursion I followed these links for links of their own and so on. However, I am running into some problems with text encodings and using them with beautifulsoup. Also there are several other problems associated with this approach - (1) it is slow b/c it grows fast b/c it is kind of a tree structure with the root at the first url and each url from that having it's own subtree that we must iterate through - (2) it is likely to be an incomplete listing for the site b/c a site might not link top every one of its pages in its own pages - (3) there may be paywalls for some of our sources and unless we get logins for those then we are stuck.

# (2) Using our associated Urls from (1) and finding/counting all occurences of our keywords
This is not a terrible process as long as we know how the site stores its keywords in the HTML files. Every site is different so this had to be done by hand and I have them in hard copy in my notebook.
