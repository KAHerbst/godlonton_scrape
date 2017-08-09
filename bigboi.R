#install.packages("rvest")
library(rvest)

#I think that this can be sped up quite a bit by spreading it across a few cores ask the CS department

#'we want to find all the urls that are associated with a url_root
#'(a base site) or a list of urls so that it can be called recursively
#'
#'@param url_root - the base of the url for all the sites that we want to parse(the home site)
#'@param urls(optional unless recursing) - a list of urls that we want to find links that chain directly off
#'
#'@return - all the urls associated with the url_root that base themselves off the url_root OR if urls is specified
#'all the urls found within the pages of urls that base themselves off url_root
find_internal_urls <- function(url_root, urls = NULL){
  non_secure_url_root <- paste0(substr(url_root, 1, 4),substr(url_root, 5, nchar(url_root)))
  print(non_secure_url_root)
  #print(nchar(url_root))
  #we first check if we are just doing the first call (from the base) or not
  if(is.null(urls)){
    root_links <- character()
    #we simply find all the tags from the url_root
    loaded_html <- read_html(url_root)
    possible_root_links <- loaded_html %>% html_nodes("a") %>% html_attr("href")
    for(tag in possible_root_links){
      #print(substr(tag,1,(nchar(url_root) - 1)))
      #print(paste0("the cut tag is: ",tag[1:nchar(url_root)]))
      if(!is.na(tag) & ((substr(tag,1,(nchar(url_root)) ) == url_root) | 
         (substr(tag,1,(nchar(url_root) - 1)) == non_secure_url_root))){
        root_links <- c(root_links, tag)
      }
    }
    return(root_links)
    #if not we are away from the home page and want to check every url in
    #our urls vector for similarity to the url_root and catalog it if so
  }else{
    urls_sub_level <- character()
    for(url in urls){
      tryCatch({loaded_html <- read_html(url)
               possible_tags <- loaded_html %>% html_nodes("a") %>% html_attr("href")},
               warning = function(e){warning("there was a warning thrown")},
               error = function(e){message(paste("URL does not seem to exist:", url))
                                   message("Here's the original error message:")
                                   message(e)
                 return(NA)})
      for(tag in possible_tags){
        if(!is.na(tag) & ((substr(tag,1,(nchar(url_root)) ) == url_root) | 
                          (substr(tag,1,(nchar(url_root) - 1)) == non_secure_url_root))){
          urls_sub_level <- c(urls_sub_level, tag)
        }
      }
    }
    return(urls_sub_level)
  }
}

find_urls <- function(url_root, depth){
  links <- find_internal_urls(url_root = url_root)
  for(level in 1:depth){
    links <- find_internal_urls(url_root = url_root, urls = unique(links[!is.na(links)]))
  }
  return(links)
}

#this is to actually parse the article and find occurences of our keywords
article_analysis <- function(url, term_list, element_selector_list, text_selector){
  loaded <- read_html(url)
  article_text <- loaded_html %>% html_nodes(loaded, text_selector)
  for(selector in article_text){
    titles <- html_nodes(loaded, selector)
    text <- html_text(titles)
  }
    return(text)
}

#i can do a listing of single words and a listing of bound words!
test_terms <- c("king", "arab")
element_selectors <- c("time", "h1")
k <- article_analysis(url = "http://www.arabnews.com/node/1137746/saudi-arabia", )

