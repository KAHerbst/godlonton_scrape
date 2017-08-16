#install.packages("rvest")
library(rvest)
options(stringsAsFactors = FALSE)
#I think that this can be sped up quite a bit by spreading it across a few cores ask the CS department

k <- NULL

#'Finds all the urls that are associated with a url_root
#'(a base site) or a list of urls so that it can be called recursively
#'
#'@param url_root - the base of the url for all the sites that we want to parse(the home site)
#'@param urls(optional unless recursing) - a list of urls that we want to find links froms
#'@param chained - tells us whether or not our source links with included roots(TRUE) or without(FALSE)
#'
#'@return - all the urls associated with the url_root that base themselves off the url_root OR if urls is specified
#'all the urls found within the pages of urls that base themselves off url_root
find_internal_urls <- function(url_root, urls = NULL, chained = TRUE){
  non_secure_url_root <- paste0(substr(url_root, 1, 4),substr(url_root, 5, nchar(url_root)))
  #print(non_secure_url_root)
  #we first check if we are just doing the first call (from the base) or not
  if(is.null(urls)){
    root_links <- character()
    #we simply find all the tags from the url_root
    loaded_html <- read_html(url_root)
    possible_root_links <- loaded_html %>% html_nodes("a") %>% html_attr("href")
    for(tag in possible_root_links){
      if(!is.na(tag)){
        root_links <- c(root_links, tag)
      }
    }
    return(root_links)
    #if not we are away from the home page and want to check every url in
    #our urls vector for similarity to the url_root and catalog it if so
  } else{
    urls_sub_level <- character()
    if (chained == TRUE) {
      for (url in urls) {
        possible_tags <- NA
        print("checking rooted")
        #we check those that should include the root
        tryCatch({
          loaded_html <- read_html(url)
          possible_tags <- loaded_html %>% html_nodes("a") %>% html_attr("href")
        },
        warning = function(e) {
          warning("there was a warning thrown")
        },
        error = function(e) {
          message("There was a problem with this link: ")
          message(e)
          possible_tags <<- NA
        })
        if (!is.na(possible_tags)) {
          for (tag in possible_tags) {
            if (!is.na(tag) & ((substr(tag, 1, (
              nchar(url_root)
            )) == url_root) |
            (substr(tag, 1, (
              nchar(url_root) - 1
            )) == non_secure_url_root))) {
              urls_sub_level <- c(urls_sub_level, tag)
            }
          }
        }
      }
    } else{
      #we check to see if the chain is not direct i.e. the root is not included
      #for reasons unknown some of our sources do this?
      for(url in urls){
        possible_tags <- NA
        print("checking unchained")
        tryCatch({
          loaded_html <- read_html(paste0(url_root, url))
          print(paste0(url_root, url))
          possible_tags <- loaded_html %>% html_nodes("a") %>% html_attr("href")
          print("after loaded")
        },
        warning = function(e) {
          warning("there was a warning thrown")
        },
        error = function(e) {
          message("There was a problem with this link: ")
          message(e)
          possible_tags <- NA
        })
        print("after trycatch")

        if (!is.na(possible_tags)) {
          print("in loop")
          for (tag in possible_tags) {
            print("where")
            if (!is.na(tag)) {
              print("here?")
              urls_sub_level <- c(urls_sub_level, tag)
              print("no?")
            }
            print("1")
          }
          print("2")
        }
        print("3")
      }
      print("4")
    }
    print("5")
  }
  print('6')
  return(urls_sub_level)
}



#'Searches the list of urls HTML for links whose root is url_root
#'
#'@param url_root - the base url for the sites that we want to include
#'example: "https://example.com" would be the root for "https://example.com/exampleChain"
#'@param depth - the amount of "layers" that we want to parse i.e. the amount of times we want to 
#'recursively call the function. We have this b/c after a few iteration we can have a massive amount of matches
#'and we might want to limit the search because of time constraints or possibly we aren't finding anything new
#'
#'@return a vector of links that correspond to the links associated with a site
find_urls_single_source <- function(url_root, depth, chained = TRUE){
  links <- find_internal_urls(url_root = url_root, chained = chained)
  for(level in 1:depth){
    links <- find_internal_urls(url_root = url_root, urls = unique(links[!is.na(links)]), chained = chained)
  }
  return(links)
}



#'Searches the text of an article for occurences of our terms and return either NULL if there are none or
#'the title, source, url, date of publishing and term counts if any of the terms are there
#'
#'@param url - the link to the site that we want to search
#'@param source - the name of the source (can be anything it is only for a direct return so we can easily automate)
#'@param terms - a character vector of the terms we want to search for
#'@note - all selectors should be the least restrictive we can find i.e "p" is preferred to "p article"
#'@param title_selector - the CSS selector for the article title
#'@param published_selector - the CSS selector for the publishing date
#'@param text_selector - the CSS selector for the text of the article that we want to parse for our terms
#'
#'@return either NULL if none of our terms are in the article or the title, source, url, 
#'date of publishing and term counts if any of the terms are there
single_article_analysis <- function(url, source, terms, title_selector, published_selector, text_selector){
  #(1) first we build regex for our terms so that they can be accurately searched
  terms_regex <- character()
  for(term in terms){
    terms_regex <- c(terms_regex, paste0("\\W*((?i)", term, "(?-i))\\W*"))
  }
  #(2) next we make an empty list to store our counts
  term_counts <- vector("list", length(terms))
  names(term_counts) <- terms_regex
  #(3) now we are ready to actually read in the text of our article
  loaded_html <- read_html(url)
  article_text <- loaded_html %>% html_nodes(text_selector) %>% html_text()
  #(4) now to check for our terms in the article
  #I want to occurences and counts of singular words and of word combos
  #first we do single words by splitting the text on one space and filtering the regex to single terms
  paired_indices <- grep(" ", terms_regex)
  #so at this point there are three possibilities and we handle them all
  #possibility 1 - there are no paired words in our terms, so length(paired_indices) will be 0
  #possibility 2 - there are no single words in our terms, so length(paired_indices) will be length(terms_regex)
  #possibility 3 - we have a mixture of single and paired words in our term set
  #possiblity 1
  if(length(paired_indices) == 0){
    single_regex <- terms_regex
    single_words_in_article <- unlist(strsplit(article_text, " "))
    for(index in 1:length(single_regex)){
      term_counts[[single_regex[index]]] <- length(grep(single_regex[index], single_words_in_article, value = FALSE))
    }
    #possibility 2
  }else if(length(paired_indices) == length(terms_regex)){
    paired_regex <- terms_regex[paired_indices]
    single_regex <- terms_regex[-(paired_indices)]
    #the words in the article
    single_words_in_article <- unlist(strsplit(article_text, " "))
    odd_words <- single_words_in_article[c(TRUE,FALSE)]
    even_words <- single_words_in_article[c(FALSE,TRUE)]
    #the words in pairs of two in the article
    odd_pairings <- paste(odd_words, even_words)
    even_pairings <- paste(even_words, odd_words)
    for(index in 1:length(paired_regex)){
      term_counts[[paired_regex[index]]] <- length(grep(paired_regex[index], even_pairings, value = FALSE))
      term_counts[[paired_regex[index]]] <- term_counts[[paired_regex[index]]] + length(grep(paired_regex[index], odd_pairings, value = FALSE))
    }
    #possibility 3
  }else{
    paired_regex <- terms_regex[paired_indices]
    single_regex <- terms_regex[-(paired_indices)]
    #the words in the article
    single_words_in_article <- unlist(strsplit(article_text, " "))
    odd_words <- single_words_in_article[c(TRUE,FALSE)]
    even_words <- single_words_in_article[c(FALSE,TRUE)]
    #the words in pairs of two in the article
    odd_pairings <- paste(odd_words, even_words)
    even_pairings <- paste(even_words, odd_words)
    for(index in 1:length(single_regex)){
      term_counts[[single_regex[index]]] <- length(grep(single_regex[index], single_words_in_article, value = FALSE))
    }
    #now I check the word combos
    for(index in 1:length(paired_regex)){
      term_counts[[paired_regex[index]]] <- length(grep(paired_regex[index], even_pairings, value = FALSE))
      term_counts[[paired_regex[index]]] <- term_counts[[paired_regex[index]]] + length(grep(paired_regex[index], odd_pairings, value = FALSE))
    }
  }
  title <- html_nodes(loaded_html, title_selector) %>% html_text()
  published_date <- html_nodes(loaded_html, published_selector) %>% html_text()
  #we return null if we did not find any of our terms
  terms_empty <- all(as.numeric(unlist(term_counts)) == 0)
  if(terms_empty){
    return(NULL)
  }
  #we return the source, title, published date, url and term counts if any of our terms are in the article
  names(term_counts) <- names(terms)
  return(c(source, title, published_date, url, term_counts))
}

m <- NULL

source_analysis <- function(url_root, depth, source, terms, title_selector, published_selector, text_selector, chained = TRUE){
  source_hits <- list()
  print("who who")
  linked_urls <- find_urls_single_source(url_root, depth, chained = chained)
  m <<- linked_urls
  print("what what")
  list_index <- 1
  print("it must be here")
  for(url in linked_urls){
    if(substr(url,1,1) == "#"){
      next
    }
    print(url)
    print("close")
    #it keeps interpreting them as paths to a directory!
    if(chained == TRUE){
      print("what is happening")
      single_article <- single_article_analysis(url = paste0(url_root,url), source = source, terms =terms, title_selector = title_selector, published_selector = published_selector, text_selector = text_selector)
      print("was it that?")
    }else{
      single_article <- single_article_analysis(url = url, source = source, terms = terms, title_selector = title_selector, published_selector = published_selector, text_selector = text_selector)
    }
    print("closer")
    if(!is.null(single_article)){
      source_hits[[list_index]] <- single_article
      list_index <- list_index + 1
    }
  }
  return(source_hits)
}
  
test_time <- Sys.time()
#first we grab all the meta data that we need
source_data <- read.csv("/Users/konnorherbst/williams/godlonton_scrape/english_identifiers.csv", header = TRUE)
names <- source_data[[1]]
roots <- source_data[[2]]
title_selectors <- source_data[[3]]
date_selectors <- source_data[[4]]
text_selectors <- source_data[[5]]
terms_to_search <- c("migrant")
m <- source_analysis(url_root = roots[1], depth = 1, source = names[1], terms = terms_to_search, title_selector = title_selectors[1], published_selector = date_selector[1], text_selector = text_selectors[1], chained = FALSE)
print(paste0("Time to run: ", Sys.time() - test_time))
