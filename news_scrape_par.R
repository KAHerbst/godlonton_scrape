#install.packages("rvest")
#install.pacakges("parallel")
library(doParallel)
library(rvest)
options(stringsAsFactors = FALSE)


#'Finds all the urls that are associated with a url_root
#'(a base site) or a list of urls so that it can be called recursively
#'
#'@param url_root - the base of the url for all the sites that we want to parse(the home site)
#'@param urls(optional unless recursing) - a list of urls that we want to find links that chain directly off
#'
#'@return - all the urls associated with the url_root that base themselves off the url_root OR if urls is specified
#'all the urls found within the pages of urls that base themselves off url_root
find_internal_urls <- function(urls, chain_base = NA){
  #we first check if we are just doing the first call (from the base) or not
  final_url_links <- character()
  for(url in urls){
    found_error <- FALSE
    #we catch any access errors in reading
    tryCatch({
      loaded_html <- read_html(url)
      possible_linked_urls <- loaded_html %>% html_nodes("a") %>% html_attr("href")
    },warning = function(e) {},error = function(e){found_error <<- TRUE})
    if(found_error){next}
    closeAllConnections()
    final_url_links <- c(final_url_links, possible_linked_urls[!is.na(possible_linked_urls)])
  }
  if(!is.na(chain_base)){
    final_url_links <- (paste0(as.character(chain_base), final_url_links))
  }
  return(unique(final_url_links))
}



#'Searches the list of urls HTML for links to follow
#'
#'@param url_root - the base url for the sites that we want to include
#'example: "https://example.com" would be the root for "https://example.com/exampleChain"
#'@param depth - the amount of "layers" that we want to parse i.e. the amount of times we want to 
#'recursively call the function. We have this b/c after a few iteration we can have a massive amount of matches
#'and we might want to limit the search because of time constraints or possibly we aren't finding anything new
#'
#'@return a vector of links that correspond to the links associated with a site
find_urls_single_source <- function(urls, depth, chain_base = NA){
  #first we find the links from the home page
  complete_links <- character()
  level <- 1
  repeat{
    closeAllConnections()
    urls <- find_internal_urls(urls = urls, chain_base = chain_base)
    complete_links <- c(complete_links, unique(urls))
    level <- level + 1
    if(level > depth){
      break
    }
  }
  return(unique(complete_links))
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
article_analysis <- function(url, source, terms, title_selector, published_selector, text_selector){
  if(substr(url,nchar(url)-3,nchar(url)) == ".pdf"){
    print("pdf doc")
    return(NULL)
  }
  #(1) first we build regex for our terms so that they can be accurately searched
  terms_regex <- character()
  for(term in terms){
    terms_regex <- c(terms_regex, paste0("\\W*((?i)", term, "(?-i))\\W*"))
  }
  #(2) next we make an empty list to store our counts
  print("counts")
  term_counts <- vector("list", length(terms))
  names(term_counts) <- terms_regex
  #(3) now we are ready to actually read in the text of our article
  print("loading")
  found_error <- FALSE
  print(paste("error", found_error))
  tryCatch({
    loaded_html <- read_html(url)
    article_text <- loaded_html %>% html_nodes(text_selector) %>% html_text()
  },warning = function(e) {},error = function(e){
    print("before")
    found_error <<- TRUE
    print("after")})
  if(found_error){print("error in opening") 
    return(NULL)}
  closeAllConnections()
  
  #(4) now to check for our terms in the article
  #I want to occurences and counts of singular words and of word combos
  #first we do single words by splitting the text on one space and filtering the regex to single terms
  print("pairing")
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
    print("empty terms")
    return(NULL)
  }
  #we return the source, title, published date, url and term counts if any of our terms are in the article
  names(term_counts) <- terms
  meta_data <- c(source,title,published_date,url)
  names(meta_data) <- c("source","title","publishing date","url")
  print("found")
  return(c(meta_data, term_counts))
}

source_analysis <- function(url, source, terms, title_selector, published_selector, text_selector, chain_base = NA, depth){
  discovered <- find_urls_single_source(urls = url, depth = depth, chain_base = chain_base)
  source_hits <- character()
  for(link in discovered){
    hit <- article_analysis(url = link, source = source, terms = terms, title_selector = title_selector, published_selector = published_selector, text_selector = text_selector)
    if(is.null(hit)){
      print("next")
      next
    }
    print("found one")
    source_hits <- c(source_hits, hit)
  }
  source_hits <- data.frame(matrix(unlist(source_hits), nrow = length(source_hits)/(4 + length(terms)), byrow = TRUE))
  names(source_hits) <- c("Source","Title","Date","URL",terms)
  return(source_hits)
}


source_data <- read.csv("/Users/konnorherbst/williams/godlonton_scrape/english_identifiers.csv", header = TRUE)
#This code is for parallel execution
cores <- detectCores() - 1
cl <- makeCluster(cores)
registerDoParallel(cores = cl)
on.exit(stopCluster(cl))


#I recommend a depth of 3
#Here we are going through all of our sources for hits and returning them in matrix format
combed_list <- list()
foreach(index = 1:nrow(source_data)) %dopar%{
  combed_data[[index]] <- source_analysis(url = source_data[[2]][index], source = source_data[[1]][index], terms = c("prince, saudi", "federal"), title_selector = source_data[[4]][index], published_selector = source_data[[5]][index], text_selector = source_data[[6]][index], chain_base = source_data[[3]][index], depth = 3)
}
