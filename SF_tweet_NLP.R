options(stringsAsFactors = F)
tweets <- read.csv("city_of_SF_tweets.csv")

# interested in:
  # keywords, hashtags(#), mentions(@), and emojis
# not interested in:
  # hyperlinks, phone numbers, addresses

pre_process_corpus <- function(data, text_col, replace_emojis = FALSE, replace_numbers = FALSE,
                               replace_strings = NULL, remove_strings = NULL, non_stopwords = NULL,
                               extra_stopwords = NULL, root_gen = NULL){
  
  text <- data[, text_col]
  
  # replacing contractions
  text <- replace_contraction(text)
  
  # replace emojis
  if(replace_emojis == T){
    text <- replace_emoji(text)
  }
  
  # replace numbers
  if(replace_numbers == T){
    text <- replace_number(text)
  }
  
  # converting all text to lower case
  text <- tolower(text)
  
  # removing non-ascii characters
  text <- gsub("[^\001-\177]",'', text, perl = TRUE)
  
  # replace specific strings
  if(!is.null(replace_strings)){
    old_str <- replace_strings[1:(length(replace_strings)/2)]
    new_str <- replace_strings[((length(replace_strings)/2) +1):length(replace_strings)]
    for(i in 1:length(old_str)){text <- gsub(old_str[i], new_str[i], text)}
  }
  
  # removing specific strings
  if(!is.null(remove_strings)){
    strs <- paste(remove_strings, collapse = "|")
    
    text <- strsplit(text, " ")
    text <- unlist(lapply(text, function(x) {
      paste(x[!grepl(strs, x)], collapse = " ")
    }))
  }
  
  # converting to volatile corpus
  text <- VCorpus(VectorSource(text))
  
  # removing terms from stopword dictionary
  stopwords <- stopwords()
  stopwords <- stopwords[which(!stopwords %in% non_stopwords)]
  
  # adding stopwords
  stopwords <- c(stopwords,extra_stopwords)
  
  # removing stopwords
  text <- tm_map(text, function(x) {removeWords(x,stopwords)})
  
  # removing punctuation, numbers, and whitespace
  text <- tm_map(text, function(x) {removePunctuation(x)})
  text <- tm_map(text, function(x) {removeNumbers(x)})
  text <- tm_map(text, function(x) {stripWhitespace(x)})
  
  # generating term roots
  if(!is.null(root_gen)){
    if(root_gen == 'stem'){
      text <- ldply(lapply(text, function(x) {stem_strings(x$content)}), rbind)[, 2]
      text <- VCorpus(VectorSource(text))
    }
    
    if(root_gen == 'lemmatize'){
      text <- ldply(lapply(text, function(x) {lemmatize_strings(x$content)}), rbind)[, 2]
      text <- VCorpus(VectorSource(text))
    }
  }
  
  return(text)
  
}

tmp <- pre_process_corpus(tweets, "Message", replace_emojis = T, remove_strings = "http",
                          replace_strings = c('#', '@', 'hashtag ', 'mention '), root_gen = 'lemmatize')
lapply(tmp, function(x) {x$content})
