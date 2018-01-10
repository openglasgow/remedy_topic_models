### document processing

# Lets have a look at terms
term_frequency = term_stats(tokens)

corpus_filter = function(docs, drop_words){
  cat("Applying standard text manipulation from the corpus package \n")
  #### corpus library filtering
  filter = text_filter(map_quote=T, stemmer="en", drop_punct=T, drop_number = T, drop_symbol=T, drop=c(drop_words, stopwords_en))
  ### convert docs to right format
  corpus = as_corpus_text(tokens)
  ### Apply filter conditions
  text_filter(corpus) <- filter
  ### Tokenize
  corpus_tokens = text_tokens(corpus)
  ### Return to document format
  docs = unlist(lapply(corpus_tokens, function(x) paste(x, collapse=" ")), recursive=F)
  ### Return
  return(docs)
}


#### manual filtering
regex_remove = function(docs, regex_vector){
  cat("Applying regular expressions to remove tokens from documents\n")
  ### join them into one regex
  regex = paste(regex_vector, collapse="|")
  
  docs = unlist(lapply(docs, function(x) str_trim(str_replace_all(x, regex,  ""))), recursive=F)
  return(docs)
}
### Apply regex
#regexed_docs = regex_remove(filtered_docs)

### Remove low frequency terms
strip_low_freq = function(docs, epsilon){
  cat("Removing terms with low frequency counts\n")
  ### Get term frequencies
  term_freqs = term_stats(docs)
  ### Get all terms with appearances less than epsilon
  terms = term_freqs %>% filter(support < epsilon) %>% .$term
  ### tokenize docs
  tokenized_docs = tokenize_words(docs)
  ### remove them from the documents
  stripped_docs = lapply(tokenized_docs, function(x) setdiff(x, terms))
  ### paste back together
  stripped_docs = unlist(lapply(stripped_docs, function(x) paste(x, collapse=" ")), recursive=F)
  ### Return
  return(stripped_docs)
}

### Apply low freq removal
#high_freq_docs = strip_low_freq(regexed_docs, 10)


