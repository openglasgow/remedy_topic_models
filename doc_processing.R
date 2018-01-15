### document processing

# Lets have a look at terms
#term_frequency = term_stats(tokens)

### Lemmatization & POS tagging
#lemmatize_strings(docs[1:50], dictionary=lexicon::hash_lemmas)

pos_strip = function(docs, pos_types=c()){
  
  tag_and_strip = function(doc, pos_types){
    if(is.na(doc) | doc==""){
      return(doc)
    }
    ## setup annotators
    st_annotator = Maxent_Sent_Token_Annotator()
    wt_annotator = Maxent_Word_Token_Annotator()
    pt_annotator = Maxent_POS_Tag_Annotator()
    ### Map tokens and POS
    char_map = NLP::annotate(doc , list(st_annotator, wt_annotator))
    pos_map = NLP::annotate(doc, list(pt_annotator), char_map) %>% data.frame() %>% filter(type=="word") %>% mutate(features = as.character(unlist(features)))
    pos_map$words = sapply(1:nrow(pos_map), function(x){substr(doc, pos_map[x, "start"], pos_map[x, "end",]) })
    ### Strip unwanted pos types
    stripped_map = pos_map %>% filter(features %in% pos_types)
    sentence = paste(stripped_map$words, collapse = " ")
    return(sentence)
  }
  
  docs_stripped = pblapply(docs, function(x) {tag_and_strip(x, pos_types)}) %>% unlist()

  return(docs_stripped)
}



lemmatize_docs = function(docs){
  lemmatized = pblapply(docs, function(x) lemmatize_strings(x, dictionary=lexicon::hash_lemmas))
  # lemmatized = lemmatize_strings(docs, dictionary=lexicon::hash_lemmas)
  
}

corpus_filter = function(docs, drop_words, stem=F){
  cat("Applying standard text manipulation from the corpus package \n")
  #### corpus library filtering
  filter = text_filter(map_quote=T, drop_punct=T, drop_number = T, drop_symbol=T, drop=union(drop_words, stopwords_en))
  if(stem){
    filter <- text_filter(stemmer="en")
  }
  ### convert docs to right format
  corpus = as_corpus_text(docs)
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
  # Tokenize docs for vectorization
  doc_tokens = tokenize_words(docs)
  
  pb <- txtProgressBar(min = 0, max = length(regex_vector), style = 3)
  # Loop over with regex replace and write over the tokens each time
  for(i in 1:length(regex_vector)){
    doc_tokens = lapply(doc_tokens, function(x) str_replace_all(x, regex_vector[i],  ""))
    setTxtProgressBar(pb, i)
  }
  close(pb)
  # Flatten back to document format
  docs_flattened = unlist(lapply(doc_tokens, function(x) paste(x, collapse=" ")), recursive=F)
  return(docs_flattened)
  
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
  stripped_docs = pblapply(tokenized_docs, function(x) setdiff(x, terms))
  ### paste back together
  stripped_docs = unlist(lapply(stripped_docs, function(x) paste(x, collapse=" ")), recursive=F)
  ### Return
  return(stripped_docs)
}

### Apply low freq removal
#high_freq_docs = strip_low_freq(regexed_docs, 10)


