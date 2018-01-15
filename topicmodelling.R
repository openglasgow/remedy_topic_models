# Topic modelling remedy cases
source("init.R")

remedy_processed = import_remedy() %>% process_remedy()

### Set up remedy
remedy = remedy_processed$remedy

### process documents
docs = unlist(lapply(remedy_processed$tokens, function(x) paste(x, collapse=" ")))

# Terms to remove
drop_terms = c(
  "information",
  "freedom",
  "request", 
  "council", 
  "glasgow", 
  "city",
  "details",
  "scotland",
  "dear", 
  "thanks",
  "notify",
  "please",
  "ref", 
  "foi",
  "letter",
  "provide",
  "record",
  "copy",
  "received",
  "forward",
  "refer",
  "received",
  "confidential",
  "response",
  "issue",
  "public",
  "act",
  "email",
  "service",
  "address",
  "attached",
  "message",
  "http",
  "gov",
  "office",
  "mail",
  "attach",
  "include",
  "included",
  "report",
  "detail",
  "communication",
  "required",
  "sir"
)
drop_terms_long = c(drop_terms, remedy_processed$stopwords_long)
### list of regex's to apply
regexes = c("\\b.*?\\..*?\\b", # Anything with a dot in it
            "\\w*\\d[\\w\\d]+", #words with numbers in them
            "\\b[\\w]{1,2}\\b", #single or double character words
            "\\b[\\d]+\\b", # digit only terms
            "\\b(([\\w-]+://?|www[.])[^\\s()<>]+(?:\\([\\w\\d]+\\)|([^[:punct:]\\s]|/)))" # URL's
) 

pos_types = c("NN", "NNS", "NNP", "NNPS", "JJ", "JJS", "JJR")

### Document processing pipeline
processed_docs = corpus_filter(docs, drop_terms_long, stem=F) %>% 
                  regex_remove(regexes) %>% 
                  strip_low_freq(5) %>% 
                  lemmatize_docs() %>% 
                  pos_strip(pos_types) %>% 
                  corpus_filter(drop_terms_long, stem=T)

### Add to remedy
remedy$details_doc = processed_docs

### Remove cases with no terms
remedy_sliced = remedy %>% filter(nchar(details_doc) >0)

### Temp - sample list
remedy_sample = remedy_sliced[sample(nrow(remedy_sliced), 500),]

# Generate docs
docs = Corpus(VectorSource(remedy_sliced$details_doc))
dtm <- DocumentTermMatrix(docs)

#Set parameters for Gibbs sampling
burnin <- 4000
iter <- 150000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE
verbose = 0

#Number of topics
k <- 15

#Run LDA using Gibbs sampling
ldaOut <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin, alpha=20/k, verbose=verbose), beta=0.1)


### Topics
remedy_sliced$topic = topics(ldaOut)

### Terms in topic
terms = terms(ldaOut, 7)

### Topic probabilities
topic_probs = as.data.frame(ldaOut@gamma)

### Map topic numbers to words
topic_titles = list("1" = "what do we know", 
                    "2" = "social care contracts", 
                    "3" = "parliamentary work", 
                    "4" = "property and planning applications", 
                    "5" = "vehicle parking charge notices", 
                    "6" = "mail words",
                    "7" = "road maintenance",
                    "8" = "data transfer words",
                    "9" = "local school information",
                    "10" = "council jobs & pay",
                    "11" = "generic business words",
                    "12" = "registered companies",
                    "13" = "documents on work carried out",
                    "14" = "office words",
                    "15" = "accident legal claims")

### Apply topic names
remedy_sliced = remedy_sliced %>% mutate(primary_topic = unlist(topic_titles[as.character(topic)]))

### Add secondary and tertiary topics and probabilities
sort_row = function(row){ sorted_row = gsub("V", "", names(sort(row, TRUE))) }
get_probs = function(row){ sort(row, T)}

topic_rankings = t(apply(topic_probs, 1, sort_row))
topic_rankings_prob = t(apply(topic_probs, 1, get_probs))

secondary_tertiary_topics = data.frame(t(sapply(1:nrow(remedy_sliced), function(x) {
  ### set up vars
  topic = remedy_sliced[x,"topic"]
  topic_ranking_sorted = topic_rankings[x,]
  topic_prob_sorted = topic_rankings_prob[x,]
  ### Compare  
  if(as.numeric(topic_ranking_sorted[2]) == topic){
    c(topic_titles[topic_ranking_sorted[1]], topic_titles[topic_ranking_sorted[3]], topic_prob_sorted[1:3])
  } else {
    c(topic_titles[topic_ranking_sorted[2]], topic_titles[topic_ranking_sorted[3]], topic_prob_sorted[1:3])
  }
})))
colnames(secondary_tertiary_topics) = c("secondary_topic", "tertiary_topic", "primary_prob", "secondary_prob", "tertiary_prob")
secondary_tertiary_topics = data.frame(lapply(secondary_tertiary_topics, function(x) unlist(x)))
### Bind topics and probs
remedy_sliced = cbind(remedy_sliced, secondary_tertiary_topics)

