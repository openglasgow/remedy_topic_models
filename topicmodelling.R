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
  "foi",
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
  "inclued",
  "report"
)
drop_terms_long = c(drop_terms, remedy_processed$stopwords_long)
### list of regex's to apply
regexes = c("\\w*\\d[\\w\\d]+", #words with numbers in them
            "\\b[\\w]{1,2}\\b", #single or double character words
            "\\b[\\d]+\\b", # digit only terms
            "\\b(([\\w-]+://?|www[.])[^\\s()<>]+(?:\\([\\w\\d]+\\)|([^[:punct:]\\s]|/)))" # URL's
) 

### Document processing pipeline
processed_docs = corpus_filter(docs, drop_terms_long) %>% regex_remove(regexes)

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
iter <- 2000
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
as.data.frame(ldaOut@gamma)
