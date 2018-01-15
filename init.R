## Init script
options(java.parameters = "-Xmx8000m")

### library
library(ggplot2)
library(stringr)
library(reshape2)
library(tidyr)
library(dplyr)
library(stringr)
library(knitr)
library(ggmap)
library(RPostgreSQL)
library(tokenizers)
library(topicmodels)
library(corpus)
library(koRpus)
#library(quanteda)
library(textstem)
library(NLP)
library(tm)
library(openNLP)

### local libs

### local files
source("import.R")
source("process.R")
source("functions.R")
source("address_norm.R")
source("doc_processing.R")
