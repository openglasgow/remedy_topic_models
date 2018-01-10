### Import data

import_remedy = function(){
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = "remedy", host = "localhost", port = 5432, user = "devonwalshe")
  query = "select * from archive_cases where call_topic LIKE '%FOI%' or call_topic LIKE '%EIR%';"
  cat("Fetching rows from DB \n")
  remedy = dbGetQuery(con, query)
  
  cat("importing geo lookup\n")
  geo_lookup = read.csv('../data/import/small-and-large-user-postcodes.csv', stringsAsFactors = F, strip.white = T, na.strings = c("", "^\\s+$", "N/A", "N / A", "na", "n/a", "n / a"))
  cat("importing stopwords")
  stopwords_long = scan("../data/import/stopwords_long.txt", character(), quote="")
  
  return(list(remedy=remedy, geo_lookup = geo_lookup, stopwords_long = stopwords_long))
}

