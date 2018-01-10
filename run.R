source("init.R")

### Pipeline
refuse = import_refuse() %>% process_refuse()

### Export
write.csv(bcs$all, paste("../data/bcs_all_processed_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv", sep=""), row.names = FALSE)
