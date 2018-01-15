### Process dataset

process_remedy = function(remedy_raw){
  remedy_processed = remedy_raw
  
  cat("importing and string normalizing all datasets \n")
  #import and string normalize datasets
  remedy_processed = lapply(remedy_processed[c("remedy", "geo_lookup")], function(x) import_string_normalize(x))
  
  cat("processing remedy\n")
  remedy_processed$remedy = process_cases(remedy_processed$remedy)
  
  cat("adding geo to remedy and CT\n")
  # remedy_processed = add_geo(remedy_processed)
  
  ##Tokenize description
  description_tokens = tokenize_descriptions(remedy_processed$remedy)
  return(list(remedy=remedy_processed$remedy, tokens=description_tokens))
}

process_cases = function(remedy){
  remedy_processed = remedy
  
  # Remove everything except a few columns
  remedy_processed = remedy_processed %>% select(id, call_id, caller_first_name, caller_second_name, assigned_to, caller_type, agent_login_id, ward_no__decoded_from_ward_name_, 
                                                 host_department, call_topic, call_topic_category_1, call_topic_category_2, call_topic_category_3, details, service_address, call_opened_date, original_create_date, original_create_time, last_modified_by)
  # Normalize address
  remedy_processed = remedy_processed %>% mutate(postcode = as.vector(str_match(service_address, "\\w\\d{1,2}\\s\\d\\w{1,2}")), 
                                                 addr_1 = trim_all(str_match(service_address, "^(.*?)(\\w\\d{1,2}\\s\\d\\w{1,2})(.*?)$")[,c(2)]),
                                                 addr_2 = trim_all(str_match(service_address, "^(.*?)(\\w\\d{1,2}\\s\\d\\w{1,2})(.*?)$")[,c(4)]),
                                                 addr_vec = paste(addr_1, addr_2, sep="")) %>% select(-addr_1, -addr_2)

  # Addresses not really parsable, ignore
  #remedy_processed = cbind(remedy_processed, address_norm(remedy_processed$addr_vec))
  
  # parse dates
  remedy_processed = remedy_processed %>% mutate(call_opened_date = as.Date(str_match(call_opened_date, "(^.*)\\s")[,2], "%Y-%m-%d"),
                              original_create_datetime = as.POSIXct(paste(str_match(original_create_date, "\\d+-\\d+-\\d+"), str_match(original_create_time, "\\d+:\\d+:\\d+")), format="%Y-%m-%d %H:%M:%S"))
  
  # reorder and remove anything I don't need
  remedy_processed = remedy_processed %>% rename(ward_no=ward_no__decoded_from_ward_name_)
  
  # Return
  return(remedy_processed)
  
}

tokenize_descriptions = function(remedy){
  tokens = tokenize_words(remedy$details, stopwords=stopwords("en"))
  return(tokens)
  
}

add_geo = function(ft_processed){
  remedy = ft_processed$remedy
  geo = ft_processed$geo_lookup
  # Bit of processing on geo
  geo = geo %>% select(Postcode, PCSector, PCDistrict, X2001.Output.Area, Datazone, IntZone, Elec_Ward.99, Council_Area)
  colnames(geo) = c("postcode", "pc_sector", "pc_district", "output_area", "datazone", "int_zone", "ward", "council")
  
  # Add geo cols to sets
  remedy = left_join(remedy %>% filter(!is.na(postcode)), geo, by=c("postcode"))
  
  # Assign
  ft_processed$remedy=remedy
  
  return(ft_processed)
}