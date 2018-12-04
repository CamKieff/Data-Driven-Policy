require(rvest)
require(plyr)
require(dplyr)
require(readr)

setwd("~/GitHub/Data-Driven-Policy/IrishHealthProducts")

pages <-seq(1:535) # number of pages as of December 1, 2018
linkfirst <- "http://www.hpra.ie/homepage/medicines/medicines-information/find-a-medicine/results?page="
linklast <- "&field=&query="

all_records <- vector("list", length = length(pages))

# scrapes available data from the search results pages and creates a df
for(j in pages){
  webpage <- read_html(paste0(linkfirst, j, linklast))
  results <- webpage %>% html_nodes("tr")
  
  records<-vector("list", length = length(results)-1)
  for (i in 1:(length(results)-1)){
    
    name <- results[[i+1]] %>% html_nodes(".productname") %>% html_text(trim = TRUE)
    
    strength <- results[[i+1]] %>% 
      html_nodes(paste0("#ContentPlaceHolderBody_C001__rptitems__lblstrength_", i-1)) %>% 
      html_text(trim = TRUE)
    
    authdate <- results[[i+1]] %>%
      html_nodes(paste0("#ContentPlaceHolderBody_C001__rptitems__lblauthoriseddate_", i-1)) %>% 
      html_text(trim = TRUE)
    
    licensenum <- results[[i+1]] %>% 
      html_nodes(paste0("#ContentPlaceHolderBody_C001__rptitems__lbllicencenumber_", i-1)) %>% 
      html_text(trim = TRUE)
    
    licenseholder <- results[[i+1]] %>% 
      html_nodes(paste0("#ContentPlaceHolderBody_C001__rptitems__lbllicenceholder_", i-1)) %>% 
      html_text(trim = TRUE)
    
    records[[i]]<-data_frame(name = name, strength = strength, authdate = authdate, licensenum = licensenum, licenseholder = licenseholder)
  }
  
  all_records[[j]] <- bind_rows(records)
}

df <- bind_rows(all_records)
df$authdate <- as.Date(substring(df$authdate, 13), format = "%d/%m/%Y")
df$ACTIVE_COMP <- NA
df$licensenum <- gsub(" ", "", df$licensenum)

medpage <- "http://www.hpra.ie/homepage/medicines/medicines-information/find-a-medicine/results/item?pano="

# opens every page from the search pages results above (~10,000 sites) and extracts an additional
# pieces of information, the active drug ingredient
for (i in 1:nrow(df)){
  webpage <- read_html(paste0(medpage, df$licensenum[i]))
  results <- webpage %>% html_nodes(".item_element") %>%
    gsub(pattern = '<br>', replacement = "|") %>% # replace break tags with pipes to ensure we get every drug component in multi-drug formulations
    gsub(pattern = '<span class=\"item_element\">|</span>', replacement = "") %>%
    substring(1, nchar(.)-1)
  
  df$ACTIVE_COMP[i] <- results[2] #results[2] is the active drug component. There are other components on the page, but many are duplicates of information obtained from the search pages
}

names(df) <- c("NAME", "STRENGTH", "AUTH_DATE", "LICENSE_NUM", "LICENSE_HOLDER", "ACTIVE_COMP")
df$ACTIVE_COMP<- gsub("  ", "", df$ACTIVE_COMP, fixed = TRUE) # remove large spaces

write.csv(df2, "irish_output.csv") # export