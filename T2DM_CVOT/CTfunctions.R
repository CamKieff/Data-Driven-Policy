# functions for use with T2DM_yoy.R and T2DM_regression.R for formatting and doing data analysis
# of ClinicalTrials.gov data

require(plyr)
require(dplyr)
require(lubridate)

# Pharmacetuical companies with greater than $10B market cap as of 2017. See link:
# https://gfmasset.com/2017/01/the-200-largest-drug-pharmaceutical-companies-by-market-cap-2017/
# plus boehringer ingelheim and samsung

big_pharma <- c("Johnson", "Pfizer", "Roche", "Genentech", "Boehringer Ingelheim", "Novartis", 
                "Merck", "Sanofi", "Genzyme", "Ono Pharma",
                "AbbVie", "Bristol-Myers Squibb", "Bristol Myers Squibb", "Bayer", "Lilly", 
                "GlaxoSmithKline", "GSK", "Allergan", "Nordisk", "AstraZeneca", "Biogen", 
                "Abbott", "Shire", "Teva", "CSL", "Takeda", "Astellas", "Alexion", "Zoetis", 
                "Otsuka", "Mylan", "Eisai", "Shionogi", "Chugai", "Jiangsu", "Biomarin", 
                "Daiichi", "Kangmei", "Grifols", "UCB", "Perrigo", "Mitsubishi", "Samsung")
big_pattern <- paste(big_pharma, collapse="|")

# data formatting for clinicaltrials.gov output files.
CTformat <- function(file_name){
  workingdf <- read.csv(file_name, header = TRUE) %>%
    select(1:26) %>%
    "names<-"(c("NCT_NUMBER", "TITLE", "ACRONYM", "STATUS", "STUDY_RESULTS", 
                "CONDITIONS", "INTERVENTIONS", "OUTCOME_MEASURES", "SPONSORS", 
                "GENDER", "AGE", "PHASES", "ENROLLMENT", "FUNDED_BY", "STUDY_TYPE", 
                "STUDY_DESIGN", "OTHER_IDS", "START_DATE", "PRIMARY_COMPLETION_DATE", 
                "COMPLETION_DATE", "FIRST_POSTED", "RESULTS_FIRST_POSTED", 
                "LAST_UPDATE_POSTED", "LOCATIONS", "STUDY_DOCUMENTS", "URL")) %>%                   
    mutate(BIG_PHARMA = grepl(big_pattern, .$SPONSORS, ignore.case = TRUE)) %>% # create a "big pharma" variable
    mutate(INDUSTRY = grepl("Industry", .$FUNDED_BY, ignore.case = TRUE)) %>% # create a variable for industry sponsored trials
    mutate(I_BP_IND = INDUSTRY + BIG_PHARMA) %>% #interaction b/w Big Pharma and Industry: industry/small pharma = 1, industry/big pharma = 2
    filter(grepl("1|2|3", .$PHASES))       # select only trials that are phase 1, 2, or 3                     
  # filter(STATUS == "Completed" | STATUS == "Recruiting" | STATUS == "Active, not recruiting") # only select trials with given statuses
  
  # the start dates are in multiple formats. This puts them all in a common date format.
  workingdf$START_DATE <- as.character(workingdf$START_DATE)
  for(i in 1:nrow(workingdf)){                         #some dates dont have days, just months and years, so we append a 1 to each of the shorter dates.
    if(nchar(workingdf$START_DATE[i]) < 8){
      workingdf$START_DATE[i] <- as.character(as.Date(paste0("01-", workingdf$START_DATE[i]), tryFormats = c("%d-%b-%y", "%d-%y-%b")))
    } else(
      workingdf$START_DATE[i] <- as.character(as.Date(workingdf$START_DATE[i], "%d-%b-%y"))
    )
  }
  workingdf$PHASES <- revalue(workingdf$PHASES, c("Phase 1|Phase 2" = "Phase 1", "Phase 2|Phase 3" = "Phase 2", "Early Phase 1" = "Phase 1"))
  workingdf$I_BP_IND <- revalue(as.character(workingdf$I_BP_IND), c("0" = "Non-Industry", "1" = "Small Industry", "2" = "Large Industry"))
  
  workingdf$START_DATE <- as.Date(workingdf$START_DATE, optional = FALSE)
  workingdf$START_DATE <- floor_date(workingdf$START_DATE, "year") #sets dates at year only for easier plotting
  
  return(workingdf)
}

# year-over-year change calculations
yoy_stats <- function(y, time.period = "POST2008"){
  if (time.period == "PRE2008"){
    y %>% filter(START_DATE < "2009-01-01") -> y
  } else if (time.period == "POST2008"){
    y %>% filter(START_DATE >= "2009-01-01") -> y
  } else {
    print("time.period must be PRE2008 or POST2008")
  }
  
  y2 <- ddply(y, "DISEASE", function(x){ x["n"] / c(NA, x["n"][-nrow(x["n"]),]) * 100 -100}) #divide y by y(-1) to get year-over-year change
  y3 <- ddply(y2, "DISEASE", function(x){mean(x$n, na.rm = TRUE)})
  y4 <- ddply(y2, "DISEASE", function(x){sd(x$n, na.rm = TRUE)})
  y5 <- ddply(y2, "DISEASE", function(x){sd(x$n, na.rm = TRUE)/(sqrt(length(x$n)-1))})
  
  y6 <- merge(y3, y4, "DISEASE")
  y7 <- merge(y6, y5, "DISEASE")
  names(y7) <- c("DISEASE", "MEAN", "SD", "SEM")
  return(y7)
}