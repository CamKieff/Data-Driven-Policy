library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(randomForestSRC)
library(survival)

setwd("~/GitHub/Data-Driven-Policy/SupplementPredictions")

# ------------------------------------- Import data --------------------------------------------

products <- read.csv("drugsAtFDA_Local/Products.txt", 
                     sep = "\t", header = TRUE)

submissions <- read.csv("drugsAtFDA_Local/Submissions.txt", 
                        sep = "\t", header = TRUE)

app_docs <- read.csv("drugsAtFDA_Local/ApplicationDocs.txt",
                     sep = "\t", header = TRUE)

applications <- read.csv("drugsAtFDA_Local/Applications.txt",
                         sep = "\t", header = TRUE) %>% select(-ApplPublicNotes)

company_names <- read.csv('unique_sponsors_2012.csv', header = TRUE)
employees <- read.csv("2019_pharma_employees.csv", header = TRUE)

# minor text and date formatting
app_docs$SubmissionType <- trimws(app_docs$SubmissionType)
# app_docs$ApplicationDocsDate <- as.Date(app_docs$ApplicationDocsDate)
submissions$SubmissionStatusDate <- as.Date(submissions$SubmissionStatusDate)

# --------------------------------------- Combine Datasets --------------------------------------
start_date <- "2012-01-01"
end_date <- Sys.Date()
  
working_df <- submissions %>%
  filter(SubmissionStatusDate >= as.Date(start_date) & 
           SubmissionStatusDate < as.Date(end_date)) %>%
  select(-SubmissionsPublicNotes, -SubmissionStatus)
  # filter(SubmissionType == "ORIG") %>%
  # filter(SubmissionClassCodeID == 7 | SubmissionClassCodeID == 8)

# combine all the data.frames together
working_df <- left_join(working_df, app_docs[,c("ApplNo", "ApplicationDocsTypeID")], by = "ApplNo")
working_df <- left_join(working_df, products, by = "ApplNo")
working_df <- left_join(working_df, applications, by = "ApplNo")
working_df <- left_join(working_df, company_names, by = c("SponsorName" = "NAME")) # this causes a harmless warning about charactesr and factors
working_df <- left_join(working_df, employees, by = "NAME_EDIT")

working_df <- working_df %>% filter(ApplType != "ANDA") %>% # remove ANDAs leaving only BLA and NDA
  select(-ApplicationDocsTypeID, -ProductNo, -ReferenceDrug, -ReferenceStandard, -DrugName, -ActiveIngredient, -Form, -Strength) #remove exces columns to help with duplicate removal

working_df <- distinct(working_df) # delete duplicate rows

orig_df <- working_df %>% filter(SubmissionType == "ORIG") # create Data.frame of original applications

dupe_df <- orig_df[orig_df$SubmissionNo == 2,]
for (i in 1:nrow(dupe_df)){
  # first test if there are two apps in orig_df that have the same applno
  # delete duplicated original applications
}


#create new row in working_df of originial submission date matched by ApplNo

# ------------------------------------- Random Forest Survival -----------------------------------

# manually create test data
set.seed(1606)
training_row <- sample(round(nrow(df)*0.7, 0), replace = FALSE)
training.data <- df[training_row, ]
test.data <- df[-training_row,]

z <- Surv() # create survival analysis object that Surv(time, status)

# rfimpute() # impute data if needed

fit <- rfsrc(z ~ ., data = training.data, importance = TRUE)
plot(fit)
