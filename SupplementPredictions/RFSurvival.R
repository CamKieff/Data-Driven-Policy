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

dupe_df <- orig_df[orig_df$SubmissionNo == 2,] # find potential duplicate submissions

for (i in 1:nrow(dupe_df)){
  
  # first test if there are two applications in orig_df that have the same ApplNo
  if(nrow(orig_df[orig_df$ApplNo == dupe_df$ApplNo[i],]) > 1){
    
    # delete duplicated original applications
    remove.num <- as.numeric(rownames(orig_df[orig_df$ApplNo == dupe_df$ApplNo[i] & orig_df$SubmissionNo == 2,]))
    orig_df <- orig_df[-remove.num,]
  }
}

colnames(orig_df)[5] <- "Orig_App_Date"

# combine supplements with the original 
survival_df <- left_join(working_df, orig_df[c("ApplNo", "Orig_App_Date")], by = "ApplNo")
survival_df <- survival_df %>% filter(!is.na(Orig_App_Date)) %>% mutate(DAY_DELAY = SubmissionStatusDate - Orig_App_Date)
survival_df <- survival_df %>% group_by(ApplNo) %>% top_n(-2, wt = DAY_DELAY) %>% ungroup()

survival.df.orig <- survival_df %>% filter(SubmissionType == "ORIG") %>% select(-SubmissionNo, -SubmissionStatusDate, -DAY_DELAY, -SubmissionType)
survival.df.suppl <- survival_df %>% filter(SubmissionType == "SUPPL") %>% select(-Orig_App_Date, -ApplType, -SponsorName, -NAME_EDIT, -EMPLOYEES, -ESTIMATED, -SubmissionType)

new.survival <- left_join(survival.df.orig, survival.df.suppl, by = "ApplNo")
new.survival <- new.survival %>% mutate(SURVIVAL = as.integer(!is.na(DAY_DELAY))) %>% 
  mutate(NDA = as.integer(grepl("NDA", .$ApplType))) %>% select(-ApplType) %>%
  mutate(ORIG_PRIORITY = as.integer(grepl("PRIORITY", .$ReviewPriority.x))) %>% select(-ReviewPriority.x)

# for products without supplements, calculate the number of days since original approval.
# these are the censored values
new.survival[is.na(new.survival$DAY_DELAY),]$DAY_DELAY <- (Sys.Date()-new.survival[is.na(new.survival$DAY_DELAY),]$Orig_App_Date)

new.companies <- unique(new.survival[is.na(new.survival$NAME_EDIT),]$SponsorName)
write.csv(new.companies, "new_company_names.csv")
# ------------------------------------- Random Forest Survival -----------------------------------

df <- new.survival %>% filter(!is.na(EMPLOYEES)) %>% filter(DAY_DELAY > 0) #remove companies without employee values
df$EMPLOYEES <- as.integer(df$EMPLOYEES)
df$DAY_DELAY <- as.integer(df$DAY_DELAY)
df$SubmissionClassCodeID.x <- factor(df$SubmissionClassCodeID.x)
df$NDA <- factor(df$NDA)
df$ORIG_PRIORITY <- factor(df$ORIG_PRIORITY)
str(df)

# manually create test data
set.seed(1606)
training_row <- sample(round(nrow(df)*0.7, 0), replace = FALSE)
training.data <- df[training_row, ]
test.data <- df[-training_row,]

z <- with(training.data, Surv(DAY_DELAY, SURVIVAL)) # create survival analysis object that Surv(time, status)

# rfimpute() # impute data if needed

fit <- rfsrc(Surv(DAY_DELAY, SURVIVAL) ~ ORIG_PRIORITY + NDA + EMPLOYEES, data = as.data.frame(training.data), ntree = 1000, mtry = 2, importance = TRUE)
plot(fit)
fit

pred <- predict.rfsrc(fit, newdata = test.data, na.action = "na.impute")
plot.survival(pred)
