library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(randomForestSRC)
library(survival)
library(fastDummies)

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

company_names <- read.csv('unique_sponsors_2012.csv', header = TRUE) %>% select(-X)

# minor text and date formatting
app_docs$SubmissionType <- trimws(app_docs$SubmissionType)
submissions$SubmissionStatusDate <- as.Date(submissions$SubmissionStatusDate)

# --------------------------------------- Combine Datasets --------------------------------------
start_date <- "2009-01-01"
end_date <- Sys.Date()
  
working_df <- submissions %>%
  filter(SubmissionStatusDate >= as.Date(start_date) & 
           SubmissionStatusDate < as.Date(end_date)) %>%
  select(-SubmissionsPublicNotes, -SubmissionStatus)
  # filter(SubmissionClassCodeID == 7 | SubmissionClassCodeID == 8)

# combine all the data.frames together
working_df <- left_join(working_df, app_docs[,c("ApplNo", "ApplicationDocsTypeID")], by = "ApplNo")
working_df <- left_join(working_df, products, by = "ApplNo")
working_df <- left_join(working_df, applications, by = "ApplNo")
working_df <- left_join(working_df, company_names, by = c("SponsorName" = "NAME")) # this causes a harmless warning about characters and factors

working_df <- working_df %>% 
  filter(ApplType != "ANDA") %>% # remove ANDAs leaving only BLA and NDA
  select(-ApplicationDocsTypeID, -ProductNo, -ReferenceDrug, -ReferenceStandard, -DrugName, -ActiveIngredient, -Form, -Strength) %>% #remove excess columns to help with duplicate removal
  distinct() # delete duplicate rows

orig_df <- working_df %>% # remove a few duplicate ORIG applications 
  filter(SubmissionType == "ORIG") %>%
  group_by(ApplNo) %>%
  top_n(-1, wt = SubmissionNo) %>% 
  ungroup() # create Data.frame of original applications

colnames(orig_df)[5] <- "Orig_App_Date" # rename column

# combine all applications with the unique originals
survival_df <- left_join(working_df, orig_df[c("ApplNo", "Orig_App_Date")], by = "ApplNo") %>% # add original application approval date
  filter(!is.na(Orig_App_Date)) %>% 
  mutate(DAY_DELAY = SubmissionStatusDate - Orig_App_Date) # create DAY_DELAY column

# select only efficacy supplements (SubmissionClassCodeID == 2)
survival.df.orig <- survival_df %>% filter(SubmissionType == "ORIG") 
survival.df.suppl <- survival_df %>% filter(SubmissionType == "SUPPL") %>% 
  filter(SubmissionClassCodeID == 2)
survival_df <- rbind(survival.df.orig, survival.df.suppl)

# select only the original application and the first supplement for each ApplNo, regardless of supplement number
# take the two DAY_DELAY values for each ApplNo that are the lowest. Break ties by submission number
survival_df <- survival_df %>% 
  group_by(ApplNo) %>% 
  top_n(-2, wt = DAY_DELAY) %>% 
  top_n(-2, wt = SubmissionNo) %>% 
  ungroup()

# split the df by submission type again
survival.df.orig <- survival_df %>% filter(SubmissionType == "ORIG") %>% 
  select(-SubmissionNo, -SubmissionStatusDate, -DAY_DELAY, -SubmissionType)
survival.df.suppl <- survival_df %>% filter(SubmissionType == "SUPPL") %>% 
  select(-Orig_App_Date, -ApplType, -SponsorName, -NAME_EDIT, -SubmissionType, -STOCK)

# re-combine df by column
new.survival <- left_join(survival.df.orig, survival.df.suppl, by = "ApplNo")
new.survival <- new.survival %>% mutate(SURVIVAL = as.integer(!is.na(DAY_DELAY))) %>% 
  mutate(NDA = as.integer(grepl("NDA", .$ApplType))) %>%  # Create an NDA variable
  select(-ApplType) %>%
  mutate(ORIG_PRIORITY = as.integer(grepl("PRIORITY", .$ReviewPriority.x))) %>% # create a review priority variable (original application)
  select(-ReviewPriority.x) 

# for products without supplements, calculate the number of days since original approval.
# these will be the censored values in the survival model
new.survival[is.na(new.survival$DAY_DELAY),]$DAY_DELAY <- (Sys.Date()-new.survival[is.na(new.survival$DAY_DELAY),]$Orig_App_Date)

new.products <- products %>% 
  select(ApplNo, DrugName, ActiveIngredient) %>% 
  unique() 
new.products$DrugName <- tolower(new.products$DrugName)
new.products$ActiveIngredient <- tolower(new.products$ActiveIngredient)

new.survival <- left_join(new.survival, new.products, by = "ApplNo") %>% group_by(ApplNo) %>% top_n(-1, wt = DrugName) %>% ungroup()

# ---------------------------------- Company data from Medtrack -------------------------------------------
# 
# RandD <- read.csv('Medtrack/medtrack_RandD_edit.csv', header = TRUE, na.strings=c("","NA"))
# Finances <- read.csv('Medtrack/medtrack_Finances_edit.csv', header = TRUE, na.strings=c("","NA"))
# 
# medtrack_comb <- left_join(Finances, RandD[-1], by = "TICKER")
# 
# surv.medtrack <- left_join(new.survival, medtrack_comb, by = c("STOCK" = "TICKER"))
# 
# # surv.medtrack <- unique(surv.medtrack) %>% 
# #   filter(STOCK != "") 
# 
# surv.medtrack <- unique(surv.medtrack) 
# 
# surv.medtrack$NUM_EMPLOYEES <- as.numeric(sub(",", "",surv.medtrack$NUM_EMPLOYEES))
# surv.medtrack$REVENUE <- as.numeric(sub(",", "",surv.medtrack$REVENUE))
# 
# # currency conversion table
# FX_tbl <- read.csv("currency_table.csv", header = TRUE)
# 
# surv.medtrack <- left_join(surv.medtrack, FX_tbl, by = "FIN_CURRENCY")
# 
# surv.medtrack$MARKET_CAP <- as.numeric(sub(",", "",surv.medtrack$MARKET_CAP)) 
# surv.medtrack$MARKET_CAP_FX <- surv.medtrack$MARKET_CAP * surv.medtrack$EX_RATE

# ---------------------------------- Add Bio Informa Data -------------------------------------------

name_Appl <- new.survival %>% select(ApplNo, DrugName, ActiveIngredient) %>% distinct()
name_Appl$DrugName <- str_remove(name_Appl$DrugName, "\\skit")

bio_informa <- read.csv("bioinforma_all_results2.csv", header = TRUE) %>%
  select(-Likelihood_of_Approval, -Event_Phase, -Current_Phase, -Future_NDA_BLA_Date, -Future_PDUFA_Date)

bio_informa$Actual_US_Approval_Date <- as.Date(bio_informa$Actual_US_Approval_Date, format = "%m/%d/%y")
bio_informa$Drug_Name <- tolower(bio_informa$Drug_Name)

bioinforma_index <- bio_informa %>% 
  select(DrugID, Drug_Name, Generic_Name) %>% 
  distinct()

merged_index <- left_join(name_Appl, bioinforma_index, by = c("DrugName" = "Drug_Name"))

# remove medical gasses and generic drugs. Not strictly necessary but made matching ApplNo and DrugID searching easier
gas_rows <- filter(mutate(merged_index, gas = grepl("nitrogen|oxygen|carbon dioxide|medical air|nitrous oxide|helium", merged_index$DrugName)), gas == TRUE)
merged_index <- anti_join(merged_index, gas_rows, by = "DrugName")
generic_drugs <- merged_index[merged_index$DrugName == merged_index$ActiveIngredient,]
merged_index <- anti_join(merged_index, generic_drugs, by = "DrugName")

#export the merged_index to do some final ApplNo and DrugID matching manually
#write.csv(merged_index, "merged_index.csv")

#import manually edited merged_index_edit.csv
merged_index_e <- read.csv("merged_index_edit.csv", header = TRUE) %>%
  select(ApplNo, DrugID)

merged_final <- rbind(merged_index %>% select(ApplNo, DrugID), merged_index_e)
merged_final <- distinct(merged_final) %>% filter(!is.na(DrugID))

# ---------------------------------- Add Bio Informa Company Data -------------------------------------------

FX_tbl <- read.csv("currency_table.csv", header = TRUE) # foreign exchange table

bio_informa_company <- read.csv("bioinforma_company_results.csv", header = TRUE) %>%
  select(-Number.of.Employees.Qtr, -Number.of.Employees.TTM) %>%
  mutate(Public = as.factor(grepl("Public", .$Company.Type..Public.Private.)))

bio_informa_company <- left_join(bio_informa_company, FX_tbl, by = c("Currency" = "ABREV")) %>%
  select(-FIN_CURRENCY, -Country, -Company.Type..Public.Private.) %>%
  mutate(Market.Cap.FX = Market.Cap * EX_RATE)

bio_informa_company2 <- read.csv("bioinforma_company_results2.csv", header = TRUE) %>%
  left_join(FX_tbl, by = c("Currency" = "ABREV")) %>%
  select(Company.ID, EX_RATE, EBITDA.Ann, Total.Revenue.Ann) %>%
  mutate(EBITDA.FX = EBITDA.Ann * EX_RATE) %>%
  mutate(Total.Revenue.FX = Total.Revenue.Ann * EX_RATE) %>%
  select(Company.ID, EBITDA.FX, Total.Revenue.FX)
bio_informa_company2[bio_informa_company2 == 0] <- NA

bio_informa_company <- left_join(bio_informa_company, bio_informa_company2, by = "Company.ID") %>% distinct()


# ---------------------------------- Combine Bio Informa and FDA Data -------------------------------------------

merged.data <- left_join(new.survival, merged_final, by = "ApplNo")
merged.data <- left_join(merged.data, bio_informa, by = "DrugID") 
merged.data <- left_join(merged.data, bio_informa_company, by = c("LeadCompanyID" = "Company.ID")) %>% distinct()
merged.data$LeadCompanyID <- as.factor(merged.data$LeadCompanyID)

df <- merged.data %>% 
  filter(Lead_Indication == "Y") %>% # only select the lead indication
  filter(DAY_DELAY >= 0) # ensure DAY_DELAY is positive

df <- left_join(df, as.data.frame(table(df$LeadCompanyID)), by = c("LeadCompanyID" = "Var1"))
names(df)[length(names(df))] <- "Company_Drug_Count"

df$DAY_DELAY <- as.integer(df$DAY_DELAY)
df$SubmissionClassCodeID.x <- factor(df$SubmissionClassCodeID.x) # turn these columns into classification values rather than numbers
df$SubmissionClassCodeID.y <- factor(df$SubmissionClassCodeID.y)
df$NDA <- factor(df$NDA)
df$ORIG_PRIORITY <- factor(df$ORIG_PRIORITY)

levels(df$Route_of_Administration)[levels(df$Route_of_Administration) == ""] <- NA
levels(df$Route_of_Administration)[levels(df$Route_of_Administration) == "N/A"] <- NA

#df <- df[!is.na(df$Route_of_Administration), ]
#df <- df[!is.na(df$SubmissionClassCodeID.x),]
#df <- df[!is.na(df$Public),]

levels(df$Disease_Group)[levels(df$Disease_Group) == "Rheumatology (non autoimmune)" ] <- "Rheumatology"
levels(df$Disease_Group)[levels(df$Disease_Group) == "Gastroenterology (non inflammatory bowel disease)" ] <- "Gastroenterology"

levels(df$Drug_Classification)[levels(df$Drug_Classification) == "New Molecular Entity (NME)"] <- "NME"
  
# select columns for the model

df <- df %>% 
  select(ApplNo, DAY_DELAY, SURVIVAL, NDA, ORIG_PRIORITY, Disease_Group, Drug_Classification, 
         Route_of_Administration, Country, SubmissionClassCodeID.x, 
         Fast_Track, Public, Market.Cap.FX, Orphan, Company_Drug_Count)

    # QIDP, Orphan, Total.Revenue.FX, EBITDA.FX, Number.of.Employees.Ann, SPA , 

 df <- df %>%
   mutate(Oral = as.factor(grepl("Oral|Sublingual", .$Route_of_Administration))) %>%
   mutate(Other_ROA = as.factor(grepl("Inhaled|Intranasal|Intratracheal|Intravenous|Intraarticular|Intramuscular|Intradermal|Subcutaneous|Topical|Transdermal|Intracerebral|Intraocular|Intrathecal|Intratumoral|Intratympanic|Intrauterine|Intravaginal|Rectal|Surgical", .$Route_of_Administration)))

   # mutate(Inhaled = as.factor(grepl("Inhaled|Intranasal", .$Route_of_Administration))) %>%
   # mutate(IV = as.factor(grepl("Intravenous", .$Route_of_Administration))) %>%
   # mutate(Oral = as.factor(grepl("Oral|Sublingual", .$Route_of_Administration))) %>%
   # mutate(IM = as.factor(grepl("Intramuscular|Intradermal|Subcutaneous", .$Route_of_Administration))) %>%
   # mutate(Topical = as.factor(grepl("Topical|Transdermal", .$Route_of_Administration))) %>%
   # mutate(Other_ROA = as.factor(grepl("Intratracheal|Intraarticular|Intracerebral|Intraocular|Intrathecal|Intratumoral|Intratympanic|Intrauterine|Intravaginal|Rectal|Surgical", .$Route_of_Administration)))

df$Route_of_Administration <- NULL

# not sure if we need dummy columns. rfsurv should be able to split factors by levels.
# df <- dummy_cols(df, select_columns = "Drug_Classification")#, remove_first_dummy = TRUE)
# df <- dummy_cols(df, select_columns = c("Disease_Group", "Drug_Classification"))
# df$Disease_Group <- NULL
# df$Drug_Classification <- NULL

df <- distinct(df) %>% select(-ApplNo)
# ------------------------------------- Random Forest Survival -----------------------------------

set.seed(1606) # set start point for random number generator

# manually create test data
training_row <- sample(round(nrow(df)*0.8, 0), replace = FALSE)
training.data <- df[training_row, ] # select training data, 80% of all data.
test.data <- df[-training_row,]

# FIT THE MODEL!
fit <- rfsrc(Surv(DAY_DELAY, SURVIVAL) ~ ., data = as.data.frame(training.data), ntree = 1000,
             importance = TRUE, na.action = "na.impute")
# nimpute = 3, samptype = "swr", mtry = 3
plot(fit)
fit

# submission class code for suppl
# use only class 3?

pred <- predict.rfsrc(fit, newdata = test.data, na.action = "na.impute")
plot.survival(pred)
pred

yvar <- fit$yvar
rsf.pred <- fit$predicted.oob
rsf.err <- get.cindex(yvar$DAY_DELAY, yvar$SURVIVAL, rsf.pred)


# ---------------------------------------- Plots ---------------------------------------------

# Density plot
which.max(density(df[df$SURVIVAL== 1,]$DAY_DELAY)$y)
max_density_day <- density(df[df$SURVIVAL== 1,]$DAY_DELAY)$x[129] #1.4 years (493 days); only includes confirmed supplements
median(df$DAY_DELAY)

p <- (ggplot(df[df$SURVIVAL== 1,], aes(x=DAY_DELAY, stat(count))) 
  + geom_density()
  + geom_vline(aes(xintercept=max_density_day), color="blue", linetype="dashed", size=1)
  + xlab("Time since Original Application Approval (Days)")
  + ylab("Probability Density")
  #+ ylim(0,1)
  + theme_bw()
  + scale_colour_brewer(palette = "Set2")
  + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), 
          axis.line = element_line(colour = "black"), panel.border = element_blank(),
          axis.text = element_text(size=14), axis.title = element_text(size=14), 
          plot.title = element_text(size = 24), legend.text = element_text(size = 12),
          legend.title = element_text(size = 14))
)

p 

df$extra <- 1
surv.object <- Surv(df$DAY_DELAY, df$SURVIVAL)

pred.object <- Surv(pred$yvar$DAY_DELAY, pred$yvar$SURVIVAL)

pred.fit.obj <- survfit(pred.object ~ rep(1, length(pred$yvar$SURVIVAL)))
surv.fit.obj <- survfit(surv.object ~ df$extra)

surv.fit.obj <- survfit(surv.object ~ df$Drug_Classification)
plot(surv.fit.obj, col = c("black", "blue", "red", "green"))
# "Biologic"   "Biosimilar" "NME"        "Non-NME"    "Vaccine"   

plot(pred.fit.obj)
df$DAY_DELAY <- as.numeric(df$DAY_DELAY)
library(ggRandomForests)
test_gg_pred <-gg_rfsrc(fit, oob = TRUE, conf.int=.95, surv_type = "surv")
plot(test_gg_pred)
test_gg_fit <- gg_survival(interval = "DAY_DELAY", censor = "SURVIVAL", data = as.data.frame(df), conf.int = 0.95)
plot(test_gg_fit)

new_fit_df <-as.data.frame(cbind(fit$time.interest, colMeans(fit$survival), apply(fit$survival, 2, sd)))
new_pred_df <- as.data.frame(cbind(pred$time.interest, colMeans(pred$survival), apply(pred$survival, 2, sd)))
names(new_pred_df) <- c("time", "mean", "sd")
names(new_fit_df) <- c("time", "mean", "sd")

new_fit_df <-as.data.frame(cbind(surv.fit.obj$time, surv.fit.obj$surv))
names(new_fit_df) <- c("time", "mean")
new_pred_df$sd <- NULL

new_fit_df$test <- "all data"
new_pred_df$test <- "pred"

comb_df <- rbind(new_fit_df, new_pred_df)

g0 <- (ggplot(data = comb_df, aes(x = time, y = mean, color = test)) 
  + geom_step(size = 1.3)
  #+ labs(title = "Industry-Sponsored T2DM Clinical Trials by Phase")
  + xlab("Time since Original Application Approval (Days)")
  + ylab("Inverse Cum. Probability of 1st Efficacy Supplement (1-%)")
  #+ ylim(0,1)
  + theme_bw()
  + scale_colour_brewer(palette = "Set2")
  + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), 
          axis.line = element_line(colour = "black"), panel.border = element_blank(),
          axis.text = element_text(size=14), axis.title = element_text(size=14), 
          plot.title = element_text(size = 24), legend.text = element_text(size = 12),
          legend.title = element_text(size = 14))
)
g0

new_pred_df <- new_pred_df %>% 
  mutate(upper = mean + sd*1.96/sqrt(nrow(pred$survival))) %>% #don't think these are correct
  mutate(lower = mean - sd*1.96/sqrt(nrow(pred$survival)))
#plot(new_df)


g0 <- ggplot(data = new_pred_df, aes(x = time, y = mean, ymax = upper, ymin = lower)) + geom_ribbon(alpha = 0.3) +geom_line() 
g0
