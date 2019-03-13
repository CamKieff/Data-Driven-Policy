library(plyr)
library(dplyr)
library(ggplot2)

# ------------------------------------**Load and Format Data**---------------------------------

setwd("~/GitHub/Data-Driven-Policy/PatientExperienceData") #set working directory

# Data extracted from 2018 CDER New Drug Approval Document (https://www.fda.gov/drugs/developmentapprovalprocess/druginnovation/ucm592464.htm)
nov_app <- read.csv("2019-01-31_novel_approvals_withBW.csv", header = TRUE)
nov_app$SubmissionStatusDate <- as.Date(nov_app$SubmissionStatusDate, format = "%m/%d/%Y")

# Download database with application type (NDA or BLA)
temp <- tempfile() 
download.file("https://www.fda.gov/downloads/Drugs/InformationOnDrugs/UCM527389.zip",temp)
applications <- read.csv(unz(temp, "Applications.txt"), sep = "\t", header = TRUE) %>% select(-ApplPublicNotes)
unlink(temp)

# This is PED data extracted from Drugs@FDA; PED Boxes were added manually
expPED <- read.csv("2019-02-08_patientExperience.csv", header= TRUE)
expPED$AppSubmissionDate <- as.Date(expPED$AppSubmissionDate, format = "%m/%d/%Y")
expPED <- left_join(expPED, applications[,c("ApplNo", "ApplType")], by = "ApplNo")

app_PED <- left_join(expPED, nov_app[,-c(1,2,4)], by = c("ApplNo" = "APP_NUM")) %>%
  select(-X, -ApplicationDocsTypeID)

# Format marked boxes for consistency
app_PED$BOX2 <- app_PED$BOX2 + app_PED$BOX3 + app_PED$BOX4 + app_PED$BOX5 + app_PED$BOX6
app_PED$BOX2[app_PED$BOX2 > 0] <-1

app_PED$BOX1 <- app_PED$BOX1 + app_PED$BOX2 + app_PED$BOX7 +  app_PED$BOX8 + 
  app_PED$BOX9 + app_PED$BOX10 + app_PED$BOX11 + app_PED$BOX12
app_PED$BOX1[app_PED$BOX1 > 0] <-1

app_PED$BOX13 <- app_PED$BOX13 + app_PED$BOX14 + app_PED$BOX15 + app_PED$BOX16 + app_PED$BOX17
app_PED$BOX13[app_PED$BOX13 > 0] <-1

# ------------------------------------**Novel Drug Analysis**---------------------------------
app_PED_NME <- app_PED %>% filter(NME == 1) %>% 
  filter(as.Date(SubmissionStatusDate, format = "%m/%d/%Y") >= as.Date("2018-01-01")) %>% 
  mutate(ER = FT + BTD + PR + AA) %>% 
  mutate(num_box = BOX1 + BOX2 + BOX3 + BOX4 + BOX5 + BOX6 + BOX7 + BOX8 + BOX9 + BOX10 + BOX11 
         + BOX12 + BOX13 + BOX14 + BOX15 + BOX16 + BOX17 + BOX18)

nrow(app_PED_NME) # 59 novel drugs

app_PED_NME %>% filter(BOXNA == 0) %>%              # number of novel drugs with PED section (48)
  nrow() -> num_PED

app_PED_NME %>% filter(BOXNA == 0) %>%              # number of BOX18 novel drugs
  select(BOX18) %>% sum() -> num_BOX18

1 - num_BOX18/num_PED                               # percent novel drug reviews with PED (70.8%)

app_PED_NME %>% filter(BOXNA == 0) %>%              # number with Section A and Section B (4)
  filter(BOX1 == 1 & BOX13 == 1) %>% nrow()            
   
app_PED_NME %>% filter(BOXNA == 0) %>%              # number with Section A only (29)
  filter(BOX1 == 1 & BOX13 == 0) %>% nrow()            

app_PED_NME %>% filter(BOXNA == 0) %>%              # number with Section B only (1)
  filter(BOX1 == 0 & BOX13 == 1) %>% nrow()

NDA_PED <- app_PED_NME[app_PED_NME$ApplType == "NDA",] %>% filter(BOXNA == 0) #NDA (73.5%)
1 - sum(NDA_PED$BOX18)/nrow(NDA_PED)

BLA_PED <- app_PED_NME[app_PED_NME$ApplType == "BLA",] %>% filter(BOXNA == 0) #BLA (64.3%)
1 - sum(BLA_PED$BOX18)/nrow(BLA_PED)

ER_PED <- app_PED_NME[app_PED_NME$ER > 0,] %>% filter(BOXNA == 0) #Expedited Review Pathways (64.9%)
1 - sum(ER_PED$BOX18)/nrow(ER_PED)

BW_PED <- app_PED_NME[app_PED_NME$BW == 1, ] %>% filter(BOXNA == 0) # Boxed Warnings (62.5%)
1 - sum(BW_PED$BOX18)/nrow(BW_PED)

app_PED_NME %>% arrange(desc(num_box)) %>%                    #drug with most boxes marked (Crysvita, 8)
   select(DrugName, num_box) %>% head() 

app_PED_NME %>% filter(BOXNA == 0 & BOX18 == 0) %>%           # find mean number of boxes checked (3.6)
  select(num_box) %>% colMeans()

# Patient Reported Outcomes

app_PED_NME %>% filter(BOXNA == 0 & BOX18 == 0) %>%             # Number of drugs with PROs (29/34)
  select(BOX3) %>% sum()

app_PED_NME %>% filter(BOXNA == 0 & BOX18 == 0 & BOX3 == 1) %>% # Reported PED is ONLY PRO (15)
  filter(num_box == 3) %>% nrow()

app_PED_NME %>% filter(BOXNA == 0 & BOX18 == 0 & BOX3 == 1) %>% # Reported PED is PRO and other (14)
  filter(num_box > 3) %>% nrow()

app_PED_NME %>% filter(BOXNA == 0 & BOX18 == 0 & BOX3 == 0) %>% # Reported PED is NOT PRO (5)
    nrow()
  
# Breakdown by Office

app_PED_NME %>% filter(BOXNA == 0) %>%                # calculate Box column sums
  select(13:30) %>% colSums()

# Calculate counts for each box broken down by FDA Office
aggregate(app_PED_NME[app_PED_NME$BOXNA == 0,13:30], by = list(app_PED_NME[app_PED_NME$BOXNA ==0,]$OFFICE), sum)

app_PED_NME %>% filter(BOXNA == 0) %>% # Total counts for each FDA Office
  select(OFFICE) %>% table()

# ------------------------------------**Plots**---------------------------------

# Histogram of the number of boxes marked for each drug
g1 <- (ggplot(app_PED_NME[app_PED_NME$BOXNA == 0 & app_PED_NME$BOX18 == 0, ], aes(x = num_box)) 
+ geom_histogram(binwidth = 1, color="black", fill="darkblue", alpha = 0.5)
+ geom_vline(aes(xintercept=mean(num_box)),color="orange", linetype="dashed", size=1.5)
+ labs(title = "Distribution of Number of Marked Boxes")
+ xlab("Count")
+ ylab("Number of Boxes Marked")
+ scale_y_continuous(breaks = 1:16)
+ scale_x_continuous(breaks = c(1:8, round(mean(app_PED_NME[app_PED_NME$BOXNA == 0 & app_PED_NME$BOX18 == 0, ]$num_box),1)))
+ theme_bw()
+ theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), 
        axis.line = element_line(colour = "black"), panel.border = element_blank(),
        axis.text = element_text(size=14), axis.title = element_text(size=16), 
        plot.title = element_text(size = 24), legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))
)
g1