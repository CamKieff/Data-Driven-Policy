library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

# ------------------------------------**Load and Format Data**---------------------------------

setwd("~/GitHub/Data-Driven-Policy/PatientExperienceData") #set working directory

#Data extracted from 2018 CDER New Drug Approval Document (https://www.fda.gov/drugs/developmentapprovalprocess/druginnovation/ucm592464.htm)
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

# Adjudicated boxes for three drugs with ambiguous boxes checked
app_PED[app_PED$DrugName == "TAKHZYRO","BOX8"] <- 0 # move box 8 to box 15 (not submitted by sponsor)
app_PED[app_PED$DrugName == "TAKHZYRO","BOX15"] <- 1
app_PED[app_PED$DrugName == "PALYNZIQ","BOX14"] <- 1 # box 13 indicates there was a PKU Alliance meeting 
app_PED[app_PED$DrugName == "PALYNZIQ","BOX15"] <- 1 # there was a Voice of the Patient Report
app_PED[app_PED$DrugName == "XERAVA","BOX5"] <- 1 # COA is marked but the specific COA was not indicated. It is a ClinRO based on analysis of the document.

# ------------------------------------**Novel Drug Analysis**---------------------------------
app_PED_NME <- app_PED %>% filter(NME == 1) %>% 
  filter(as.Date(SubmissionStatusDate, format = "%m/%d/%Y") >= as.Date("2018-01-01")) %>% 
  mutate(ER = FT + BTD + PR + AA) %>% 
  mutate(num_box = BOX1 + BOX2 + BOX3 + BOX4 + BOX5 + BOX6 + BOX7 + BOX8 + BOX9 + BOX10 + BOX11 
         + BOX12 + BOX13 + BOX14 + BOX15 + BOX16 + BOX17 + BOX18) %>%
  mutate(term_box = BOX3 + BOX4 + BOX5 + BOX6 + BOX7 + BOX8 + BOX9 + BOX10 + BOX11
        + BOX12 + BOX14 + BOX15 + BOX16 + BOX17 + BOX18) %>% #terminal box sum, excluding boxes 1,2,13
  mutate(SectionA_notCOA = BOX7 + BOX8 + BOX9 + BOX10 + BOX11 + BOX12) # Non-COA parts of section A variable

write.csv(app_PED_NME, "2019_04-18_formatted_PED_output.csv")

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

app_PED_NME %>% filter(BOXNA == 0 & BOX18 == 0) %>%           # find mean number of terminal boxes marked (1.65)
  select(term_box) %>% colMeans()

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
office.df <- aggregate(app_PED_NME[app_PED_NME$BOXNA == 0,13:30], by = list(app_PED_NME[app_PED_NME$BOXNA ==0,]$OFFICE), sum)
office_tally <- app_PED_NME %>% filter(BOXNA == 0) %>% # Total counts for each FDA Office
  select(OFFICE) %>% table() %>% as.data.frame()

# Calculate counts for each box broken down by FDA Division
DOP_PED_NME <- app_PED_NME
DOP_PED_NME$DIVISION <- str_replace(app_PED_NME$DIVISION, "DOP\\S", "DOP") #combine DOP1 and DOP2
division.df <- aggregate(app_PED_NME[DOP_PED_NME$BOXNA == 0,13:30], by = list(DOP_PED_NME[app_PED_NME$BOXNA ==0,]$DIVISION), sum)
division_tally <- DOP_PED_NME %>% filter(BOXNA == 0) %>% # Total counts for each FDA Division
  select(DIVISION) %>% table() %>% as.data.frame()

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

# Histogram of the number of terminal boxes marked for each drug
g2 <- (ggplot(app_PED_NME[app_PED_NME$BOXNA == 0 & app_PED_NME$BOX18 == 0, ], aes(x = term_box)) 
       + geom_histogram(binwidth = 1, color="black", fill="darkblue", alpha = 0.5)
       + geom_vline(aes(xintercept=mean(term_box)),color="orange", linetype="dashed", size=1.5)
       + labs(title = "Distribution of Number of Terminal Marked Boxes")
       + xlab("Count")
       + ylab("Number of Terminal Boxes Marked")
       + scale_y_continuous(breaks = 1:16)
       + scale_x_continuous(breaks = c(1:8, round(mean(app_PED_NME[app_PED_NME$BOXNA == 0 & app_PED_NME$BOX18 == 0, ]$term_box),2)))
       + theme_bw()
       + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), 
               axis.line = element_line(colour = "black"), panel.border = element_blank(),
               axis.text = element_text(size=14), axis.title = element_text(size=16), 
               plot.title = element_text(size = 24), legend.text = element_text(size = 12),
               legend.title = element_text(size = 14))
)
g2


# format data for heatmap plot
heatmap.office <- as.data.frame(office.df) %>% gather(BOX, NUMBER, -Group.1)
heatmap.division <- as.data.frame(division.df) %>% gather(BOX, NUMBER, -Group.1)

names(office_tally) <- c("OFFICE", "FREQ")
heatmap.office <- left_join(heatmap.office, office_tally, by = c("Group.1" = "OFFICE") )
heatmap.office <- heatmap.office %>% mutate(PERCENT = NUMBER/FREQ*100) %>% filter(Group.1 != "OHOP")

names(division_tally) <- c("DIVISION", "FREQ")
heatmap.division <- left_join(heatmap.division, division_tally, by = c("Group.1" = "DIVISION") )
heatmap.division <- heatmap.division %>% mutate(PERCENT = NUMBER/FREQ*100) %>% filter(Group.1 == "DOP" | Group.1 == "DHP")
heatmap.df <- rbind(heatmap.office, heatmap.division) %>% select(-FREQ)
heatmap.df$BOX <- as.numeric(str_remove(heatmap.df$BOX, "BOX"))

heatmap.df <- heatmap.df %>% filter(NUMBER > 0) %>%
  filter(BOX != 1) %>% filter(BOX != 2) %>% filter(BOX != 13) # remove zero counts and non-terminal boxes
# reorder levels for plotting
heatmap.df$Group.1 <- factor(heatmap.df$Group.1, levels = c("DOP", "DHP", "ODE3", "ODE2", "ODE1", "OAP"))

# plot "heat map" diagram
h <- ggplot(heatmap.df, aes(x = BOX, y = Group.1))
h1 <- (h + geom_point(aes(size = NUMBER, color = PERCENT))
       + geom_vline(aes(xintercept=13),color="black", size=1)
       + geom_vline(aes(xintercept=17.5),color="black", size=1)
       + geom_vline(aes(xintercept=6.5),color="black", size=1, linetype = "dashed")
       + theme_bw()
       + scale_size_continuous(breaks = c(1,3,5,7,9), range = c(3, 13))
       + scale_color_gradient(low = "#ffc547", high = "#0072b2")
       + scale_x_continuous(breaks = c(3,4,5,6,7,8,9,10,11,12,14,15,16,17,18), minor_breaks = NULL)
       + labs(title = "Terminal Marked Boxes by Office")
       + xlab("Box Number")
       + ylab("Office or Division")
       + theme(axis.line = element_line(colour = "black"), panel.border = element_blank(),
               axis.text = element_text(size=14), axis.title = element_text(size=16), 
               plot.title = element_text(size = 24), legend.text = element_text(size = 12),
               legend.title = element_text(size = 14))
       #+coord_flip() # For vertical plot
       #+ scale_x_reverse(breaks = c(3,4,5,6,7,8,9,10,11,12,14,15,16,17,18), minor_breaks = NULL)
       
)
h1

