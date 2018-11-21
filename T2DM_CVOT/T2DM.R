require(ggplot2)
require(lubridate)
require(reshape2)
require(plyr)
require(dplyr)
require(ggrepel)
require(gridExtra)

setwd("~/GitHub/Data-Driven-Policy/T2DM_CVOT")

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

# function to format ClinicalTrials data. Takes filename as an input.
CTformat <- function(file_name){
  workingdf <- read.csv(file_name, header = TRUE) %>%
    select(1:26) %>%
    mutate(BIG_PHARMA = grepl(big_pattern, .$SPONSORS, ignore.case = TRUE)) %>% # create a "big pharma" variable
    mutate(INDUSTRY = grepl("Industry", .$FUNDED_BY, ignore.case = TRUE)) %>% # create a variable for industry sponsored trials
    mutate(I_BP_IND = INDUSTRY + BIG_PHARMA) %>% #interaction b/w Big Pharma and Industry: industry/small pharma = 1, industry/big pharma = 2
    filter(grepl("1|2|3", .$PHASES)) %>%        # select only trials that are phase 1, 2, or 3                     
    filter(STATUS == "Completed" | STATUS == "Recruiting" | STATUS == "Active, not recruiting") # only select trials with given statuses
  
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
  workingdf$I_BP_IND <- revalue(as.character(workingdf$I_BP_IND), c("0" = "Non-Industry", "1" = "Small Pharma", "2" = "Large Pharma"))
  
  workingdf$START_DATE <- as.Date(workingdf$START_DATE, optional = FALSE)

  workingdf$START_DATE <- floor_date(workingdf$START_DATE, "year") #sets dates at year only for easier plotting

  return(workingdf)
}

# list of filenames downloaded from ClinicalTrials.gov
f <- c("2018-11-09-T2DM_clinical_trials_interventional_edit.csv", 
       "2018-11-12-hypertension_clinical_trials_interventional.csv",
       "2018-11-12-bcancer_clinical_trials_interventional.csv",
       "2018-11-12-obesity_clinical_trials_interventional.csv")

f_diabetes <- CTformat(f[1]) %>% mutate(DISEASE = "DIABETES")
f_HBP <- CTformat(f[2]) %>% mutate(DISEASE = "HYPERTENSION")
f_bcancer <- CTformat(f[3]) %>% mutate(DISEASE = "BREAST_CANCER")
f_obesity <- CTformat(f[4]) %>% mutate(DISEASE = "OBESITY")

f_all <- rbind(f_HBP, f_bcancer, f_obesity, f_diabetes)
f_all$DISEASE <- revalue(f_all$DISEASE, c("BREAST_CANCER" = "Breast Cancer", "DIABETES" = "Diabetes", "HYPERTENSION" = "Hypertension", "OBESITY" = "Obesity"))

# Graph 1, plot year based on Phase of clinical trial
f_all %>% filter(DISEASE == "Diabetes") %>% group_by(START_DATE, PHASES) %>% tally() -> phases_df

g1 <- (ggplot(phases_df, aes(x=START_DATE, y = n, color = PHASES, label = n)) 
       + geom_line(size = 1.5)
       + geom_vline(aes(xintercept=as.Date("2008-12-01")), 
                    color = "blue", linetype="dashed", size = 1 )
       + scale_color_brewer(palette = "Set2")
       + scale_x_date(date_breaks = "1 year", date_labels = "%Y")
       + labs(title = "Number of T2DM Clinical Trials by Trial Phase", color = "Phase")
       + xlab("Trial Start Year")
       + ylab("Number of Clinical Trials")
       + theme_bw()
       + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), 
               axis.line = element_line(colour = "black"), panel.border = element_blank(),
               axis.text = element_text(size=12), axis.title = element_text(size=14), 
               plot.title = element_text(size = 18), legend.text = element_text(size = 12),
               legend.title = element_text(size = 14))
)
g1

# Graph 2, plot year by type of sponsor
f_all %>% filter(DISEASE == "Diabetes") %>% group_by(START_DATE, I_BP_IND) %>% tally() -> industry_df

g2 <- (ggplot(industry_df, aes(x=START_DATE, y = n, color = I_BP_IND)) 
       + geom_line(size = 1.5)
       + geom_vline(aes(xintercept=as.Date("2008-12-01")), 
                    color = "blue", linetype="dashed", size = 1 )
       + scale_x_date(date_breaks = "1 year", date_labels = "%Y")
       + labs(title = "Number of T2DM Clinical Trials by Sponsor Type", color = "Sponsor")
       + xlab("Trial Start Year")
       + ylab("Number of Clinical Trials")
       + ylim(0,150)
      #+ scale_color_brewer(palette = "Set2")
       + theme_bw()
       + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), 
              axis.line = element_line(colour = "black"), panel.border = element_blank(),
              axis.text = element_text(size=12), axis.title = element_text(size=14), 
              plot.title = element_text(size = 18), legend.text = element_text(size = 12),
              legend.title = element_text(size = 14))
)
g2
# Facet-wrapped  graph of the above.
  # f_all %>% group_by(START_DATE, DISEASE, I_BP_IND) %>% tally() -> industry_df
  # 
  # g2 <- (ggplot(industry_df, aes(x=START_DATE, y = n, color = I_BP_IND)) 
  #        + facet_wrap(~DISEASE)
  #        + geom_line(size = 1.5)
  #        + geom_vline(aes(xintercept=as.Date("2008-07-01")), 
  #                     color = "blue", linetype="dashed", size = 1 )
  #        + scale_x_date(date_breaks = "2 years", date_labels = "%Y")
  #        + labs(title = "Clinical Trials by Start Date and Funding Source", color = "Sponsor")
  #        + xlab("Trial Start Year")
  #        + ylab("Number of Clinical Trials")
  #        + ylim(0,150)
  #        #+ scale_color_brewer(palette = "Set2")
  #        + theme_bw()
  #        + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), 
  #                axis.line = element_line(colour = "black"), panel.border = element_blank(),
  #                axis.text = element_text(size=12), axis.title = element_text(size=14), 
  #                plot.title = element_text(size = 18), legend.text = element_text(size = 12),
  #                legend.title = element_text(size = 14), strip.background = element_rect(fill="white"),
  #                strip.text = element_text(size=12))
  # )
  # g2
#Graph 3, plot multiple diseases to see their trends over time
f_all %>% group_by(START_DATE, DISEASE) %>% tally() -> f_diseases
g3 <- (ggplot(f_diseases, aes(x=START_DATE, y = n, color = DISEASE)) 
       + geom_line(size = 1.25)
       + geom_line(data = subset(f_diseases, DISEASE == "Diabetes"),size = 1.6)
       #+ geom_label(data = subset(f_diseases, DISEASE == "Diabetes"), aes(label = n), size = 4, hjust = 0.5, vjust=0.5, )
       + geom_vline(aes(xintercept=as.Date("2008-12-01")), 
                    color = "blue", linetype="dashed", size = 1 )
       + scale_x_date(date_breaks = "1 year", date_labels = "%Y")
       + labs(title = " Clinical Trial Number by Disease", color = "Disease")
       + xlab("Trial Start Year")
       + ylab("Number of Clinical Trials")
       + ylim(0,300)
       + scale_color_brewer(palette = "Set2")
       + theme_bw()
       + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), 
             axis.line = element_line(colour = "black"), panel.border = element_blank(),
             axis.text = element_text(size=12), axis.title = element_text(size=14), 
             plot.title = element_text(size = 20), legend.text = element_text(size = 12),
             legend.title = element_text(size = 14))
)
g3

#Graph 4: just the labeled diabetes trend
f_all %>% filter(DISEASE == "Diabetes") %>% group_by(START_DATE) %>% tally() -> f_diseases
g4 <- (ggplot(f_diseases, aes(x=START_DATE, y = n)) 
       + geom_line(size = 1.5)
       + geom_label(aes(label = n), size = 4, hjust = 0.5, vjust=0.5)
       + geom_vline(aes(xintercept=as.Date("2008-12-01")), 
                    color = "blue", linetype="dashed", size = 1 )
       + scale_x_date(date_breaks = "1 year", date_labels = "%Y")
       + labs(title = "All Interventional Diabetes Clinical Trials")
       + xlab("Trial Start Year")
       + ylab("Number of Clinical Trials")
       + ylim(0,250)
       #+ scale_color_brewer(palette = "Set2")
       + theme_bw()
       + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), 
               axis.line = element_line(colour = "black"), panel.border = element_blank(),
               axis.text = element_text(size=12), axis.title = element_text(size=14), 
               plot.title = element_text(size = 20), legend.text = element_text(size = 12),
               legend.title = element_text(size = 14))
)
g4

# Below is the linear regression analyses.
# convert start dates to numeric year values for linear regressions
f_all$START_DATE <- as.numeric(format(f_all$START_DATE,"%Y"))

# year-over-year changes by phase
phasesMod <- f_all %>% 
  filter(DISEASE == "Diabetes" & START_DATE >= 2009) %>%
  group_by(START_DATE) %>% 
  tally() %>% 
  lm(n ~ START_DATE, data = .)
confint(phasesMod, level = 0.95)

phasesMod_1 <- f_all %>% 
  filter(DISEASE == "Diabetes" & START_DATE >= 2009 & PHASES == "Phase 1") %>%
  group_by(START_DATE) %>% 
  tally() %>% 
  lm(n ~ START_DATE, data = .)
confint(phasesMod_1, level = 0.95)

phasesMod_2 <- f_all %>% 
  filter(DISEASE == "Diabetes" & START_DATE >= 2009 & PHASES == "Phase 2") %>%
  group_by(START_DATE) %>% 
  tally() %>% 
  lm(n ~ START_DATE, data = .)
confint(phasesMod_2, level = 0.95)

phasesMod_3 <- f_all %>% 
  filter(DISEASE == "Diabetes" & START_DATE >= 2009 & PHASES == "Phase 3") %>%
  group_by(START_DATE) %>% 
  tally() %>% 
  lm(n ~ START_DATE, data = .)
confint(phasesMod_3, level = 0.95)

# regressions by disease

bcancerMod <- f_all %>% 
  filter(DISEASE == "Breast Cancer" & START_DATE >= 2009) %>%
  group_by(START_DATE) %>% 
  tally() %>% 
  lm(n ~ START_DATE, data = .)
confint(bcancerMod, level = 0.95)

obesityMod <- f_all %>% 
  filter(DISEASE == "Obesity" & START_DATE >= 2009) %>%
  group_by(START_DATE) %>% 
  tally() %>% 
  lm(n ~ START_DATE, data = .)
confint(obesityMod, level = 0.95)

hyertensionMod <- f_all %>% 
  filter(DISEASE == "Hypertension" & START_DATE >= 2009) %>%
  group_by(START_DATE) %>% 
  tally() %>% 
  lm(n ~ START_DATE, data = .)
confint(hyertensionMod, level = 0.95)
