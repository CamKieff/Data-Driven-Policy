require(ggplot2)
require(plyr)
require(dplyr)

setwd("~/GitHub/Data-Driven-Policy/T2DM_CVOT")

source("CTfunctions.R")

# list of filenames downloaded from ClinicalTrials.gov
f <- c("Data/2018-11-09-T2DM_clinical_trials_interventional_edit.csv", 
       "Data/2018-11-12-hypertension_clinical_trials_interventional.csv",
       "Data/2018-11-12-bcancer_clinical_trials_interventional.csv",
       "Data/2018-11-12-obesity_clinical_trials_interventional.csv")

#run the formatting function on each dataset
f_diabetes <- CTformat(f[1]) %>% mutate(DISEASE = "DIABETES")
f_HBP <- CTformat(f[2]) %>% mutate(DISEASE = "HYPERTENSION")
f_bcancer <- CTformat(f[3]) %>% mutate(DISEASE = "BREAST_CANCER")
f_obesity <- CTformat(f[4]) %>% mutate(DISEASE = "OBESITY")

# combine datasets and rename diseases (for graphing)
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
       + ylim(0,152)
      #+ scale_color_brewer(palette = "Set2")
       + theme_bw()
       + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), 
              axis.line = element_line(colour = "black"), panel.border = element_blank(),
              axis.text = element_text(size=12), axis.title = element_text(size=14), 
              plot.title = element_text(size = 18), legend.text = element_text(size = 12),
              legend.title = element_text(size = 14))
)
g2

#Graph 3, plot multiple diseases to see their trends over time
f_all %>% filter(I_BP_IND == "Large Industry") %>% group_by(START_DATE, DISEASE) %>% tally() -> f_diseases
g3 <- (ggplot(f_diseases, aes(x=START_DATE, y = n, color = DISEASE)) 
       + geom_line(size = 0.8)
       #+ geom_line(data = subset(f_diseases, DISEASE == "Diabetes"),size = 1.6)
       + geom_smooth(method = lm, se = FALSE, data = subset(f_diseases, START_DATE >= as.Date("2009-01-01")), size = 1.4)
       + geom_smooth(method = lm, se = FALSE, data = subset(f_diseases, START_DATE < as.Date("2009-01-01")), size = 1.4)
       #+ geom_label(data = subset(f_diseases, DISEASE == "Diabetes"), aes(label = n), size = 4, hjust = 0.5, vjust=0.5, )
       + geom_vline(aes(xintercept=as.Date("2008-12-01")), 
                    color = "blue", linetype="dashed", size = 1 )
       + scale_x_date(date_breaks = "1 year", date_labels = "%Y")
       + labs(title = " Clinical Trial Number by Disease", color = "Disease")
       + xlab("Trial Start Year")
       + ylab("Number of Clinical Trials")
       + ylim(0,152)
       + scale_color_brewer(palette = "Set2")
       + theme_bw()
       + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), 
             axis.line = element_line(colour = "black"), panel.border = element_blank(),
             axis.text = element_text(size=12), axis.title = element_text(size=14), 
             plot.title = element_text(size = 20), legend.text = element_text(size = 12),
             legend.title = element_text(size = 14))
)
g3

#Graph 4: just a labeled diabetes trend
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
f_regression <- f_all
f_regression$START_DATE <- as.numeric(format(f_regression$START_DATE,"%Y"))

# linear regression trends by phase
# ***double check the filter settings on each regression depending on what you want***
phasesMod_1 <- f_regression %>% 
  filter(DISEASE == "Diabetes" & START_DATE >= 2009 & PHASES == "Phase 1" & I_BP_IND == "Large Industry") %>%
  group_by(START_DATE) %>% 
  tally() %>% 
  lm(n ~ START_DATE, data = .)
summary(phasesMod_1)
confint(phasesMod_1, level = 0.95)

phasesMod_2 <- f_regression %>% 
  filter(DISEASE == "Diabetes" & START_DATE >= 2009 & PHASES == "Phase 2" & I_BP_IND == "Large Industry") %>%
  group_by(START_DATE) %>% 
  tally() %>% 
  lm(n ~ START_DATE, data = .)
summary(phasesMod_2)
confint(phasesMod_2, level = 0.95)

phasesMod_3 <- f_regression %>% 
  filter(DISEASE == "Diabetes" & START_DATE >= 2009 & PHASES == "Phase 3" & I_BP_IND == "Large Industry") %>%
  group_by(START_DATE) %>% 
  tally() %>% 
  lm(n ~ START_DATE, data = .)
summary(phasesMod_3)
confint(phasesMod_3, level = 0.95)

# regressions by disease

diabetesMod <- f_regression %>% 
  filter(DISEASE == "Diabetes" & START_DATE < 2009 & I_BP_IND == "Large Industry") %>%
  group_by(START_DATE) %>% 
  tally() %>% 
  lm(n ~ START_DATE, data = .)
summary(diabetesMod)
confint(diabetesMod, level = 0.95)

bcancerMod <- f_regression %>% 
  filter(DISEASE == "Breast Cancer" & START_DATE < 2009 & I_BP_IND == "Large Industry") %>%
  group_by(START_DATE) %>% 
  tally() %>% 
  lm(n ~ START_DATE, data = .)
summary(bcancerMod)
confint(bcancerMod, level = 0.95)

obesityMod <- f_regression %>% 
  filter(DISEASE == "Obesity" & START_DATE < 2009 & I_BP_IND == "Large Industry") %>%
  group_by(START_DATE) %>% 
  tally() %>% 
  lm(n ~ START_DATE, data = .)
summary(obesityMod)
confint(obesityMod, level = 0.95)

hypertensionMod <- f_regression %>% 
  filter(DISEASE == "Hypertension" & START_DATE < 2009 & I_BP_IND == "Large Industry") %>%
  group_by(START_DATE) %>% 
  tally() %>% 
  lm(n ~ START_DATE, data = .)
summary(hypertensionMod)
confint(hypertensionMod, level = 0.95)

bigpharmaMod <- f_regression %>% 
  filter(DISEASE == "Diabetes" & START_DATE >= 2009 & I_BP_IND == "Large Industry") %>%
  group_by(START_DATE) %>% 
  tally() %>% 
  lm(n ~ START_DATE, data = .)
summary(bigpharmaMod)
confint(bigpharmaMod, level = 0.95)

lilpharmaMod <- f_regression %>% 
  filter(DISEASE == "Diabetes" & START_DATE >= 2009 & I_BP_IND == "Small Industry") %>%
  group_by(START_DATE) %>% 
  tally() %>% 
  lm(n ~ START_DATE, data = .)
summary(lilpharmaMod)
confint(lilpharmaMod, level = 0.95)

nonpharmaMod <- f_regression %>% 
  filter(DISEASE == "Diabetes" & START_DATE >= 2009 & I_BP_IND == "Non-Industry") %>%
  group_by(START_DATE) %>% 
  tally() %>% 
  lm(n ~ START_DATE, data = .)
summary(nonpharmaMod)
confint(nonpharmaMod, level = 0.95)
