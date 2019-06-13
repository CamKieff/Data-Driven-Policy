library(ggplot2)
library(plyr)
library(dplyr)
library(zoo)
library(gridExtra)

setwd("~/GitHub/Data-Driven-Policy/T2DM_CVOT")

source("CTfunctions.R")

# list of filenames downloaded from ClinicalTrials.gov
f <- c("Data/OtherTherapeuticAreas/2019-05-20-alzheimers.csv", 
       "Data/OtherTherapeuticAreas/2019-05-20-anxiety.csv", 
       "Data/OtherTherapeuticAreas/2019-05-20-asthma.csv", 
       "Data/OtherTherapeuticAreas/2019-05-20-dyslipidemia.csv", 
       "Data/OtherTherapeuticAreas/2019-05-20-HIV.csv",
       "Data/OtherTherapeuticAreas/2019-05-20-NASH.csv", 
       "Data/OtherTherapeuticAreas/2019-05-20-non-small-cell-lung-cancer.csv",
       "Data/OtherTherapeuticAreas/2019-05-20-type1-diabetes.csv",
       "Data/2019-01-14-T2DM_clinical_trials_interventional_2000-2017.csv", 
       "Data/2019-03-28-hypertension_clinical_trials_interventional_2000-2017.csv",
       "Data/2019-01-22-bcancer_clinical_trials_interventional_2000-2017.csv",
       "Data/2019-03-28-obesity_clinical_trials_interventional_2000-2017.csv",
       "Data/2019-01-22-depression_clinical_trials_interventional_2000-2017.csv",
       "Data/OtherTherapeuticAreas/2019-05-21-nephropathy.csv")

# run the import/formatting function on each dataset
f_alz <- CTformat(f[1]) %>% mutate(DISEASE = "Alzheimers")
f_anx <- CTformat(f[2]) %>% mutate(DISEASE = "Anxiety")
f_ast <- CTformat(f[3]) %>% mutate(DISEASE = "Asthma")
f_dys <- CTformat(f[4]) %>% mutate(DISEASE = "Dyslipidemia")
f_hiv <- CTformat(f[5]) %>% mutate(DISEASE = "HIV")
f_nash <- CTformat(f[6]) %>% mutate(DISEASE = "NASH")
f_nsclc <- CTformat(f[7]) %>% mutate(DISEASE = "NSCLCancer")
f_type1 <- CTformat(f[8]) %>% mutate(DISEASE = "Diabetes1")
f_type2 <- CTformat(f[9]) %>% mutate(DISEASE = "Diabetes2")
f_hyp <- CTformat(f[10]) %>% mutate(DISEASE = "Hypertension")
f_bre <- CTformat(f[11]) %>% mutate(DISEASE = "BreastCancer")
f_obe <- CTformat(f[12]) %>% mutate(DISEASE = "Obesity")
f_dep <- CTformat(f[13]) %>% mutate(DISEASE = "Depression")
f_nep <- CTformat(f[14]) %>% mutate(DISEASE = "Nephropathy")


# combine datasets and rename diseases (for graphing)
f_all <- rbind(f_alz, f_anx, f_ast, f_dys, f_hiv, f_nash, f_nsclc, f_type1, f_type2, f_hyp, f_bre, f_obe, f_dep, f_nep)

f_all %>% filter(INDUSTRY == TRUE) %>% group_by(START_DATE, DISEASE) %>% tally() -> f_diseases_ind # select industry
f_all %>% filter(INDUSTRY == FALSE) %>% group_by(START_DATE, DISEASE) %>% tally() -> f_diseases_nind # select non-industry

# f_diseases_ind$START_DATE <- as.Date(paste0(f_diseases_ind$START_DATE, "-01-01"))
# f_diseases_nind$START_DATE <- as.Date(paste0(f_diseases_nind$START_DATE, "-01-01"))

# bind hand extracted total intervetional clinical trials (phase 1,2,3) for industry 
# and non-industry to f_diseases data.frames
v1 <- c(474, 747, 1262, 1647, 2239, 2812, 3492, 3821, 4107, 4169, 4019, 4103, 3821, 3720, 4003, 3977, 3891, 3845) #industry 1,2,3
v2 <- c(953, 1042, 1171, 1482, 1935, 2163, 2419, 2471, 2780, 2962, 3061, 3121, 3216, 3175, 3157, 3483, 3435, 3156) #non industry 123
v1_dates <- as.Date(paste0(seq(from = 2000, to = 2017), "-01-01"))
f_v1 <- as_tibble(list(START_DATE = v1_dates, DISEASE = rep("All Trials", length(v1_dates)), n = v1))
f_v2 <- as_tibble(list(START_DATE = v1_dates, DISEASE = rep("All Trials", length(v1_dates)), n = v2))

names(f_v1) <- names(f_v2) <- names(f_diseases_ind) # rename columns
f_diseases_ind <- bind_rows(f_diseases_ind, f_v1)
f_diseases_nind <- bind_rows(f_diseases_nind, f_v2)

#industry trials plotting
f_diseases_ind %>% filter(DISEASE == "Diabetes2") -> f_diseases_ind1
f_diseases_ind %>% filter(DISEASE == "All Trials") -> f_diseases_ind2

g1 <- (ggplot(f_diseases_ind1, aes(x=START_DATE, y = n)) 
       + geom_line(size = 1.3, color ="#f79017")
       + geom_vline(aes(xintercept=as.Date("2008-12-01")), 
                    color = "blue", linetype="dashed", size = 1 )
       + scale_x_date(date_breaks = "1 year", date_labels = "'%y")
       + labs(title = "Industry-Sponsored T2DM Clinical Trials")
       + xlab("Trial Start Year")
       + ylab("Number of Clinical Trials")
       + ylim(0,max(f_diseases_ind1$n))
       + theme_bw()
       + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), 
               axis.line = element_line(colour = "black"), panel.border = element_blank(),
               axis.text = element_text(size=14), axis.title = element_text(size=16), 
               plot.title = element_text(size = 24), legend.text = element_text(size = 12),
               legend.title = element_text(size = 14))
)
g2 <- (ggplot(f_diseases_ind2, aes(x=START_DATE, y = n)) 
       + geom_line(size = 1.3)
       + geom_vline(aes(xintercept=as.Date("2008-12-01")), 
                    color = "blue", linetype="dashed", size = 1 )
       + scale_x_date(date_breaks = "1 year", date_labels = "'%y")
       + labs(title = "All Industry-Sponsored Clinical Trials")
       + xlab("Trial Start Year")
       + ylab("Number of Clinical Trials")
       + ylim(0,max(f_diseases_ind2$n))
       + theme_bw()
       + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), 
               axis.line = element_line(colour = "black"), panel.border = element_blank(),
               axis.text = element_text(size=14), axis.title = element_text(size=16), 
               plot.title = element_text(size = 24), legend.text = element_text(size = 12),
               legend.title = element_text(size = 14))
)

# non-industry trials plotting
f_diseases_nind %>% filter(DISEASE == "Diabetes") -> f_diseases_nind3
f_diseases_nind %>% filter(DISEASE == "All Trials") -> f_diseases_nind4

g3 <- (ggplot(f_diseases_nind3, aes(x=START_DATE, y = n)) 
       + geom_line(size = 1.3, color = "#09a57e")
       + geom_vline(aes(xintercept=as.Date("2008-12-01")), 
                    color = "blue", linetype="dashed", size = 1 )
       + scale_x_date(date_breaks = "1 year", date_labels = "'%y")
       + labs(title = "Non-Industry T2DM Clinical Trials")
       + xlab("Trial Start Year")
       + ylab("Number of Clinical Trials")
       + ylim(0,max(f_diseases_nind3$n))
       + theme_bw()
       + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), 
               axis.line = element_line(colour = "black"), panel.border = element_blank(),
               axis.text = element_text(size=14), axis.title = element_text(size=16), 
               plot.title = element_text(size = 24), legend.text = element_text(size = 12),
               legend.title = element_text(size = 14))
)

g4 <- (ggplot(f_diseases_nind4, aes(x=START_DATE, y = n)) 
       + geom_line(size = 1.3)
       + geom_vline(aes(xintercept=as.Date("2008-12-01")), 
                    color = "blue", linetype="dashed", size = 1 )
       + scale_x_date(date_breaks = "1 year", date_labels = "'%y")
       + labs(title = "All Non-Industry Clinical Trials")
       + xlab("Trial Start Year")
       + ylab("Number of Clinical Trials")
       + ylim(0,max(f_diseases_nind4$n))
       + theme_bw()
       + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), 
               axis.line = element_line(colour = "black"), panel.border = element_blank(),
               axis.text = element_text(size=14), axis.title = element_text(size=16), 
               plot.title = element_text(size = 24), legend.text = element_text(size = 12),
               legend.title = element_text(size = 14))
)

grid.arrange(g1,g2,g3,g4, ncol=2) # plot all 4 graphs on a single graph. Probably could have done this with a facet wrap if the data was structured a bit differently.

post_2008_ind <- yoy_stats(f_diseases_ind, time.period = "POST2008")
pre_2008_ind <- yoy_stats(f_diseases_ind, time.period = "PRE2008")
post_2008_nind <- yoy_stats(f_diseases_nind, time.period = "POST2008")
pre_2008_nind <- yoy_stats(f_diseases_nind, time.period = "PRE2008")

# Google Patent analysis

patents <- read.csv("data/2019-01-24_T2DM_google_patent_count.csv", header = TRUE)
patents$YEAR <- as.Date(paste0(patents$YEAR, "-01-01"))

g5 <- (ggplot(patents, aes(x=YEAR, y = Type2Diabetes)) 
       + geom_point(size = 3, color = "#FF0000AA")
       + geom_vline(aes(xintercept=as.Date("2008-12-01")), 
                    color = "blue", linetype="dashed", size = 1 )
       + geom_line(aes(y=rollmean(Type2Diabetes, 3, na.pad = TRUE)), size = 1.3, linetype="dashed")
       + scale_x_date(date_breaks = "1 year", date_labels = "'%y", limits = c(as.Date("1995-01-01"), as.Date("2016-01-01")))
       + labs(title = "Patents for Anti-Diabetic Agents")
       + xlab("Year Filed")
       + ylab("Number of Patents")
       + ylim(0,max(patents$Type2Diabetes))
       + theme_bw()
       + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), 
               axis.line = element_line(colour = "black"), panel.border = element_blank(),
               axis.text = element_text(size=14), axis.title = element_text(size=16), 
               plot.title = element_text(size = 24), legend.text = element_text(size = 12),
               legend.title = element_text(size = 14))
)
g5

# plot of clinical trial data by phase
f_all %>% filter(INDUSTRY == TRUE) %>% group_by(START_DATE, DISEASE, PHASES) %>% tally() -> f_diseases_phases
f_diseases_phases %>% filter(DISEASE == "Diabetes") -> f_diseases_phases1
g6 <- (ggplot(f_diseases_phases1, aes(x=START_DATE, y = n, color = PHASES)) 
       + geom_line(size = 1.3)
       + geom_vline(aes(xintercept=as.Date("2008-12-01")), 
                    color = "blue", linetype="dashed", size = 1 )
       + scale_x_date(date_breaks = "1 year", date_labels = "'%y")
       + labs(title = "Industry-Sponsored T2DM Clinical Trials by Phase")
       + xlab("Trial Start Year")
       + ylab("Number of Clinical Trials")
       + ylim(0,max(f_diseases_phases1$n))
       + theme_bw()
       + scale_colour_brewer(palette = "Set2")
       + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), 
               axis.line = element_line(colour = "black"), panel.border = element_blank(),
               axis.text = element_text(size=14), axis.title = element_text(size=16), 
               plot.title = element_text(size = 24), legend.text = element_text(size = 12),
               legend.title = element_text(size = 14))
)
g6

# Plot by therapeutic area, including non diabetes drugs
f_diseases_ind %>% filter(DISEASE != "All Trials") -> f_diseases_TA
f_diseases_TA %>% filter(DISEASE == "Diabetes2" | DISEASE == "Hypertension" | DISEASE == "BreastCancer" | DISEASE == "Obesity" | DISEASE == "Depression" | DISEASE == "Nephropathy") -> f_diseases_TA
g7 <- (ggplot(f_diseases_TA, aes(x=START_DATE, y = n, color = DISEASE)) 
       + geom_line(size = 1.3)
       #+ facet_wrap(~ DISEASE, ncol = 4)
       + geom_line(data = f_diseases_TA[f_diseases_TA$DISEASE == "Diabetes2",], size = 2) # bold diabetes line
       + geom_vline(aes(xintercept=as.Date("2008-12-01")), 
                    color = "blue", linetype="dashed", size = 1 )
       + scale_x_date(date_breaks = "1 year", date_labels = "'%y")
       + labs(title = "Industry-Sponsored Clinical Trials by Therapeutic Area")
       + xlab("Trial Start Year")
       + ylab("Number of Clinical Trials")
       + ylim(0,max(f_diseases_TA$n))
       + theme_bw()
       + scale_colour_brewer(palette = "Set2")
       + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), 
               axis.line = element_line(colour = "black"), panel.border = element_blank(),
               axis.text = element_text(size=14), axis.title = element_text(size=16), 
               plot.title = element_text(size = 24), legend.text = element_text(size = 12),
               legend.title = element_text(size = 14))
)
g7

f_diseases_phases %>% 
        filter(PHASES == "Phase 1") %>%
        yoy_stats(time.period = "PRE2008")

f_diseases_phases %>% 
        filter(PHASES == "Phase 2") %>%
        yoy_stats(time.period = "PRE2008")

f_diseases_phases %>% 
        filter(PHASES == "Phase 3") %>%
        yoy_stats(time.period = "PRE2008")

as_tibble(f_all) %>%
        filter(INDUSTRY == TRUE & DISEASE == "Diabetes") -> test

# Find enrollment statistics
test$ENROLLMENT <- as.numeric(levels(test$ENROLLMENT))[test$ENROLLMENT]
mean(test$ENROLLMENT, na.rm = TRUE)
sd(test$ENROLLMENT, na.rm = TRUE)/sqrt(sum(!is.na(test$ENROLLMENT)))

test %>%
        select(PHASES, ENROLLMENT) %>% 
        aggregate(. ~ PHASES, ., FUN = function(x){sd(x)/sqrt(length(x))})
