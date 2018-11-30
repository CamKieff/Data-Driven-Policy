require(ggplot2)
require(plyr)
require(dplyr)

setwd("~/GitHub/Data-Driven-Policy/T2DM_CVOT")

source("CTfunctions.R")

# list of filenames downloaded from ClinicalTrials.gov
f <- c("2018-11-09-T2DM_clinical_trials_interventional_edit.csv", 
       "2018-11-12-hypertension_clinical_trials_interventional.csv",
       "2018-11-12-bcancer_clinical_trials_interventional.csv",
       "2018-11-12-obesity_clinical_trials_interventional.csv")

#run the formatting function on each dataset
f_diabetes <- CTformat(f[1]) %>% mutate(DISEASE = "DIABETES")
f_HBP <- CTformat(f[2]) %>% mutate(DISEASE = "HYPERTENSION")
f_bcancer <- CTformat(f[3]) %>% mutate(DISEASE = "BREAST_CANCER")
f_obesity <- CTformat(f[4]) %>% mutate(DISEASE = "OBESITY")

# combine datasets and rename diseases (for graphing)
f_all <- rbind(f_HBP, f_bcancer, f_obesity, f_diabetes)
f_all$DISEASE <- revalue(f_all$DISEASE, c("BREAST_CANCER" = "Breast Cancer", "DIABETES" = "Diabetes", "HYPERTENSION" = "Hypertension", "OBESITY" = "Obesity"))

f_all %>% filter(INDUSTRY == TRUE) %>% group_by(START_DATE, DISEASE) %>% tally() -> f_diseases_ind # select industry
f_all %>% filter(INDUSTRY == FALSE) %>% group_by(START_DATE, DISEASE) %>% tally() -> f_diseases_nind # select non-industry

f_diseases_ind$START_DATE <- as.Date(paste0(f_diseases_ind$START_DATE, "-01-01"))
f_diseases_nind$START_DATE <- as.Date(paste0(f_diseases_nind$START_DATE, "-01-01"))

# bind hand extracted total intervetional clinical trials (phase 1,2,3) for industry 
# and non-industry to f_diseases data.frames
v1 <- c(2238, 2812, 3492, 3821, 4107, 4169, 4019, 4103, 3821, 3720, 4003, 3977, 3891, 3845) #industry 1,2,3
v2 <- c(1935, 2163, 2419, 2471, 2780, 2962, 3061, 3121, 3216, 3175, 3157, 3483, 3435, 3156) #non industry 123
v1_dates <- as.Date(paste0(seq(from = 2004, to = 2017), "-01-01"))
f_v1 <- as_tibble(list(START_DATE = v1_dates, DISEASE = rep("All Trials", length(v1_dates)), n = v1))
f_v2 <- as_tibble(list(START_DATE = v1_dates, DISEASE = rep("All Trials", length(v1_dates)), n = v2))

names(f_v1) <- names(f_v2) <- names(f_diseases_ind) # rename columns
f_diseases_ind <- bind_rows(f_diseases_ind, f_v1)
f_diseases_nind <- bind_rows(f_diseases_nind, f_v2)

# normalize data to 2004 baseline and extract all trials and diabetes data for plotting
y1 <- ddply(f_diseases_ind, "DISEASE", function(x){ x["n"] <- x["n"] / x[1, "n"] * 100 })
f_diseases_ind1 <- cbind(rep(seq(as.Date("2004-1-1"), as.Date("2017-1-1"), "years"),
                                   nlevels(factor(y1$DISEASE))), y1) 
names(f_diseases_ind1) <- c("START_DATE", "DISEASE", "n")
f_diseases_ind1 %>% filter(DISEASE == "All Trials" | DISEASE == "Diabetes") -> f_diseases_ind1

g1 <- (ggplot(f_diseases_ind1, aes(x=START_DATE, y = n, color = DISEASE)) 
       + geom_line(size = 1.3)
       + geom_vline(aes(xintercept=as.Date("2008-12-01")), 
                    color = "blue", linetype="dashed", size = 1 )
       + scale_x_date(date_breaks = "1 year", date_labels = "%Y")
       + labs(title = "Industry Clinical Trials", color = "Disease")
       + xlab("Trial Start Year")
       + ylab("Normalized Clinical Trials")
       + ylim(0,250)
       + scale_color_brewer(palette = "Set2")
       + theme_bw()
       + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), 
               axis.line = element_line(colour = "black"), panel.border = element_blank(),
               axis.text = element_text(size=12), axis.title = element_text(size=14), 
               plot.title = element_text(size = 20), legend.text = element_text(size = 12),
               legend.title = element_text(size = 14))
)
g1

y2 <- ddply(f_diseases_nind, "DISEASE", function(x){ x["n"] <- x["n"] / x[1, "n"] * 100 })
f_diseases_nind2 <- cbind(rep(seq(as.Date("2004-1-1"), as.Date("2017-1-1"), "years"),
                              nlevels(factor(y2$DISEASE))), y2)
names(f_diseases_nind2) <- c("START_DATE", "DISEASE", "n")
f_diseases_nind2 %>% filter(DISEASE == "All Trials" | DISEASE == "Diabetes") -> f_diseases_nind2

g2 <- (ggplot(f_diseases_nind2, aes(x=START_DATE, y = n, color = DISEASE)) 
       + geom_line(size = 1.3)
       + geom_vline(aes(xintercept=as.Date("2008-12-01")), 
                    color = "blue", linetype="dashed", size = 1 )
       + scale_x_date(date_breaks = "1 year", date_labels = "%Y")
       + labs(title = "Non-Industry Clinical Trials", color = "Disease")
       + xlab("Trial Start Year")
       + ylab("Normalized Clinical Trials")
       + ylim(0,250)
       + scale_color_brewer(palette = "Set2")
       + theme_bw()
       + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), 
               axis.line = element_line(colour = "black"), panel.border = element_blank(),
               axis.text = element_text(size=12), axis.title = element_text(size=14), 
               plot.title = element_text(size = 20), legend.text = element_text(size = 12),
               legend.title = element_text(size = 14))
)
g2

post_2008_ind <- yoy_stats(f_diseases_ind, time.period = "POST2008")
pre_2008_ind <- yoy_stats(f_diseases_ind, time.period = "PRE2008")
post_2008_nind <- yoy_stats(f_diseases_nind, time.period = "POST2008")
pre_2008_nind <- yoy_stats(f_diseases_nind, time.period = "PRE2008")