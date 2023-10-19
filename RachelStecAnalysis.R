# libraries needed
library(tidyverse)
library(ggplot2)
library(rstatix)

# load in data sets
Cauti_Incidences <- read_csv("~/Downloads/Cauti_Incidences.csv")
Post_Intervention_Nursing_Survey <- read_csv("~/Downloads/Post Intervention Nursing Survey.csv")
StecData <- read_csv("~/Downloads/StecData.csv")

# paired t-test with pre and post survey
 paired_test <- t.test(StecData$Pre, StecData$Post, paired = TRUE)
 paired_test
 
# binomial test and ratios
 total_cauti <- sum(Cauti_Incidences$`# of CAUTI`[-7])
 total_resident_days <- sum(Cauti_Incidences$`Resident Days`[-7]) 
 pre_cauti <- sum(Cauti_Incidences$`# of CAUTI`[1:6])
 pre_resident_days <- sum(Cauti_Incidences$`Resident Days`[1:6])
 post_cauti <- sum(Cauti_Incidences$`# of CAUTI`[8:9])
 post_resident_days <- sum(Cauti_Incidences$`Resident Days`[8:9])
 ratio_expected_resident <- post_resident_days/(post_resident_days + pre_resident_days)
 ratio_expected_cauti <- total_cauti*ratio_expected_resident
 
 sum(dbinom(post_cauti:total_cauti, total_cauti, ratio_expected_resident))
 
# plot of Post Nursing Survey Results
 stacked_survey <- cbind(Post_Intervention_Nursing_Survey[1], stack(Post_Intervention_Nursing_Survey[2:8]))
 stacked_survey
 ggplot(stacked_survey, aes(ind, values)) + 
 scale_y_continuous(breaks = 1:7, labels = c("Strongly Disagree", "Disagree", "Somewhat Disagree", "Neutral", "Somewhat Agree", "Agree", "Strongly Agree")) + 
 ylim("Strongly Disagree", "Disagree", "Somewhat Disagree", "Neutral", "Somewhat Agree", "Agree", "Strongly Agree") + ggtitle("Post Survey Results measuring knowledge of Urinary Retention Checklist and the Catheter Indwelling Handoff Tool") + 
 xlab("Question") + 
 ylab("Likert Scale Measure") + 
 geom_boxplot() + 
 coord_flip() + 
 geom_point()
 