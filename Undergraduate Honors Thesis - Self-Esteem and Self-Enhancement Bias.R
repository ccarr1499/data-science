# CT (Task Difficulty and Self-Assessment) cleaning and analysis
# Code by Christina Carr

library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)
library(stringr)
library(psych)


#setwd('C:/Users/Nina/Desktop/Honors')

#Read in data
CT_data <- read.csv(file = "CT_data_all.csv", stringsAsFactors = FALSE)

#delete rows of Qualtrics info
CT_data <- CT_data[-c(1:2),]

#########################################
### REMOVING PRE-REGISTERED EXCLUSIONS ###
# remove this section for analyses on whole sample #
#########################################

#remove data from after the data collection deadline (loses 57 responses)
CT_data$StartDate <- as.Date(CT_data$StartDate,'%Y-%m-%d %H:%M:%S')
CT_data <- CT_data[CT_data$StartDate <= '2019-12-06',]
CT_data$StartDate <- as.factor(CT_data$StartDate)

#remove people who failed the comprehension check (loses 25 responses)
CT_data <- CT_data[(CT_data$Comprehension_check==10),]

#remove people who spent less than 5 minutes total on the survey (loses 20 responses)
CT_data$Duration..in.seconds. <- as.numeric(as.character(CT_data$Duration..in.seconds.))
CT_data <- CT_data[CT_data$Duration..in.seconds. >= 300,]

#remove responses from duplicate IP Addresses (loses 16 responses)
CT_data <- CT_data[!duplicated(CT_data$IPAddress),]

#remove people who did not answer any IQ questions AND took less than 15 minutes overall
CT_data[CT_data==''] <- NA
CT_data <- CT_data[(rowSums(is.na(CT_data[,c(30:39,44:53)]))!=20)||CT_data$Duration..in.seconds.>900,]

#########################################
### SCORING RSES ###
#########################################

rses <- c("RSES_1", "RSES_2", "RSES_3", "RSES_4", "RSES_5", "RSES_6", "RSES_7", "RSES_8", "RSES_9", "RSES_10")

for (val in rses) {
  CT_data[,val] <- as.numeric(as.character(CT_data[,val]))
}

#Reverse score
cols <- c("RSES_3", "RSES_5", "RSES_8", "RSES_9", "RSES_10")
CT_data[,cols] <- 5 - CT_data[ ,cols]

#########################################
### SCORING SSES ###
#########################################

sses <- c("SSES_1", "SSES_2", "SSES_3", "SSES_4", "SSES_5", "SSES_6", "SSES_7", "SSES_8", "SSES_9",
          "SSES_10", "SSES_11", "SSES_12", "SSES_13", "SSES_14", "SSES_15", "SSES_16", "SSES_17",
          "SSES_18", "SSES_19", "SSES_20")

for (val in sses) {
  CT_data[,val] <- as.numeric(as.character(CT_data[,val]))
}

#Reverse score
reverse_sses <- c("SSES_2", "SSES_4", "SSES_5", "SSES_7", "SSES_8", "SSES_10", "SSES_13", "SSES_15",
                  "SSES_16", "SSES_17", "SSES_18", "SSES_19", "SSES_20")
CT_data[,reverse_sses] <- 6 - CT_data[ ,reverse_sses]


rses_sums <- rowSums(CT_data[,rses])

sses_sums <- rowSums(CT_data[,sses])

#########################################
### SCALE RELIABILITY ###
#########################################
psych::alpha(CT_data[,rses])

psych::alpha(CT_data[,sses])


#########################################
### MANIPULATION CHECK ###
#########################################

#add condition column

CT_data$timing_easy_Page.Submit <- as.numeric(as.character(CT_data$timing_easy_Page.Submit))
Condition <- list()
for (i in c(1:length(CT_data$timing_easy_Page.Submit))) {
  if (is.na(CT_data$timing_easy_Page.Submit[i])) {
    Condition[i] <- "Difficult"
  }
  else {
    Condition[i] <- "Easy"
  }
}
  
CT_data$Condition <- Condition
CT_data$RSES_total <- rses_sums
CT_data$SSES_total <- sses_sums

diff <- CT_data[CT_data$Condition == "Difficult",]
easy <- CT_data[CT_data$Condition=="Easy",]

model <- aov(SSES_total ~ RSES_total+unlist(Condition), data = CT_data)
summary(model)

# Barplot of mean SSES scores by condition

describe(diff$SSES_total) #SE 1.67, mean 66.45, sd 11.93; for full sample (without exclusions), SE 1.33, mean 65.73, sd 11.99
describe(easy$SSES_total) #SE 1.63, mean 67.57, sd 13.12; for full sample (without exclusions), SE 1.31, mean 68.13, sd 12.59

means <- c(66.45, 67.57) #means of difficult and easy condition SSES scores (change to appropriate values if using full sample)
se <- c(1.67, 1.63) #standard errors of difficult and easy condition SSES scores (change to appropriate values if using full sample)

# Barplot
manipulation <- barplot(means, main = "Manipulation Check", names.arg=c("Difficult", "Easy"),
                      col=c("red", "blue"), las=1, ylim=c(0,80), xlab = "IQ Test Condition", ylab = "SSES Score")


# Add error bars
arrows(manipulation, means-se*2, manipulation, means+se*2, lwd=2, angle=90, code=3)

#########################################
### SCORING WEIGHT ESTIMATES ###
#########################################

#get relevant columns
weight_ests <- CT_data[,c('X2_photo_est', 'X4_photo_est', 'X7_photo_est', 'X10_photo_est',
                          'self_spd_1', 'self_spd_2', 'self_spd_3', 'self_spd_4', 'self_spd_5',
                          'other_spd_1', 'other_spd_2', 'other_spd_3', 'other_spd_4', 'other_spd_5',
                          'RSES_total', 'SSES_total','Condition')]

#clean how people responded to the weight estimate questions
weight_ests[,18:21] <- apply(weight_ests[,1:4],c(1,2),toString)
weight_ests <- na.omit(weight_ests)

for (i in c(18:21)) {
  for (j in c(1:dim(weight_ests)[1])) {
    val <- weight_ests[j,i]
    if (
      str_detect(val, 'kg', negate=FALSE) |
      str_detect(val, ' kg', negate=FALSE) |
      str_detect(val, 'kgs', negate=FALSE)
    ) {
      weight_ests[j,i] <- gsub("[a-zA-Z ]","",weight_ests[j,i])
      weight_ests[j,i] <- as.numeric(as.character(weight_ests[j,i]))*2.205 
    }
    else {
      weight_ests[j,i] <- gsub("[a-zA-Z ]","",weight_ests[j,i])
    }
  }
}
weight_ests[46,18] <- as.numeric(as.character(weight_ests[46,18]))*2.205

weight_ests$V18 <- as.numeric(as.character(weight_ests$V18))
weight_ests$V19 <- as.numeric(as.character(weight_ests$V19))
weight_ests$V20 <- as.numeric(as.character(weight_ests$V20))
weight_ests$V21 <- as.numeric(as.character(weight_ests$V21))

# score weight estimates
correct_weights <- c(200, 169, 149, 113)

correct <- replicate(dim(weight_ests)[1], 0)

for (i in c(1:4)) {
  # check if the individual's response was within 5 lbs of the correct weight
  new_correct <- lapply(weight_ests[,i+17], function(x) x <= (correct_weights[i]+5) &&
                          x >= (correct_weights[i]-5))
  # add the new list of ones and zeroes to the intial number correct list
  new_correct <- lapply(seq_along(new_correct),
                        function(k) unlist(new_correct[k])+unlist(correct[k]))
  # re-initialize the list
  correct <- new_correct
}

weight_ests$num_correct <- correct
weight_ests$num_correct <- as.numeric(as.character(weight_ests$num_correct))

#########################################
### WEIGHTED SELF SPD SCORES ###
#########################################

weight_ests$self_spd_1 <- as.numeric(as.character(weight_ests$self_spd_1))
weight_ests$self_spd_2 <- as.numeric(as.character(weight_ests$self_spd_2))
weight_ests$self_spd_3 <- as.numeric(as.character(weight_ests$self_spd_3))
weight_ests$self_spd_4 <- as.numeric(as.character(weight_ests$self_spd_4))
weight_ests$self_spd_5 <- as.numeric(as.character(weight_ests$self_spd_5))

weight_ests$self_total <- rowSums(weight_ests[,5:9])

weight_ests$self_total <- as.numeric(as.character(weight_ests$self_total))

weight_ests$self_spd_1 <- weight_ests$self_spd_1/weight_ests$self_total
weight_ests$self_spd_2 <- weight_ests$self_spd_2/weight_ests$self_total
weight_ests$self_spd_3 <- weight_ests$self_spd_3/weight_ests$self_total
weight_ests$self_spd_4 <- weight_ests$self_spd_4/weight_ests$self_total
weight_ests$self_spd_5 <- weight_ests$self_spd_5/weight_ests$self_total

#########################################
### WEIGHTED OTHER SPD SCORES ###
#########################################

weight_ests$other_spd_1 <- as.numeric(as.character(weight_ests$other_spd_1))
weight_ests$other_spd_2 <- as.numeric(as.character(weight_ests$other_spd_2))
weight_ests$other_spd_3 <- as.numeric(as.character(weight_ests$other_spd_3))
weight_ests$other_spd_4 <- as.numeric(as.character(weight_ests$other_spd_4))
weight_ests$other_spd_5 <- as.numeric(as.character(weight_ests$other_spd_5))

weight_ests$other_total <- rowSums(weight_ests[,10:14])

weight_ests$other_total <- as.numeric(as.character(weight_ests$other_total))

weight_ests$other_spd_1 <- weight_ests$other_spd_1/weight_ests$other_total
weight_ests$other_spd_2 <- weight_ests$other_spd_2/weight_ests$other_total
weight_ests$other_spd_3 <- weight_ests$other_spd_3/weight_ests$other_total
weight_ests$other_spd_4 <- weight_ests$other_spd_4/weight_ests$other_total
weight_ests$other_spd_5 <- weight_ests$other_spd_5/weight_ests$other_total


# convert to estimate of score (SELF)
weight_ests$self_spd_1 <- weight_ests$self_spd_1*0
weight_ests$self_spd_2 <- weight_ests$self_spd_2*1
weight_ests$self_spd_3 <- weight_ests$self_spd_3*2
weight_ests$self_spd_4 <- weight_ests$self_spd_4*3
weight_ests$self_spd_5 <- weight_ests$self_spd_5*4

weight_ests$pred_self <- rowSums(weight_ests[,5:9])

# convert to estimate of score (OTHERS)
weight_ests$other_spd_1 <- weight_ests$other_spd_1*0
weight_ests$other_spd_2 <- weight_ests$other_spd_2*1
weight_ests$other_spd_3 <- weight_ests$other_spd_3*2
weight_ests$other_spd_4 <- weight_ests$other_spd_4*3
weight_ests$other_spd_5 <- weight_ests$other_spd_5*4

weight_ests$pred_other <- rowSums(weight_ests[,10:14])

#########################################
### OVERCONFIDENCE ANALYSES ###
#########################################

# OVERESTIMATION
# From pre-reg: "The difference between the participant's weighted estimated score and their actual score."
weight_ests$overestimation <- weight_ests$pred_self - weight_ests$num_correct

# OVERPLACEMENT
# From pre-reg: "The difference between the participant's weighted estimated score for themselves
#and the participant's weighted estimated score for others, minus the difference between their own
#actual score and the actual average score."
weight_ests$overplacement <- (weight_ests$pred_self - weight_ests$pred_other) - (
  weight_ests$num_correct - mean(weight_ests$num_correct, na.rm=TRUE))

# graph overestimation
ggplot(weight_ests, aes(overestimation)) + geom_density() + xlab("Overestimation") + ylab("Density")
mean(weight_ests$overestimation, na.rm=T)
median(weight_ests$overestimation, na.rm=T)

# graph overplacement
ggplot(weight_ests, aes(overplacement)) + geom_density() + xlab("Overplacement") + ylab("Density")
mean(weight_ests$overplacement, na.rm=T)
median(weight_ests$overplacement, na.rm=T)


#########################################
### PEARSON CORRELATION ANALYSES ###
#########################################

#install.packages("ggpubr")
library(ggpubr)

#plots
ggscatter(weight_ests, x = "RSES_total", y = "overestimation", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = FALSE, cor.method = "pearson",
          xlab = "RSES Score", ylab = "Overestimation")


ggscatter(weight_ests, x = "RSES_total", y = "overplacement", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = FALSE, cor.method = "pearson",
          xlab = "RSES Score", ylab = "Overplacement")


#Pearson Correlation tests
r_overplacement <- cor.test(weight_ests$overplacement, weight_ests$RSES_total, method = "pearson")
r_overplacement

r_overestimation <- cor.test(weight_ests$overestimation, weight_ests$RSES_total, method = "pearson")
r_overestimation

#########################################
### ADDITIONAL ANALYSIS ###
#########################################

#scatter plots of state self-esteem and overplacement/overestimation
ggscatter(weight_ests, x = "SSES_total", y = "overplacement", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = FALSE, cor.method = "pearson",
          xlab = "SSES Score", ylab = "Overplacement")

ggscatter(weight_ests, x = "SSES_total", y = "overestimation", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = FALSE, cor.method = "pearson",
          xlab = "SSES Score", ylab = "Overestimation")

#Pearson Correlation tests for state self-esteem scores and overplacement/overestimation
s_overplacement <- cor.test(weight_ests$overplacement, weight_ests$SSES_total, method = "pearson")
s_overplacement

s_overestimation <- cor.test(weight_ests$overestimation, weight_ests$SSES_total, method = "pearson")
s_overestimation

#scatter plots broken down by condition
weight_ests$Condition <- as.factor(unlist(weight_ests$Condition))

ggscatter(weight_ests, x = "SSES_total", y = "overplacement", 
          color = "Condition", palette = c("blue", "black"), add = "reg.line", conf.int = TRUE, 
          cor.coef = FALSE, cor.method = "pearson",
          xlab = "SSES Score", ylab = "Overplacement")

ggscatter(weight_ests, x = "SSES_total", y = "overestimation", 
          color = "Condition", palette = c("blue", "black"), add = "reg.line", conf.int = TRUE, 
          cor.coef = FALSE, cor.method = "pearson",
          xlab = "SSES Score", ylab = "Overestimation")

#Multiple linear regression for overplacement and overestimation by SSES and condition
mlr_overplacement <- lm(overplacement ~ SSES_total*Condition, data=weight_ests)
summary(mlr_overplacement)

mlr_overestimation <- lm(overestimation ~ SSES_total*Condition, data=weight_ests)
summary(mlr_overestimation)

#t-tests for differences in overestimation and overplacement by condition
t.test(weight_ests$overestimation ~ weight_ests$Condition)
t.test(weight_ests$overplacement ~ weight_ests$Condition)
