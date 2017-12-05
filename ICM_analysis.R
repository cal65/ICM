setwd('C:/Users/212590921/Documents/Personal_Projects/ICM/')
options(stringsAsFactors = F)
library(plyr)
library(reshape)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(data.table)
library(tidyr)
activities <- read.csv('Activities.csv')
activities$Due.Date <- as.Date(activities$Due.Date, format='%m/%d/%Y')
ggplot(activities) + geom_boxplot(aes(x=Type, y=Total.Household.Gifts, color=Type))
ggplot(activities) + geom_point(aes(x=Due.Date, y=Total.Household.Gifts, 
                                    color=Type), alpha=0.3)
table(activities$Type, activities$Total.Household.Gifts)
at <- data.table(activities)
act_analysis <- as.data.frame(at[,unique(Total.Household.Gifts), by=.(Contact.ID, Type)])
colnames(act_analysis)[3] <- 'Value'
ggplot(act_analysis) + geom_boxplot(aes(x=Type, y=Value, color=Type))
ddply(act_analysis, .(Type), summarize, mean_value=mean(Value), median_value=median(Value))

individual <- unique(activities[c('Contact.ID', 'Total.Household.Gifts')])

type_table <- at[ , .(Type = paste(Type, collapse="@")), by = Contact.ID]
at1 <- spread(type_table, Type, Contact.ID, fill = " ")

unique_types <- sort(unique(activities$Type))
type_df <- ddply(activities, .(Contact.ID), summarize, 
                 blank = length(which(Type==unique_types[1])),
                 others = length(which(Type==unique_types[2])),
                 emails = length(which(Type==unique_types[3])),
                 mails = length(which(Type==unique_types[4])),
                 calls = length(which(Type==unique_types[5])),
                 meetings = length(which(Type==unique_types[6])),
                 presentations = length(which(Type==unique_types[7])),
                 events = length(which(Type==unique_types[8])))
type_df <- merge(type_df, individual, by='Contact.ID')

#first regression using all the variables
lm1 <- lm(data=type_df[,-1], Total.Household.Gifts ~ .)

#need to transform variables to pursue a logistic
type_df$Gave <- type_df$Total.Household.Gifts!=0
glm1 <- glm(data=type_df, Gave ~ emails + meetings)
summary(glm1)
glm2 <- glm(data=type_df, Gave ~ emails + mails + calls + meetings + presentations)
summary(glm2)

eestimates <- numeric(30)
for (i in 1:length(eestimates)){
  type_df$bulk_email <- type_df$emails > (i+4)
  assign(paste('email_glm',i, sep="_"), glm(data=type_df, Gave ~ bulk_email + (mails> 0) + (calls>0) + (meetings>0)))
  current <- get(paste('email_glm',i, sep="_")) 
  eestimates[i] <- coefficients(current)[2]
}

type_df$didMail <- type_df$mails != 0
type_df$didCall <- type_df$mails != 0

plot(allEffects(glm2), ylim=c(0,2.4))
