setwd('C:/Users/212590921/Documents/Personal_Projects/ICM/')
options(stringsAsFactors = F)
library(plyr)
library(reshape)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(data.table)
library(tidyr)
library(scales)
library(RColorBrewer)
library(effects)
report <- read.csv('ICM Donations Report 20171010.csv')
report <- data.table(report)
report <- report[!is.na(report$Amount)]
report$Close.Date <- as.Date(report$Close.Date, format='%m/%d/%Y')
report <- report[order(Close.Date)]
names(report)[4] <- 'Household.ID'
names(report)[5] <- 'Organization.ID'
intro <- read.csv('Intro to ICM.csv')
activities <- read.csv('ICM Activities Data.csv')
activities <- data.table(activities)

#let's identify year of first donation
first_donation <- report[Household..Household.ID != '', .(first_date = min(Close.Date), last_date=max(Close.Date),
                                                          n=length(Close.Date)),
                         by=Household..Household.ID]
#need to cut by Fiscal year, which goes from June 1 to May 30 in Hong Kong
first_donation$first_year <- cut(first_donation$first_date, 
                           c(seq(as.Date('2003-06-01'), as.Date('2018-06-01'), 'years')),
                           paste0('FY', 2003:2017))
first_donation$last_year <- cut(first_donation$last_date, 
                                 c(seq(as.Date('2003-06-01'), as.Date('2018-06-01'), 'years')),
                                 paste0('FY', 2003:2017))
#nwo we
table(first_donation$first_year)
table(first_donation[first_year=='FY2014']$last_year)
report$FY <- cut(report$Close.Date, 
                 c(seq(as.Date('2003-06-01'), as.Date('2018-06-01'), 'years')),
                 paste0('FY', 2003:2017))
donation_years <- report[, .(total=sum(Amount), ave=mean(Amount), 
                             n=length(unique(Household..Household.ID))),
                             by=FY]

#recreate the bands talk
report$cut <- cut(report$Amount, c(0, 20000, 80000, 1.5e5, Inf), 
                  c('0-20k', '20-80k', '80-150k', '150k+'))
bands <- report[FY %in% c('FY2013', 'FY2014', 'FY2015', 'FY2016', 'FY2017'),
                .(total=sum(Amount), av=mean(Amount)), by=c('cut', 'FY')]
bands <- bands[!is.na(cut)]
bands.cast <- dcast(bands, cut ~ FY, value.var = c('total', 'av'))
ggplot(bands) + geom_col(aes(x=FY, y=total, fill=FY)) + facet_grid(cut~.) +
  scale_y_continuous(label=dollar) + ggtitle('Donations by Year and Band') +
  geom_text(aes(x=FY, y=0, label=dollar(av)), vjust=-1) + 
  theme(panel.background = element_blank(), plot.title=element_text(hjust=0.5)) 
ggsave('Bands_Plot.jpeg', width=10, height=9, dpi=650)

ggplot(bands) + geom_col(aes(x=FY, y=av, fill=FY)) + facet_grid(cut~., scales='free') +
  scale_y_continuous(label=dollar) + ggtitle('Donations by Year and Band') +
  geom_text(aes(x=FY, y=0, label=dollar(total)), vjust=-1) + 
  theme(panel.background = element_blank(), plot.title=element_text(hjust=0.5))

by_donor <- report[Household.ID != '' , .(n=length(Amount), total=sum(Amount), 
                  first=head(Amount,1), ave = mean(Amount), date=min(Close.Date), country=head(Contact..Primary.Country,1)), 
                   by=Organization.ID]
by_donor <- merge(by_donor, intro, by='Organization.ID', all.x=T)
by_donor <- by_donor[order(total, decreasing=T)]
ggplot(by_donor) + geom_col(aes(x=n, y=total))
by_donor$simp_n <- ifelse(by_donor$n > 9, 10, by_donor$n)
intro_simplifier <- read.csv('Introduction_Table.csv')
by_donor$simp_intro <- mapvalues(by_donor$Introduction.to.ICM, from=intro_simplifier$Intro,
                                 to=intro_simplifier$Simplified)
by_donor$band <- cut(by_donor$ave, c(0, 20000, 80000, 1.5e5, Inf), 
    c('0-20k', '20-80k', '80-150k', '150k+'))
intro_bands <- as.data.frame.matrix(table(by_donor[,c('simp_intro', 'band')]))
intro_bands$intro <- row.names(intro_bands)
intro_bands.m <- melt(intro_bands, id.vars = 'intro')
ggplot(subset(intro_bands.m, intro != 'Other')) + geom_col(aes(x=intro, y=value, fill=intro)) +
  facet_grid(variable ~ ., scales='free') + scale_fill_brewer(palette = 'Set1')+
  theme(axis.text.x = element_text(angle=45, vjust=0.5), plot.title = element_text(hjust=0.5)) + 
  ggtitle('Profile by Band')
ggsave('Introductions_by_band.jpeg', width=10, height=8, dpi=500)
ggplot(by_donor[!is.na(band)]) + geom_boxplot(aes(x=band, y=n), fill='dark red') + ylim(0,50) +
  ylab('Number of Donations') + ggtitle('Number of Donations per Band') +
  theme(panel.background = element_blank(), plot.title = element_text(hjust=0.5))
#ggsave('Number_by_band.jpeg', width=10, height=8, dpi=500)
donation_count <- as.data.frame(table(by_donor$n))
donation_count$cumsum <- cumsum(donation_count$Freq)
donation_count$percent <- donation_count$cumsum/sum(donation_count$Freq)
donation_count$Var1 <- as.numeric(donation_count$Var1)
names(donation_count)[1] <- 'n'
ggplot(donation_count) + geom_line(aes(x=n,y=percent))

donation_values <- by_donor[, .(total=sum(total), ave = mean(ave)), by=simp_n][order(simp_n)]
ggplot(donation_values) + geom_col(aes(y=ave, x=as.factor(simp_n)), fill='dark blue')+
  geom_col(aes(y=total, x=as.factor(simp_n)), fill='dark red') +
  theme(panel.background = element_blank()) + coord_flip() + scale_y_continuous(label=dollar)

donation_values$percent <- with(donation_values, total/sum(total))
report[Contact..Contact.ID == '0039000000HJN2b']

types <- report[,.(total=sum(Amount)), by='Donation.Record.Type']
types <- types[order(total)]
types$Donation.Record.Type <- factor(types$Donation.Record.Type, levels=types$Donation.Record.Type)
ggplot(types) + geom_col(aes(y=total, x=Donation.Record.Type), fill='dark red') +
  theme(panel.background = element_blank()) + coord_flip() + scale_y_continuous(label=dollar)


#logistic regression: what size of the first donation implies a second donation
glm(data=by_donor, n > 1 ~ first) -> glm1 
glm(data=by_donor[first>0], n > 1 ~ log(first)) -> glm2
plot(allEffects(glm2), ylim=c(0,1), xlim=c(0, 400000))
ggplot(by_donor) + geom_point(aes(x=log(first), y=(n>1)), alpha=0.2)
exp((.5 - coefficients(glm2)[1]) / coefficients(glm2)[2])
plot(density(summary(log(by_donor[first>0]$first))))

by_donor$breaks <- cut(by_donor$first, breaks=c(0, 1000, 5000, 10000, 15000, Inf), 
                       labels=c('<$1k', '$1-5k', '$5-10k', '$10-15k', '$15k+'), right=F)
percent_recur <- by_donor[, .(over1 = length(which(n>1))/length(n), over2=length(which(n>2))/length(n)),
                          by=breaks]
percent_recur$breaks <- factor(percent_recur$breaks, 
                               levels=c('<$1k', '$1-5k', '$5-10k', '$10-15k', '$15k+'))
percent_recur.m <- melt(percent_recur, id.var='breaks')
ggplot(percent_recur.m) + geom_col(aes(x=breaks, y=value, fill=variable), position = 'dodge') + 
  scale_y_continuous(labels=percent, limits=c(0,1)) +
  ylab('Chance of Becoming Repeat Donor') + 
  scale_fill_brewer('Number of Donations', palette='Set2',
                    labels=c('More Than Once', 'More Than Twice')) +
  theme(panel.background = element_blank())
                               
ggsave('Repeats.jpeg', width=11, height=7, dpi=500)

#analyze with activities
names(activities)[1] <- 'ID'
activities <- activities[ID != '']
activities$Type <- gsub('[0-9 ]', '', activities$Type)
activity_types <- as.data.frame.matrix(table(activities[, c('ID', 'Type')]))
activity_types$Contact..Contact.ID <- row.names(activity_types)
activity_types <- activity_types[,-1]
by_donor_merged <- merge(by_donor, activity_types, by='Contact..Contact.ID', all.x=T)
customer_train <- by_donor_merged[!is.na(Email), c('n', 'total', 'ave', 'Call', 'Mail', 'Meeting')]
c_t <- scale(customer_train)
c_t <- as.data.frame(c_t)
c_t <- data.table(c_t)
c_t$Meeting <- c_t$Meeting/4
tree1 <- hclust(dist(c_t))
n <- 10
table(cutree(tree1, n))
c_t$cluster <- unlist(cutree(tree1, n))
head(c_t)
c_t[, .(n=mean(n), total=mean(total), ave=mean(ave), Call=mean(Call)), by=cluster]
