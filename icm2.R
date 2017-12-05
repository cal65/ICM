setwd('C:/Users/212590921/Documents/Personal_Projects/ICM/')
options(stringsAsFactors = F)
library(plyr)
library(reshape)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(icm.table)
library(tidyr)
library(scales)
library(RColorBrewer)
giving <- read.csv('Giving.csv')
meetings <- read.csv('Meetings.csv')
transactions <- read.csv('Transactions.csv')
first_gift <- read.csv('FirstGift.csv')

colnames(giving)[-1] <- gsub('FY', 'Giving', colnames(giving)[-1])
colnames(meetings)[-1] <- gsub('FY', 'Meeting', colnames(meetings)[-1])
colnames(transactions)[-1] <- gsub('FY', 'Trans', colnames(transactions)[-1])

icm <- merge(giving, meetings, all.x=T,incomparables = 0)
icm <- merge(icm, transactions, all.x=T,incomparables = 0)

icm[is.na(icm)] <- 0

first = function(x) {
  for (i in 1:length(x)){
    if (x[i] > 0){
      return(i)
    }
  }
  return(NA)
}
icm$giving_first <- apply(icm[,grep('Giving', colnames(icm))], 1, first)
icm$giving_first <- icm$giving_first + 2012
first_gift <- first_gift[-which(duplicated(first_gift)),]
first_gift$Household..First.Gift.Date <- as.Date(first_gift$Household..First.Gift.Date, format='%m/%d/%Y' )
icm <- merge(icm, first_gift, all.x=T, by="Household..Household.ID")
#How many times do people donate?
times <- apply(giving[,-1], 1, function(x) length(which(x>0)))
times
#Where does ICM's money come from in terms of one-time vs annual donors?
freq_analysis <- ddply(cbind(giving, times), .(times), summarize, count=length(Household..Household.ID),
                       total.2013 = sum(Giving.2013), total.2014=sum(Giving.2014),
                       total.2015 = sum(Giving.2015), total.2016=sum(Giving.2016))
                       
freq_analysis <- subset(freq_analysis, times >0)
freq.m <- melt(freq_analysis, id.vars=c('times', 'count'))
colnames(freq.m) <- c('times','count', 'year', 'total.giving')
freq.m$year <- gsub('total.', '', freq.m$year)
ggplot(freq.m) + geom_col(aes(x=times, y=total.giving, color=year, fill=year), position='stack') +
  ggtitle('Donation by Frequency') + 
  geom_text(aes(x=times, y=0, label=count), vjust=1, color='darkblue')+
  scale_y_continuous(name="Total Donations $", labels=dollar_format())

freq_aves <- ddply(cbind(giving, times), .(times), summarize,
                   mean.2013 = mean(Giving.2013), mean.2014 = mean(Giving.2014),
                   mean.2015 = mean(Giving.2015), mean.2016 = mean(Giving.2016),
                   total = (Giving.2013 + Giving.2014 + Giving.2015 + Giving.2016)/length(Household..Household.ID))

#let's examine if there are different meeting profiles for different giving frequencies
icm$times <- mapvalues(icm$Household..Household.ID, from=giving$Household..Household.ID, to=times)
meetings_onetimers <- subset(icm, times==1, select=grep("Meeting", names(icm), value=T))
icm$total_meetings <- apply(icm[,grep('Meeting', names(icm))],1, sum)
icm$total_gifts <- apply(icm[,grep('Giving.', names(icm))],1, sum)
icm$total_transactions <- apply(icm[,grep('Trans', names(icm))],1, sum)

#useful table
table(icm[,c("total_meetings", 'times')])
ggplot(ddply(icm, .(total_meetings, times), summarize, ave = mean(total_gifts), 
             count=length(total_gifts))[-1,])+
  geom_tile(aes(x=total_meetings, y=times, fill=ave)) + 
  geom_text(aes(x=total_meetings, y=times, label=count))+
  scale_fill_continuous("Average Donation", low='light pink', high='dark blue', labels=dollar_format()) +
  ggtitle('Donation Averages') + 
  theme(plot.title=element_text(hjust=0.5), panel.background = element_blank())

ggplot(subset(icm, total_meetings>0)) + geom_bar(aes(x=total_meetings, fill=times)) +
  scale_fill_manual("Donation Times", values=brewer.pal(5, 'Dark2')) +
  ggtitle('Donations vs Meetings')
ggsave('Donations_vs_Meetings.jpeg', width=7, height=7, dpi=860)

selective <- subset(icm, Giving.2013 == 0 & Giving.2014 >0 & Giving.2015==0 )
ddply(selective, .(Giving.2016>0), summarize, median(Giving.2014), 
      p20 = quantile(Giving.2014, .2), p80=quantile(Giving.2014, .8), n=length(Giving.2014),
      average = mean(Giving.2014))

ggplot(subset(icm, times>0)) + geom_density(aes(x=total_gifts, fill=times)) + facet_grid(times~total_meetings) +
  scale_x_log10()

ggplot(icm) + geom_histogram(aes(total_transactions), bins=200)

melt(table(first_gift$Household..First.Gift.Date)) -> first_gift_dates
colnames(first_gift_dates) <- c('Date', 'Gifts')
first_gift_dates$Date <- as.Date(first_gift_dates$Date, format='%m/%d/%Y')
ggplot(first_gift_dates) + geom_point(aes(x=Date, size=Gifts, y=1, color=format(Date, '%Y')), 
                                      alpha=0.4)



scaled_giving <- as.data.frame(t(scale(t(giving[,-1]), center=F)))
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

scaled_giving[is.nan.data.frame(scaled_giving)] <- 0
wss <- (nrow(scaled_giving)-1)*sum(apply(scaled_giving[, -1],2,var))
for (i in 2:15) wss[i] <- sum(kmeans(scaled_giving,
                                     centers=i)$withinss)
ggplot() + geom_point(aes(x=1:15, y=wss)) + ggtitle('KMeans within Groups Sum of Squares') 
kcm <- kmeans(scaled_giving, centers=10)

giving_scaled <- cbind(ID = giving$Household..Household.ID, scaled_giving, clusters=kcm$cluster)
giving_scaled[,grep('Giving', colnames(giving_scaled))] <- apply(giving_scaled[,grep('Giving', colnames(giving_scaled))], 1, as.numeric)
giving_scaled <- as.data.frame(giving_scaled)
giving.scaled.m <- melt(giving_scaled, id.vars=c("ID", 'clusters'))
giving.scaled.m$value <- as.numeric(giving.scaled.m$value)
giving.scaled.m$variable <- as.numeric(gsub('Giving.', '', giving.scaled.m$variable))

ggplot(giving.scaled.m) + geom_point(aes(x=variable, y=value, color=as.factor(clusters)))

kcm.m <- melt(kcm$centers)
kcm.m$X2 <- as.numeric(gsub('Giving.', '', kcm.m$X2))
kcm.m$value <- as.numeric(kcm.m$value)
ggplot(kcm.m) + geom_line(aes(x=X2, y=value, color=as.factor(X1)), size=1.5) +
  facet_grid(X1 ~.) + theme(panel.background = element_blank())

icm$max <- apply(icm[,grep('Giving', names(icm))], 1, max)
subset(icm[apply(icm[,grep('Giving', names(icm))], 1, max)<20000,]) -> bucket1
bucket1.m <- melt(bucket1[,1:6])
bucket1.m$variable <- gsub('Giving', '', bucket1.m$variable)
bucket1.m$value <- as.numeric(bucket1.m$value)
ggplot(bucket1.m) + geom_col(aes(x=variable, y =value))
subset(icm, max > 20000 & max < 200000) -> bucket2
bucket2.m <- melt(bucket2[,1:6])
bucket2.m$variable <- gsub('Giving', '', bucket2.m$variable)
bucket2.m$value <- as.numeric(bucket2.m$value)
subset(icm, max > 200000) -> bucket3
bucket3.m <- melt(bucket3[,1:6])
bucket3.m$variable <- gsub('Giving', '', bucket3.m$variable)
bucket3.m$value <- as.numeric(bucket3.m$value)

bucket1.m$bucket <- "max < 20,000"
bucket2.m$bucket <- "20,000 < max < 200,000"
bucket3.m$bucket <- "max > 200,000"
all_buckets <-rbind(bucket1.m, bucket2.m, bucket3.m)
ggplot(all_buckets) + geom_col(aes(x=variable, y =value, fill=bucket)) +
  facet_grid(bucket~.)
