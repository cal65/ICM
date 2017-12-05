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
report <- read.csv('ICM Donations Report.csv')
report <- data.table(report)
report <- report[!is.na(report$Amount)]

by_donor <- report[Contact..Contact.ID != '' , .(n=length(Amount), total=sum(Amount)), by=Contact..Contact.ID]
ggplot(by_donor) + geom_col(aes(x=n, y=total))


report[Contact..Contact.ID == '0039000000HJN2b']
