# script: combining each individual yearly record table
#           merging outputs from "ebird_transform.R" and "listed_names.R"
#
# input:  "ledger_named_*.csv"
#           as obtained from "listed_names.R"
#           *entries for 1973 - 2018 if exists
#         "GBIF_ebird_table_code_year.csv"
#           as obtained from "ebird_transform.R"
#         "clusters.csv"
#           functional group clusterings from "clustering.R"
#
# output: "final_long_list.csv"
#           complete merge of all record years, with weeks, in long format
#         "final_long_clusters.csv"
#           cluster groups attached
#
# author: Casey Patmore
#           casey.patmore@ucdconnect.ie
#
# date:   last modified 10/11/2018

setwd("C:/Users/casey/Desktop/Birds")

rm(list=ls())

library(plyr)
library(dplyr)
library(tidyr)
library(data.table)

#using 1973 as a jumping off point template
all_ledgers <- read.csv("ledgers/ledger_named_1998.csv")

#append all other years next
ledger_years <- c(1973:1997)

for (year in ledger_years){
  filename = paste("ledgers/ledger_named_", year, ".csv", sep="")
  if (file.exists(filename)){
    ledger <- read.csv(filename)
    all_ledgers <- rbind.fill(all_ledgers, ledger)
    print(filename)
  }
}

ebird_2018 <- read.csv("GBIF_ebird_table_code_year.csv")
colnames(ebird_2018)[1] <- "Name"

all_years <- rbind.fill(all_ledgers, ebird_2018)

new <- gather(data = all_years, 
             key = Week, 
             value = Record, 
             -c(Name, Year))

write.csv(new, file = "final_long_list.csv" ,row.names=FALSE)

agg <- aggregate(new,
                 by = list(new$Year, new$Week),
                 FUN = mean)

agg <- agg[, c(2,4,6)]

years <- unique(agg$Year)
weeks <- c(paste("Wk_", 1:52, sep=""))

data <- data.frame(matrix(ncol = 52, nrow = 20))
colnames(data) <- weeks
rownames(data) <- years

for(year in years){
  for(week in weeks){
    sample =  agg[ which(agg$Group.2==week & agg$Year == year), ]
    if(is.na(sample$Record[1])){
      data[as.character(year), week] = 0
    }
    else{
      data[as.character(year), week] = 1
    }
  }
}
write.csv(data, file = "data_table.csv")

data <- data[9:36]

data <- data[rowSums(abs(data)) != 0,]

write.csv(data, file = "data_table.csv")

#put data table to use

keep_years <- unique(row.names(data))
keep_years <- as.integer(keep_years)

trimmed_years_data <- data.frame()
for (year in keep_years){
  print(year)
  year_data <- new[which(new$Year == year),]
  trimmed_years_data <- rbind(trimmed_years_data, year_data)
}

new <- trimmed_years_data

keep_weeks <- paste("Wk_", c(9:36), sep="")

trimmed_weeks_data <- data.frame()
for (week in keep_weeks){
  print(week)
week_data <- new[which(new$Week == week),]
trimmed_weeks_data <- rbind(trimmed_weeks_data, week_data)
}

new <- trimmed_weeks_data

write.csv(new, file = "final_long_list.csv" ,row.names=FALSE)
                 
#cleared up data

clusters <- read.csv("traits_final.csv")
clusters$X <- NULL
names(clusters)[1]<-"Name"

new$clusters <- clusters$clusters[match(new$Name, clusters$Name)]

write.csv(new, file = "final_long_clusters.csv", row.names=FALSE)
