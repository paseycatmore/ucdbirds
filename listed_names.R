# script:  converting ledgers to same format
#           correcting names, formating table, reduced to presence / absence / NA
#
# input:  "ledger_*.csv"
#           digitised record of historic ledger          
#           *entries for 1973 - 2018 if exists
#         "listed_names_taxonomy.csv"
#           manually paired list of scientific names from "listed_names.csv"
#
# output: "listed_names.csv"
#           list of unique common name entries across all ledger entries
#         "ledger_named_*"
#           each ledger formatted identically to as in "ebird_transform.R" 
#           *entries for 1973 - 2018 if exists
#
# author: Casey Patmore
#           casey.patmore@ucdconnect.ie
#
# date:   last modified 26/10/2018

setwd("C:/Users/casey/Desktop/Birds")

rm(list=ls())

library(plyr)
library(dplyr)
library(tidyr)
library(taxize)

#practice run

#ledger_1973 <- read.csv("ledgers/ledger_1973.csv")
#ledger_1995 <- read.csv("ledgers/ledger_1995.csv")

#all_ledgers <- rbind.fill(ledger_1973, ledger_1995)
#names <- unique(all_ledgers$Original_Entry)

#listed_names <- data.frame(names)
#remove weather
#listed_names <- listed_names[ !(listed_names$names=="Weather"), ]
#listed_names <- data.frame(listed_names)

#write.csv(listed_names, file = "listed_names.csv")



#using 1973 as a jumping off point template
all_ledgers <- read.csv("ledgers/ledger_1973.csv")

#append all other years next
ledger_years <- c(1974:2018)

for (year in ledger_years){
  filename = paste("ledgers/ledger_", year, ".csv", sep="")
  if (file.exists(filename)){
    ledger <- read.csv(filename)
    all_ledgers <- rbind.fill(all_ledgers, ledger)
    print(filename)
  }
}

names <- unique(all_ledgers$Original_Entry)

listed_names <- data.frame(names)
#remove weather
listed_names <- listed_names[ !(listed_names$names=="Weather"), ]
listed_names <- data.frame(listed_names)

write.csv(listed_names, file = "listed_names.csv")

#manually stringing scientific names to the common names in the ledger
#output as "listed_names_taxonomy.csv"

rm(list=ls())

list <- read.csv("listed_names_taxonomy.csv")

#replace codes with presence / absence
#
#
# 
#write each as new csv

#next do bird fractions for all

#next merge to form master file
#next format properly by year

#next trim trait database based on master file, generate functional groups and assign those

#analysis can start

ledger_years <- c(1973:2018)
wks_nums <- c(1:52)

for(wk_num in wks_nums) {
  word_week <- paste("Wk_", wk_num, sep = "")
  wks_nums[wk_num] <- word_week
}

for (year in ledger_years){
  filename = paste("ledgers/ledger_", year, ".csv", sep="")
  if (file.exists(filename)){
    ledger <- read.csv(filename)
    ledger <- ledger[!grepl("Weather", ledger$Original_Entry),]
    colnames(ledger)[4:55] <- wks_nums
    colnames(ledger)[2] <- "Name"
    ledger$Record_Type <- NULL
    ledger$Notes <- NULL
    ledger$Name <- list$ebird_name[match(ledger$Name, list$listed_names)]
    filename = paste("ledgers/ledger_named_", year, ".csv", sep="")
    write.csv(ledger,file=filename,row.names=FALSE)
  }
}

for (year in ledger_years){
  filename = paste("ledgers/ledger_named_", year, ".csv", sep="")
  if (file.exists(filename)){
    ledger <- read.csv(filename, stringsAsFactors=FALSE)
    my_species <- ledger$Name
    resolve_species_ledger <- gnr_resolve(data_source_ids = c(11), as.character(ledger$Name))
    for (Species_number in 1:length(ledger$Name)){
      species <- ledger$Name[Species_number]
      resolve_index <- which(resolve_species_ledger$user_supplied_name == species)
      if(length(resolve_index)==0){
        print("Error")
        print(species)
        ledger$Name[Species_number] = "error"
      }
      else{
        ledger$Name[Species_number] = resolve_species_ledger$matched_name[resolve_index]
        #print(resolve_species_ebird$matched_name[index])
        #ledger[rowSums(ledger[, -1] > 0) != 0, ]
        filename = paste("ledgers/ledger_named_", year, ".csv", sep="")
        write.csv(ledger,file=filename,row.names=FALSE)
      }
    }
  }
}

for (year in ledger_years){
  filename = paste("ledgers/ledger_named_", year, ".csv", sep="")
  if (file.exists(filename)){
    ledger <- read.csv(filename, stringsAsFactors=FALSE)
    for(row in 1:nrow(ledger)) {
      #skip first few
      for (col in 3:ncol(ledger)){
        if(is.na(ledger[row, col])){
          #do nothing
        }
        else if(ledger[row, col] == ""){
          ledger[row, col] = 0
        } 
        else{
          ledger[row, col] = 1
        }
          filename = paste("ledgers/ledger_named_", year, ".csv", sep="")
          write.csv(ledger,file=filename,row.names=FALSE)
        }
      }
    }
  }
