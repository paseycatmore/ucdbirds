# script: converting and trimming ebird data into ledger format
#           to be included only as a "2018" entries from "UCD" sites
#
# input:  "MyEBirdData.csv"
#           ucd bird record data downloaded directly from ebird
#
# output: "ebird_table.csv"
#           formatted into table of weekly species observations for 2018 only at UCD
#         "removed_ebird_table.csv"
#           unidentified species aggregates removed from table 
#         "ebird_table_code.csv"
#           abundance counts reduced to presence / absence / NA records
#         "GBIF_ebird_table_code.csv"
#           ebird species names replaced according to GBIF (taxized)
#         "GBIF_ebird_table_code_year.csv"
#           modified "GBIF_ebird_table_code.csv" to include year column

#
# author: Casey Patmore
#           casey.patmore@ucdconnect.ie
#
# date:   last modified 26/10/2018

setwd("C:/Users/casey/Desktop/Birds")

library(reshape)
library(data.table)
library(taxize)
library(dplyr)
library(magrittr)

rm(list=ls())

ebird_original <- read.csv("eBird\\MyEBirdData.csv")
ebird <- ebird_original[ebird_original$Location %like% "University College Dublin", ]
ebird <- ebird[ebird$Date %like% "2018", ]

ebird$Fix.Date <- strptime(ebird$Date, format = "%m-%d-%Y")
ebird$Wk_ <- strftime(ebird$Fix.Date, format="%V")

#Generate a list of unique species names
species_list <- unique(ebird["Scientific.Name"])

#Do the same thing with weeks
species_list <- unique(ebird["Scientific.Name"])
num_species <- dim(species_list)[1]

wks_nums <- c(1:52)

for(wk_num in wks_nums) {
  word_week <- paste("Wk_", wk_num, sep = "")
  wks_nums[wk_num] <- word_week
}

ebird_table <- data.frame(matrix(ncol = 52, nrow = num_species))
colnames(ebird_table) <- wks_nums
rownames(ebird_table) <- species_list$Scientific.Name

for (row in 1:nrow(ebird)) {
  species <- ebird[row,"Scientific.Name"]
  week <- ebird[row, "Wk_"]
  count <- ebird[row, "Count"]
  species_number <- match(species, rownames(ebird_table))
  week_number <- match(paste("Wk_", strtoi(week, base=10), sep=""), colnames(ebird_table))
  test <- ebird_table[species_number, week_number]
  if (is.na(test)){
    ebird_table[species_number, week_number] <- count
  }
  else {
    ebird_table[species_number, week_number] <- count + ebird_table[species_number, week_number]
  }
}

write.csv(ebird_table, file = "ebird_table.csv")

#Remove any name containing "sp."
removed_names <- c()
for (name in rownames(ebird_table)){
  if (grepl("sp.",name)){
    print(name)
    removed_names <- c(removed_names, name)
  }
}
removed_ebird <- ebird_table[!row.names(ebird_table) %in% removed_names,]

write.csv(removed_ebird, file = "removed_ebird_table.csv")

ebird_table_code <- removed_ebird
#Now, replace with codes
for (wk in colnames(removed_ebird)) {
  for ( species in rownames(removed_ebird)) {
    species_number <- match(species, rownames(removed_ebird))
    week_number <- match(wk, colnames(removed_ebird))
    test_this_cell <- removed_ebird[species_number, week_number]
    test_all_species <- all(is.na(c(removed_ebird[, wk])))
    if (is.na(test_this_cell)){
      if (test_all_species != TRUE) {
        removed_ebird[species_number, week_number] = 0
        ebird_table_code[species_number, week_number] = "0"
      }
    }
    else if (test_this_cell > 0){
      ebird_table_code[species_number, week_number] = "1"
    }
    else{
      print(paste("error for", wk, species, sep=" "))
    }
  }
}

write.csv(ebird_table_code, file = "ebird_table_code.csv")

my_species <- row.names(ebird_table_code)

#src <- c("NCBI", "Index to Organism Names", "GBIF Backbone Taxonomy", "Catalogue of Life", "EOL")
#subset(gnr_datasources(), title %in% src)

resolve_species_ebird <- gnr_resolve(data_source_ids = c(11), as.character(my_species))
#Make an empty copy of ebird_table_code to record non-duplicate species
ebird_table_unique <- ebird_table_code[0,]
  
#match $X with $user_supplied_name and replace with $matched_name
matched_species_list <- c()
#Keep track of row index in ebird_table_unique:
unique_row <- 0
for (X_number in 1:length(row.names(ebird_table_code))){
  species <- row.names(ebird_table_code)[X_number]
  resolve_index <- which(resolve_species_ebird$user_supplied_name == species)
  if(length(resolve_index)==0){
    print("Error")
    print(species)
    row.names(ebird_table_code)[X_number] = "error"
    ebird_table_code <- ebird_table_code[rownames(ebird_table_code) != "error", ]
  }
  else{
    matched_species <- resolve_species_ebird$matched_name[resolve_index]
    matched_duplicate_index <- which(matched_species_list == matched_species)
    if (length(matched_duplicate_index)!=0){
      matched_name <- matched_species_list[matched_duplicate_index]
      original_name <- resolve_species_ebird$user_supplied_name[resolve_index]
      matched_row <- sapply(ebird_table_unique[matched_name,],as.numeric)
      original_row <- sapply(ebird_table_code[original_name,], as.numeric)
      #print( rowSums(cbind( matched_row, original_row )))
      ebird_table_unique[matched_name, ] <- rowSums(cbind( matched_row, original_row ))
      for(Y_number in 1:length(ebird_table_unique[matched_name,])){
        if(!is.na(ebird_table_unique[matched_name, Y_number]) & 1 < ebird_table_unique[matched_name, Y_number]){
          ebird_table_unique[matched_name, Y_number] = 1
        }
      }
    }
    else{
      unique_row = unique_row +1
      ebird_table_unique[unique_row,] = ebird_table_code[X_number,]
      row.names(ebird_table_unique)[unique_row] = matched_species
      matched_species_list <- c(matched_species_list, matched_species)
    }
  }
}

write.csv(ebird_table_unique, file = "GBIF_ebird_table_code.csv")

#Remove any name containing "subsp."
removed_names <- c()
for (name in rownames(ebird_table_unique)){
  if (grepl("subsp.",name)){
    print(name)
    removed_names <- c(removed_names, name)
  }
}
removed_ebird <- ebird_table_unique[!row.names(ebird_table_unique) %in% removed_names,]

write.csv(removed_ebird, file = "removed_ebird_table.csv")

removed_ebird$Year <- '2018'
problems <- rownames(removed_ebird) %in%
  c("Ichthyaetus melanocephalus (Temminck, 1820)", 
              "Corvus corone Linnaeus, 1758")
with_year <- removed_ebird[!problems ,c(53, 1:52)]
write.csv(with_year, file = "GBIF_ebird_table_code_year.csv")
