# script: trimming and renaming european bird trait database to only include relevant data
#           (Storchová L, Horák D, Hurlbert A, 2018)
#
# input:  "traits.csv"
#           original european bird trait database
#         "final_long_list.csv"
#           long format of all data combined (see master_df.R)
#
# output: "GBIF_traits.csv"
#           traits species names replaced according to GBIF (taxized)
#         "traits_trimmed.csv"
#           trait database trimmed to only include observed species 
#         "traits_final.csv"
#           traits database w/ species and traits selected for clustering
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
library(tidyr)
library(qdapTools)

rm(list=ls())

#ebird_table_code.csv
#traits.csv
#ledgers_*.csv
#all want to be under the same naming convention, taxize with GBIF
#
#ebird_table_code.csv done through ebird_transform.R
#ledgers will need to first manually be matched to ebird_taxonomy
#for now just working on traits.csv

rm(list=ls())

traits <- read.csv("traits.csv", stringsAsFactors=FALSE)

my_species <- traits$Species

#src <- c("NCBI", "Index to Organism Names", "GBIF Backbone Taxonomy", "Catalogue of Life", "EOL")
#subset(gnr_datasources(), title %in% src)

resolve_species_traits <- gnr_resolve(data_source_ids = c(11), as.character(traits$Species))

#match $Species with $user_supplied_name and replace with $matched_name

for (Species_number in 1:length(traits$Species)){
  species <- traits$Species[Species_number]
  resolve_index <- which(resolve_species_traits$user_supplied_name == species)
  if(length(resolve_index)==0){
    print("Error")
    print(species)
    traits$Species[Species_number] = "error"
  }
  else{
    traits$Species[Species_number] = resolve_species_traits$matched_name[resolve_index]
    #print(resolve_species_ebird$matched_name[index])
  }
}

write.csv(traits, file = "GBIF_traits.csv")

#test idea
#traits database trimmed to only include what birds recorded
#so rerun later using a "master" spreadsheet of all ledgers + 2018 ebird data

rm(list=ls())

named_traits <- read.csv("GBIF_traits.csv", stringsAsFactors=FALSE)
final_long <- read.csv("final_long_list.csv", stringsAsFactors=FALSE)

unique_long <- unique(final_long$Name)
unique_long <- as.data.frame(unique_long)

traits_trimmed <- subset(named_traits, Species %in% final_long$Name)

traits_trimmed <- subset(traits_trimmed, select=colMeans(is.na(traits_trimmed)) == 0) 
traits_trimmed <- traits_trimmed

#Young <- cbind(traits_trimmed[1], mtabulate(strsplit(as.character(traits_trimmed$Young), ",")))
#Mating.system <- cbind(traits_trimmed[1], mtabulate(strsplit(as.character(traits_trimmed$Mating.system), ",")))
#Association.outside.the.breeding.season <- cbind(traits_trimmed[1], mtabulate(strsplit(as.character(traits_trimmed$Association.outside.the.breeding.season), ",")))

#traits_trimmed <- cbind(traits_trimmed, Young[, -1])
#traits_trimmed <- cbind(traits_trimmed, Mating.system[, -1])
#traits_trimmed <- cbind(traits_trimmed, Association.outside.the.breeding.season[, -1])

#traits_trimmed <- traits_trimmed[, -c(25,26,30)]

#traits_trimmed <- do.call(rbind,lapply(lapply(split(traits_trimmed,traits_trimmed$Species),`[`,2:ncol(traits_trimmed)),colMeans))

write.csv(traits_trimmed, file = "traits_trimmed.csv")

worth <- c("Species", 
           "Deciduous.forest", "Coniferous.forest", "Woodland", "Shrub", "Grassland", 
           "Rocks", "Human.settlements")

wanted_traits <- traits_trimmed[worth]
wanted_traits$green <- rowSums(wanted_traits[2:6])
wanted_traits$grey <- rowSums(wanted_traits[7:8])
wanted_traits <- wanted_traits[, c(1,9:10)]

for(row in 1:nrow(wanted_traits)) {
  #skip first few
for (col in 2:ncol(wanted_traits)){
  if(is.na(wanted_traits[row, col])){
    #do nothing
  }
  else if(wanted_traits[row, col] == "0"){
    wanted_traits[row, col] = 0
  } 
  else{
    wanted_traits[row, col] = 1
  }
}
}

wanted_traits$facultative <- 0; wanted_traits$bothered <- 0

for(row in 1:nrow(wanted_traits)){
  if( (wanted_traits$green[row] == 1) & (wanted_traits$grey[row] == 1)){
    wanted_traits$facultative[row] = 3
  }
  else if( (wanted_traits$green[row] == 0) & (wanted_traits$grey[row] == 0)){
    wanted_traits$bothered[row] = 4
  }
}

for(row in 1:nrow(wanted_traits)){
  if( (wanted_traits$facultative[row] == 3)){
    (wanted_traits$green[row] = 0) & (wanted_traits$grey[row] = 0)
  }
}

for(row in 1:nrow(wanted_traits)){
  if( wanted_traits$grey[row] == 1){
    wanted_traits$grey[row] = 2
  }
}

wanted_traits$clusters <- rowSums(wanted_traits[2:5])

wanted_traits <- wanted_traits[!duplicated(wanted_traits[, c('Species')]),]

write.csv(wanted_traits, file = "traits_final.csv", row.names = TRUE)
#should work?