setwd("C:/Users/casey/Desktop/Birds")

library(reshape)
library(data.table)
library(taxize)
library(dplyr)
library(magrittr)

rm(list=ls())

#ebird_table_code.csv
#traits.csv
#ledgers_*.csv
#all want to be under the same naming convention, taxize with GBIF
#
#ledgers will need to first manually be matched to ebird_taxonomy

ebird_table_code <- read.csv("ebird_table_code.csv", stringsAsFactors=FALSE)

my_species <- ebird_table_code$X

src <- c("NCBI", "Index to Organism Names", "GBIF Backbone Taxonomy", "Catalogue of Life", "EOL")
subset(gnr_datasources(), title %in% src)

resolve_species_ebird <- gnr_resolve(data_source_ids = c(11), as.character(ebird_table_code$X))

#match $X with $user_supplied_name and replace with $matched_name

for (X_number in 1:length(ebird_table_code$X)){
  species <- ebird_table_code$X[X_number]
  resolve_index <- which(resolve_species_ebird$user_supplied_name == species)
  if(length(resolve_index)==0){
    print("Error")
    print(species)
    ebird_table_code$X[X_number] = "error"
  }
  else{
    ebird_table_code$X[X_number] = resolve_species_ebird$matched_name[resolve_index]
    #print(resolve_species_ebird$matched_name[index])
  }
}

write.csv(ebird_table_code, file = "GBIF_ebird_table_code.csv")



rm(list=ls())

traits <- read.csv("traits.csv", stringsAsFactors=FALSE)

my_species <- traits$Species

src <- c("NCBI", "Index to Organism Names", "GBIF Backbone Taxonomy", "Catalogue of Life", "EOL")
subset(gnr_datasources(), title %in% src)

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
#
#rm(list=ls())
#
#ebird_table_code <- read.csv("GBIF_ebird_table_code.csv")
#traits <- read.csv("GBIF_traits.csv")
#
#aggregate_ebird_table_code <- aggregate(.~ X, data = ebird_table_code, sum)
#
#subset(traits, Species %in% subset_ebird_table_code$X)
