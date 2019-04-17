# ucdbirds
Analysing historic bird data for University College Dublin (UCD)

Historic bird observation records collected from University College Dublin (UCD), between 1973 and 2018. 

Each "ledger_X".csv is a digitised copy of the original ledger pages (1973 - 1998) for that year, X.
Each "ledger_X_wg_.csv" is a second version that had been input by another researcher for comparison.

"MyEBirdData.csv" is original data retrieved from surveys collected during 2018

"traits.csv" is data from the European Bird Trait Database, accompanied by "traits_meta.txt" to describe the categories
This has been used to develop species-habitat association groups for each species included in our analysis ("traits_final.csv")

"outputs.R" covers the main outputs (estimations of species richness, plots + diagnostics for whole community and functional groups analysis).

"data_table.csv" is a summary of the weeks that have been sampled (1) or not sampled (0) across all included years
