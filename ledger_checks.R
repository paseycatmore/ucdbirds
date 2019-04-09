# script: comparing ledger entries for interpretation biases
#           quite a manual process
#
# input:  "ledgers_1973.csv", "ledgers_1988.csv", "ledgers_1992.csv"
#           casey ledger entry for each respective year
#         "ledgers_1973_wg.csv", "ledgers_1988_Wg.csv", "ledgers_1992_wg.csv"
#           willson ledger entry for each respective year
#
# output: "1973_diff.csv", "1988_diff.csv", "1992_diff.csv"
#           all differences identified between entries for each respective year
#         
# author: Casey Patmore
#           casey.patmore@ucdconnect.ie
#
# date:   last modified 26/10/2018

setwd("C:/Users/casey/Desktop/Birds")

library(compare)

#1973

rm(list=ls())

casey1 <- read.csv("ledgers/ledger_1973.csv")
willson1 <- read.csv("ledgers/ledger_1973_wg.csv")

casey1 <- casey1[-c(1), ]
willson1 <- willson1[-c(1,2,3), ]

#excluding those NA columns I've kept
casey1 <- casey1[, grep("^(NA)", names(casey1), value = TRUE, invert = TRUE)]

casey1$Notes <- NULL
willson1$Notes <- NULL

casey1 <- casey1[order(casey1$Original_Entry),]
willson1 <- willson1[order(willson1$Original_Entry),]

compare(casey1, willson1)
check1 <- summary(compare(casey1, willson1))
check1

#specifically reorder names to match, check the "Original_Entry at least match between rows
#(ordering alphabetically a rough shortcut but still need to check for discrepancies in ordering)
willson1 <- willson1[c(1:20,22,21,23:74), ]
rownames(casey1) <- c(1:74)
rownames(willson1) <- c(1:74)

compare(casey1, willson1)

casey1[1:3] <- NULL
willson1[1:3] <- NULL

write.csv(casey1, file="casey1.csv",row.names=FALSE)
write.csv(willson1, file="willson1.csv",row.names=FALSE)

#1988

rm(list=ls())

casey2 <- read.csv("ledgers/ledger_1988.csv")
willson2 <- read.csv("ledgers/ledger_1988_wg.csv")

casey2 <- casey2[-c(1, 83, 84, 85), ]

#excluding those NA columns I've kept
casey2 <- casey2[, grep("^(NA)", names(casey2), value = TRUE, invert = TRUE)]
willson2 <- willson2[, -c(17)]

casey2$Notes <- NULL
willson2$Notes <- NULL

casey2 <- casey2[order(casey2$Original_Entry),]
willson2 <- willson2[order(willson2$Original_Entry),]

compare(casey2, willson2)
check2 <- summary(compare(casey2, willson2))
check2
write.csv(check2$diffs.table, file = "1988_diff.csv")

#specifically reorder names to match, check the "Original_Entry at least match between rows
#(ordering alphabetically a rough shortcut but still need to check for discrepancies in ordering)
willson2 <- willson2[c(1,3,2,4:74,77,75,76,79,78,80,81), ]
rownames(casey2) <- c(1:81)
rownames(willson2) <- c(1:81)

check2 <- summary(compare(casey2, willson2))
check2
write.csv(check2$diffs.table, file = "1988_diff.csv")

casey2[1:3] <- NULL
willson2[1:3] <- NULL

write.csv(casey2, file="casey2.csv",row.names=FALSE)
write.csv(willson2, file="willson2.csv",row.names=FALSE)

#1992

rm(list=ls())

casey3 <- read.csv("ledgers/ledger_1992.csv")
willson3 <- read.csv("ledgers/ledger_1992_wg.csv")

casey3 <- casey3[-c(1), ]
willson3 <- willson3[-c(1), -c(31)]

#excluding those NA columns I've kept
casey3 <- casey3[, grep("^(NA)", names(casey3), value = TRUE, invert = TRUE)]

casey3$Notes <- NULL
willson3$Notes <- NULL

casey3 <- casey3[order(casey3$Original_Entry),]
willson3 <- willson3[order(willson3$Original_Entry),]

compare(casey3, willson3)
check3 <- summary(compare(casey3, willson3))
check3
write.csv(check3$diffs.table, file = "1992_diff.csv")

#specifically reorder names to match, check the "Original_Entry at least match between rows
#(ordering alphabetically a rough shortcut but still need to check for discrepancies in ordering)
willson3 <- willson3[c(1,4,2,3,5:19,21,20,22,24,23,25:83,86,84,85,87:89), ]
rownames(casey3) <- c(1:89)
rownames(willson3) <- c(1:89)

compare(casey3, willson3)
check3 <- summary(compare(casey3, willson3))
check3
write.csv(check3$diffs.table, file = "1992_diff.csv")

casey3[1:3] <- NULL
willson3[1:3] <- NULL

write.csv(casey3, file="casey3.csv",row.names=FALSE)
write.csv(willson3, file="willson3.csv",row.names=FALSE)


####

names <- c("casey", "willson")
numbers <- c(1, 2, 3)

for (name in names){
  for(number in numbers){
    filename = paste(name, number, ".csv", sep="")
    if (file.exists(filename)){
      ledger <- read.csv(filename, stringsAsFactors=FALSE)
      for(row in 1:nrow(ledger)) {
        #skip first few
        for (col in 1:ncol(ledger)){
          if(is.na(ledger[row, col])){
            #do nothing
          }
          else if(ledger[row, col] == ""){
            ledger[row, col] = 0
          } 
          else{
            ledger[row, col] = 1
          }
          filename = paste(name, number, ".csv", sep="")
          write.csv(ledger,file=filename,row.names=FALSE)
        }
      }
    }
  }
}

for (name in names){
  for(number in numbers){
    filename = paste(name, number, ".csv", sep="")
    if (file.exists(filename)){
      ledger <- read.csv(filename, stringsAsFactors=FALSE)
      ledger <- stack(ledger)
      colnames(ledger) <- c(name, "cell")
      filename = paste(name, number, "_stack.csv", sep="")
      write.csv(ledger,file=filename,row.names=FALSE)
    }
  }
}

####

rm(list=ls())

library(irr)

casey1 <- read.csv("casey1_stack.csv")
willson1 <- read.csv("willson1_stack.csv")

casey2 <- read.csv("casey2_stack.csv")
willson2 <- read.csv("willson2_stack.csv")

casey3 <- read.csv("casey3_stack.csv")
willson3 <- read.csv("willson3_stack.csv")

casey <- rbind(casey1, casey2, casey3)
willson <- rbind(willson1, willson2, willson3)

check <- cbind(casey, willson)
check[c(2,4)] <- NULL    

agree(check, tolerance=0)

check_matrix <- as.matrix(check)
check_matrix <- t(check_matrix)
kripp.alpha(check_matrix, "ratio")
