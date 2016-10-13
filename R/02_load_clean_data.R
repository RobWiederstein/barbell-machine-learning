#libraries
library(caret)
library(mlbench)
library(ggplot2)
library(latice)
library(doMC)

#download testing set
url01 <-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
destfile01 <- "./data/pml_testing.csv"
if(!file.exists(destfile01)) download.file(url = url, destfile = destfile,
                                         method = "curl"
                                         )
#download training set
url02 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
destfile02 <- "./data/pml-training.csv"
if(!file.exists(destfile02)) download.file(url = url, destfile = destfile,
                                         method = "curl")

#read & combine
training <-read.csv(file = destfile02, header = T, stringsAsFactors = F)
training$problem_id <- 1:nrow(training)
testing <- read.csv(file = destfile01, header = T, stringsAsFactors = F)
testing$classe <- "unk"

#bb stands for "barbell"
bb <- rbind(training, testing)
seed <- 107

#####Whoa lots of missing values!
cols.na <- apply(bb, 2, function(x)(sum(is.na(x))))
bb <- bb[, which(cols.na == 0) ]
rows.na <- apply(bb, 1, function(x)(sum(is.na(x))))
bb <- bb[which(rows.na == 0), ]

###eliminate ID columns
cols.id <- 1:7
bb <- bb[, -cols.id]

##pull Testing back out
testing1 <- bb[19623:nrow(bb), ]
col.drop <- grep("classe", names(testing1))
testing1 <- testing1[, -col.drop]
training1 <- bb[1:19622, ]
col.drop <- grep("problem_id", names(training1))
training1 <- training1[, -col.drop]

#####Dataset too large.  Takes forever to compute
file <- "./data/pml_training_clean.csv"
write.csv(training1, file = file, row.names = F)
file <- "./data/pml_testing_clean.csv"
write.csv(testing1, file = file, row.names = F)
rm(list = ls())
