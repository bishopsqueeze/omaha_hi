##------------------------------------------------------------------
##
##------------------------------------------------------------------

##------------------------------------------------------------------
## Load libraries
##------------------------------------------------------------------
library(data.table)

##------------------------------------------------------------------
## Clear the workspace
##------------------------------------------------------------------
rm(list=ls())

##------------------------------------------------------------------
##
##------------------------------------------------------------------
setwd("/Users/alexstephens/Development/poker/data")

##------------------------------------------------------------------
##
##------------------------------------------------------------------
rank   <- c("A",2:9,"T","J","Q","K")


handRankIndex  <- function(h, rank) {
    tmp.table  <- table(strsplit(h, ""))
    return(list(idx=which(rank %in% names(tmp.table)), cnt=as.vector(tmp.table)))
}



##------------------------------------------------------------------
##
##------------------------------------------------------------------

## read the 6-max file
d.raw           <- read.csv("ppt_plo_handrankings_06handed.txt", header=FALSE)
colnames(d.raw) <- "raw.hand"


## append additional characterisitcs
d.raw[,c("nch")]    <- unlist(lapply(as.character(d.raw[,1]), nchar))

d.raw[,c("suit")]   <- ifelse(d.raw$nch == 8, "ds", ifelse(d.raw$nch == 6, "ss", "ns"))

## need tp


## strip suit identifiers
d.raw[,c("cards")]  <- gsub("[()]","",d.raw$raw.hand)

## need to consider the number within the () for the single-suited hands

## create individual hands
d.raw[,c("p1")] <- substr(d.raw$cards,1,2)
d.raw[,c("p2")] <- substr(d.raw$cards,3,4)


## create card-speficic columns
d.raw[,rank] <- 0

## count combos in each hand
for (i in 1:nrow(d.raw)) {
    tmp.cards   <- d.raw$cards[i]
    d.raw[i,handRankIndex(tmp.cards, rank)$idx+4]   <- handRankIndex(tmp.cards, rank)$cnt
}

##------------------------------------------------------------------
##
##------------------------------------------------------------------
n.p1    <- length(unique(d.raw$p1))
n.p2    <- length(unique(d.raw$p2))

d.mat           <- matrix(0, nrow=n.p2, ncol=n.p1)
colnames(d.mat) <- unique(d.raw$p1)
rownames(d.mat) <- unique(d.raw$p2)

for (i in 1:nrow(d.raw)) {
    i.idx               <- grep(d.raw[i,c("p2")] , rownames(d.mat))
    j.idx               <- grep(d.raw[i,c("p1")] , colnames(d.mat))
    d.mat[i.idx,j.idx]  <- d.mat[i.idx,j.idx] + 1
}








