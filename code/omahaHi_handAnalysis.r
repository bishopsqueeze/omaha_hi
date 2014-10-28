##------------------------------------------------------------------
## The purpose of this script is to parse the Pro Poker Tools (PPT)
## hand ranking file
##------------------------------------------------------------------

##------------------------------------------------------------------
## Clear the workspace
##------------------------------------------------------------------
rm(list=ls())

##------------------------------------------------------------------
## Load data
##------------------------------------------------------------------

## 6max simulation data
setwd("/Users/alexstephens/Development/poker/omaha_hi/data/simulations")
load("hotColdEquity_6max_OmahiHi_AllHands.Rdata")

## 6max hand attributes
setwd("/Users/alexstephens/Development/poker/omaha_hi/data/pptoo/OmahaHi")
load("ppt_plo_handrankings_06handed.Rdata")

##------------------------------------------------------------------
## append a cumulative %
##------------------------------------------------------------------
hand.mat        <- plo_6max.list[["hd"]]
hand.mat$wgt    <- hand.mat$shapeWgt / sum(hand.mat$shapeWgt)
hand.mat$cumwgt <- cumsum(hand.mat$shapeWgt / sum(hand.mat$shapeWgt))

eqty.mat        <- cbind(res.mat, pct=1/nrow(res.mat), cumpct=(1:nrow(res.mat))/nrow(res.mat))

join.idx        <- match(rownames(eqty.mat), hand.mat$hand)

comb.mat        <- cbind(hand.mat, eqty.mat[join.idx,])





##------------------------------------------------------------------
## examine the feature set of the top X% of hands
##------------------------------------------------------------------

## reorder columns
glob.rank   <- c("A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2")

## ranking of double-pairs
row.match   <- as.integer(na.omit(match(glob.rank, rownames(table(tmp$hiPair, tmp$loPair)))))
col.match   <- as.integer(na.omit(match(glob.rank, colnames(table(tmp$hiPair, tmp$loPair)))))
test        <- table(tmp$hiPair, tmp$loPair)[row.match, col.match]

##------------------------------------------------------------------
## [example] 30% range
##------------------------------------------------------------------


idx <- which(comb.mat[,c("10%")] >= 0.50)

plot(comb.mat$'5%')
bline(h=0.5, col="red")

table(a$suitCombo)
table(a$suitCombo, a$pair)
table(a$suitCombo, a$hiCard)

sum(a$wgt)
sum(a$pct)


## The goal here is to
##  1. Isolate ranges that perform favorably against different ranges
##  2. Identify (via tabular summaries) the *types* of hands that perform well against those ranges
##  3. Keep in mind that we've got hot/cold equity stats, so need to keep that in mind

## 4. Note the absence of rundowns from most of the profitable ranges






##
col.idx <- which(colnames(res.mat)=="30%")
row.idx <- which(res.mat[,col.idx] >= 0.50)

hand.idx    <- rownames(res.mat)[row.idx]

hand.attr   <- data.frame(  plo_6max.list[["hd"]][which( plo_6max.list[["hd"]]$hand %in% hand.idx),],
                            equity=res.mat[row.idx,col.idx] )



## next steps -- walk through an analysis for a range
## note that this is for hand shapes and not the total number of hands, so the % of shapes
## to play against a range will likely represent a larger fraction of total hands


