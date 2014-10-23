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
## [example] 30% range
##------------------------------------------------------------------

##
col.idx <- which(colnames(res.mat)=="30%")
row.idx <- which(res.mat[,col.idx] >= 0.50)

hand.idx    <- rownames(res.mat)[row.idx]

hand.attr   <- data.frame(  plo_6max.list[["hd"]][which( plo_6max.list[["hd"]]$hand %in% hand.idx),],
                            equity=res.mat[row.idx,col.idx] )



## next steps -- walk through an analysis for a range
## note that this is for hand shapes and not the total number of hands, so the % of shapes
## to play against a range will likely represent a larger fraction of total hands


