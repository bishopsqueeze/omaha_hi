
## hand ranking forms
## XYZW
## (XY)ZW
## X(YZ)W
## XY(ZW)
## (XYZ)W
## X(YZW)
## (XYZW)
## (XY)(ZW)


##     4     6     8
##  1820 11531  3081

## tl <- try(system("which java", intern=TRUE))

## which(!(tmp.ch %in% c("(",")")))
## tmp.ch[which(!(tmp.ch %in% c("(",")")))]
##/Applications/PPTOddsOracle.app/Contents/java/app/ui_jar


rm(list=ls())

setwd("/Users/alexstephens/Development/poker/omaha_hi/data")

## <function>
appendSuits <- function(h, s) {
    return(paste(paste(h,s,sep=""), collapse=""))
}

## <function>
processHandFile <- function(in.file) {

    tmp.file    <- unlist(strsplit(in.file, "[.]"))
    out.file    <- paste0(tmp.file[1], "_genericSyntax", ".", tmp.file[2])
    hf          <- read.csv(in.file, header=FALSE)
    ho          <- matrix(,nrow=nrow(hf), ncol=1)

    for (i in 1:nrow(hf)) {
        
        ## process a hand
        tmp.ch      <- unlist(strsplit(as.character(hf[i,]), ""))
        tmp.nch     <- length(tmp.ch)
        tmp.lp      <- which(tmp.ch %in% c("("))        ## location of left parentheses
        tmp.rp      <- which(tmp.ch %in% c(")"))        ## location of right parentheses
        tmp.ranks   <- tmp.ch[which(!(tmp.ch %in% c("(",")")))]
    
        ## double suited
        if (tmp.nch == 8) {
            tmp.suits   <- c("x","x","y","y")
        
        ## single suited
        } else if (tmp.nch == 6) {
        
            ## (XY)ZW [1,4]
            if ((tmp.lp == 1) & (tmp.rp == 4)) {
                tmp.suits <- c("x","x","y","z")
            
            ## X(YZ)W [2,5]
            } else if ((tmp.lp == 2) & (tmp.rp == 5)) {
                tmp.suits <- c("x","y","y","z")
            
            ## XY(ZW) [3,6]
            } else if ((tmp.lp == 3) & (tmp.rp == 6)) {
                tmp.suits <- c("x","y","z","z")
            
            ## (XYZ)W [1,5]
            } else if ((tmp.lp == 1) & (tmp.rp == 5)) {
                tmp.suits <- c("x","x","x","y")
            
            ## X(YZW) [2,6]
            } else if ((tmp.lp == 2) & (tmp.rp == 6)) {
                tmp.suits <- c("x","y","y","y")
            
            ## (XYZW) [1,6]
            } else if ((tmp.lp == 1) & (tmp.rp == 6)) {
                tmp.suits <- c("x","x","x","x")
            }
        
        ## rainbow
        } else if (tmp.nch == 4) {
            tmp.suits   <- c("x","y","z","w")
        
        ## error
        } else {
            stop("unexpected string lenght\n")
        }
    
    ## insert the hand in generic syntax to a matrix
    ho[i,1]  <- appendSuits(tmp.ranks, tmp.suits)
    
    }

    ## save the output as a file
    write.table(ho, file=out.file, row.names=FALSE, col.names=FALSE)
}



processHandFile("ppt_plo_handrankings_03handed.txt")
processHandFile("ppt_plo_handrankings_06handed.txt")
processHandFile("ppt_plo_handrankings_10handed.txt")


