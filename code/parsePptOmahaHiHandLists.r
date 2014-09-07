##------------------------------------------------------------------
## The purpose of this script is to parse the Pro Poker Tools (PPT)
## hand ranking file 
##------------------------------------------------------------------

##------------------------------------------------------------------
## Clear the workspace
##------------------------------------------------------------------
rm(list=ls())

##------------------------------------------------------------------
## Set the working directory
##------------------------------------------------------------------
setwd("/Users/alexstephens/Development/poker/omaha_hi/data")

##------------------------------------------------------------------
## The PPT hands Omaha hands have the follow structures
##------------------------------------------------------------------
## XYZW
## (XY)ZW
## X(YZ)W
## XY(ZW)
## (XYZ)W
## X(YZW)
## (XYZW)
## (XY)(ZW)
##------------------------------------------------------------------

##------------------------------------------------------------------
## The goal is to convert these into the PPT generic syntax format
##------------------------------------------------------------------
## E.g., translate (AJ)(AJ) --> AxJxAyJy (double-suited double-pair)
##------------------------------------------------------------------


##------------------------------------------------------------------
## <function> appendSuits
##------------------------------------------------------------------
## Takes a hand character vector, e.g., c("A","J","A","J") and
## appends a suit character vector, e.g., c("x", "x", "y", "y")
##------------------------------------------------------------------
appendSuits <- function(h, s) {
    return(paste(paste(h,s,sep=""), collapse=""))
}

##------------------------------------------------------------------
## <function> processHandFile
##------------------------------------------------------------------
## Takes a PPT Omaha hand file as an input and writes the translated
## format to a file.  The output filename contains the "_genericSyntax"
## postfix.
##------------------------------------------------------------------
processHandFile <- function(in.file) {

    ## process the filename
    tmp.file    <- unlist(strsplit(in.file, "[.]"))
    out.file    <- paste0(tmp.file[1], "_genericSyntax", ".", tmp.file[2])

    ## read the input file and create an identically-sized output matrix
    hf          <- read.csv(in.file, header=FALSE)
    ho          <- matrix(,nrow=nrow(hf), ncol=1)

    ## loop over each row in the file and process the ranked hand
    for (i in 1:nrow(hf)) {
        
        ## process a hand
        tmp.ch      <- unlist(strsplit(as.character(hf[i,]), ""))   ## split hand into individual characters
        tmp.nch     <- length(tmp.ch)                               ## count number of characters
        tmp.lp      <- which(tmp.ch %in% c("("))                    ## location of left parentheses
        tmp.rp      <- which(tmp.ch %in% c(")"))                    ## location of right parentheses
        tmp.ranks   <- tmp.ch[which(!(tmp.ch %in% c("(",")")))]     ## extract ranks
    
    
        ## the if/then processes the hand based on the length, and
        ## for single-suited suited hands it will use the location
        ## of the right and left parentheses to assign the suits
        
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
    
    ## insert the hand translated into generic syntax to a matrix
    ho[i,1]  <- appendSuits(tmp.ranks, tmp.suits)
    
    }

    ## save the output as a file
    write.table(ho, file=out.file, row.names=FALSE, col.names=FALSE)
}



##------------------------------------------------------------------
## <main> process hands
##------------------------------------------------------------------
processHandFile("ppt_plo_handrankings_03handed.txt")
processHandFile("ppt_plo_handrankings_06handed.txt")
processHandFile("ppt_plo_handrankings_10handed.txt")
processHandFile("ppt_plo_handrankings_preflop_vs_random.txt")

##------------------------------------------------------------------
## Before
##------------------------------------------------------------------
## qp:data alexstephens$ head ppt_plo_handrankings_preflop_vs_random.txt
## (AT)(AT)
## (AJ)(AJ)
## (AQ)(AQ)
##------------------------------------------------------------------
## After
##------------------------------------------------------------------
## qp:data alexstephens$ head ppt_plo_handrankings_preflop_vs_random_genericSyntax.txt
## "AxTxAyTy"
## "AxJxAyJy"
## "AxQxAyQy"

