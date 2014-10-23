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
setwd("/Users/alexstephens/Development/poker/omaha_hi/data/pptoo/OmahaHi")

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
## <function> rankToNum
##------------------------------------------------------------------
## Given a hand (e.g., c("A","J","A","J")), translate the ranks into
## numerical equivalents (e.g., A=13, K=12, ... 3=2, 2=1)
##------------------------------------------------------------------
rankToNum <- function(n, r, hr) {
    n[match(hr, r)]
    return(sort(n[match(hr, r)], decreasing=TRUE))
}


##------------------------------------------------------------------
## <function> processHandFile
##------------------------------------------------------------------
## Takes a PPT Omaha hand file as an input and writes the translated
## format to a file.  The output filename contains the "_genericSyntax"
## postfix.
##------------------------------------------------------------------
processHandFile <- function(in.file) {

    ## "global" rank/number vectors
    glob.num     <- seq(13, 1, -1)
    glob.rank    <- c("A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2")
    
    ## process the filename
    tmp.file    <- unlist(strsplit(in.file, "[.]"))
    out.file    <- paste0(tmp.file[1], "_genericSyntax_DEBUG", ".", tmp.file[2])

    ## read the input file and create an identically-sized output matrix
    hf          <- read.csv(in.file, header=FALSE)
    ho          <- matrix(,nrow=nrow(hf), ncol=1)
    hs          <- matrix(,nrow=nrow(hf), ncol=2)
    hg          <- matrix(,nrow=nrow(hf), ncol=1)

    ## loop over each row in the file and process the ranked hand
    for (i in 1:nrow(hf)) {
        
        ## process a hand
        tmp.ch      <- unlist(strsplit(as.character(hf[i,]), ""))   ## split hand into individual characters
        tmp.nch     <- length(tmp.ch)                               ## count number of characters
        tmp.lp      <- which(tmp.ch %in% c("("))                    ## location of left parentheses
        tmp.rp      <- which(tmp.ch %in% c(")"))                    ## location of right parentheses
        tmp.ranks   <- tmp.ch[which(!(tmp.ch %in% c("(",")")))]     ## extract ranks
        
        ## get hand shape infp
        tmp.nums    <- rankToNum(glob.num, glob.rank, tmp.ranks)    ## convert ranks to (sorted) numbers
        tmp.gaps    <- abs(diff(tmp.nums))                          ## calculate gap sizes
        tmp.unq     <- length(unique(tmp.nums))                     ## number of unique
        tmp.tbl     <- table(tmp.ranks)
        
        ##------------------------------------------------------------------
        ## the if/then processes the hand based on the length, and
        ## for single-suited suited hands it will use the location
        ## of the right and left parentheses to assign the suits
        ##------------------------------------------------------------------
  
        ## double suited
        if (tmp.nch == 8) {
            
            tmp.suits   <- c("x","x","y","y")
            tmp.color   <- c("ds")
        
        ## single suited
        } else if (tmp.nch == 6) {
        
            ## (XY)ZW [1,4]
            if ((tmp.lp == 1) & (tmp.rp == 4)) {
                tmp.suits   <- c("x","x","y","z")
                tmp.color   <- c("ss")
            
            ## X(YZ)W [2,5]
            } else if ((tmp.lp == 2) & (tmp.rp == 5)) {
                tmp.suits   <- c("x","y","y","z")
                tmp.color   <- c("ss")
            
            ## XY(ZW) [3,6]
            } else if ((tmp.lp == 3) & (tmp.rp == 6)) {
                tmp.suits   <- c("x","y","z","z")
                tmp.color   <- c("ss")
            
            ## (XYZ)W [1,5]
            } else if ((tmp.lp == 1) & (tmp.rp == 5)) {
                tmp.suits   <- c("x","x","x","y")
                tmp.color   <- c("ss")
            
            ## X(YZW) [2,6]
            } else if ((tmp.lp == 2) & (tmp.rp == 6)) {
                tmp.suits   <- c("x","y","y","y")
                tmp.color   <- c("ss")
            
            ## (XYZW) [1,6]
            } else if ((tmp.lp == 1) & (tmp.rp == 6)) {
                tmp.suits   <- c("x","x","x","x")
                tmp.color   <- c("mo")
            }
        
        ## rainbow
        } else if (tmp.nch == 4) {
            tmp.suits   <- c("x","y","z","w")
            tmp.color   <- c("rb")
        
        ## error
        } else {
            stop("unexpected string lenght\n")
        }
    
        ##------------------------------------------------------------------
        ## characterize the number of duplicate ranks
        ##------------------------------------------------------------------
        if (max(tmp.tbl) == 4) {
            tmp.count   <- c("qu")
        } else if (max(tmp.tbl) == 3) {
            tmp.count   <- c("tr")
        } else if (max(tmp.tbl == 2)) {
            if (tmp.unq == 2) {
                tmp.count   <- c("dp")
            } else {
                tmp.count   <- c("sp")
            }
        } else {
            
            ##
            if ( all(tmp.gaps <= 4) &  all(tmp.gaps >= 0) ) {
                
                
## move the logic for pairs/trips/quads into this segment (under the 0 gap area)
## ... or maybe just have a column for pair/trip quads, no pair and another
## column for rundown categories (vs. danglers)
## and somehow incorporate pair plus connected
                
                ## rundown
                if (all(tmp.gaps == 1)) {
                    tmp.count <- c("rd")
                    
                ## connected pairs
                } else if ( tmp.gaps[1] == 0 & all(tmp.gaps[c(2,3)] %in% c(1,2))) {
                    tmp.count <- c("sp+c")
                } else if ( tmp.gaps[2] == 0 & all(tmp.gaps[c(1,3)] %in% c(1,2))) {
                    tmp.count <- c("sp+c")
                } else if ( tmp.gaps[3] == 0 & all(tmp.gaps[c(1,2)] %in% c(1,2))) {
                    tmp.count <- c("sp+c")
                    
                ## single 1-gappers
                } else if ( tmp.gaps[1] == 2 & all(tmp.gaps[c(2,3)] == 1)) {
                    tmp.count <- c("t1g")
                } else if ( tmp.gaps[2] == 2 & all(tmp.gaps[c(1,3)] == 1)) {
                    tmp.count <- c("m1g")
                } else if ( tmp.gaps[3] == 2 & all(tmp.gaps[c(1,2)] == 1)) {
                    tmp.count <- c("b1g")
                ## double 1-gappers
                } else if ( all(tmp.gaps[c(1,2)] == 2) & tmp.gaps[3] == 1) {
                    tmp.count <- c("tm1g")
                } else if ( all(tmp.gaps[c(2,3)] == 2) & tmp.gaps[1] == 1) {
                    tmp.count <- c("mb1g")
                } else if ( all(tmp.gaps[c(1,3)] == 2) & tmp.gaps[2] == 1) {
                    tmp.count <- c("tb1g")
                ## triple 1-gapper
                } else if ( all(tmp.gaps == 2) ) {
                    tmp.count <- c("tmb1g")
                    
                ## single 2-gappers
                } else if ( tmp.gaps[1] == 3 & all(tmp.gaps[c(2,3)] == 1)) {
                    tmp.count <- c("t2g")
                } else if ( tmp.gaps[2] == 3 & all(tmp.gaps[c(1,3)] == 1)) {
                    tmp.count <- c("m2g")
                } else if ( tmp.gaps[3] == 3 & all(tmp.gaps[c(1,2)] == 1)) {
                    tmp.count <- c("b2g")
                ## double 2-gappers
                } else if ( tmp.gaps[1] == 3 & all(tmp.gaps[c(2,3)] == 1)) {
                    tmp.count <- c("t2g")
                } else if ( tmp.gaps[2] == 3 & all(tmp.gaps[c(1,3)] == 1)) {
                    tmp.count <- c("m2g")
                } else if ( tmp.gaps[3] == 3 & all(tmp.gaps[c(1,2)] == 1)) {
                    tmp.count <- c("b2g")
                ## triple 2-gapper
                } else if ( all(tmp.gaps == 3) ) {
                    tmp.count <- c("tmb2g")
                    
                ## single 3-gappers
                } else if ( tmp.gaps[1] == 4 & all(tmp.gaps[c(2,3)] == 1)) {
                    tmp.count <- c("t3g")
                } else if ( tmp.gaps[2] == 4 & all(tmp.gaps[c(1,3)] == 1)) {
                    tmp.count <- c("m3g")
                } else if ( tmp.gaps[3] == 4 & all(tmp.gaps[c(1,2)] == 1)) {
                    tmp.count <- c("b3g")
                ## double 3-gappers
                } else if ( tmp.gaps[1] == 4 & all(tmp.gaps[c(2,3)] == 1)) {
                    tmp.count <- c("t3g")
                } else if ( tmp.gaps[2] == 4 & all(tmp.gaps[c(1,3)] == 1)) {
                    tmp.count <- c("m3g")
                } else if ( tmp.gaps[3] == 4 & all(tmp.gaps[c(1,2)] == 1)) {
                    tmp.count <- c("b3g")
                ## triple 3-gapper
                } else if ( all(tmp.gaps == 4) ) {
                    tmp.count <- c("tmb3g")
                    
                ## backstop
                } else {
                    tmp.count <- c("np")
                }
             
            } else {
                tmp.count   <- c("np")
            }
            
        }


    
    
    ## insert the hand translated into generic syntax to a matrix
    ho[i,1]     <- appendSuits(tmp.ranks, tmp.suits)
    hs[i,]      <- c(tmp.color, tmp.count)
    hg[i,1]     <- paste0(tmp.gaps, collapse=":")
    }

    ## save the output as a file
    #write.table(ho, file=out.file, row.names=FALSE, col.names=FALSE)
    return(list(hf=hf, ho=ho, hs=hs, hg=hg))
}



##------------------------------------------------------------------
## <main> process hands
##------------------------------------------------------------------

##processHandFile("ppt_plo_handrankings_03handed.txt")

plo_6max.list   <- processHandFile("ppt_plo_handrankings_06handed.txt")
save(plo_6max.list, file="ppt_plo_handrankings_06handed.Rdata")

##processHandFile("ppt_plo_handrankings_10handed.txt")
##processHandFile("ppt_plo_handrankings_preflop_vs_random.txt")


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

