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

    ## read the input file
    hf          <- read.csv(in.file, header=FALSE)
    
    ## create output matrices
    hand.shape  <- matrix(,nrow=nrow(hf), ncol=1)
    suit.shape  <- matrix(,nrow=nrow(hf), ncol=1)
    suit.combo  <- matrix(,nrow=nrow(hf), ncol=1)
    pair.shape  <- matrix(,nrow=nrow(hf), ncol=1)
    pair.ranks  <- matrix(,nrow=nrow(hf), ncol=2)
    high.card   <- matrix(,nrow=nrow(hf), ncol=1)
    high.suited <- matrix(,nrow=nrow(hf), ncol=1)
    rank.conn   <- matrix(,nrow=nrow(hf), ncol=1)
    rank.gaps   <- matrix(,nrow=nrow(hf), ncol=1)
    shape.type  <- matrix(,nrow=nrow(hf), ncol=1)
    shape.wgt   <- matrix(,nrow=nrow(hf), ncol=1)

    ## loop over each row in the file and process the ranked hand
    for (i in 1:nrow(hf)) {
        #for (i in 1:1147) {
        
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
  
        ##------------------------------------------------------------------
        ## Deconstruct suits and identify suit shapes
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
                tmp.color   <- c("ss.2o")
            ## X(YZ)W [2,5]
            } else if ((tmp.lp == 2) & (tmp.rp == 5)) {
                tmp.suits   <- c("x","y","y","z")
                tmp.color   <- c("ss.2o")
            ## XY(ZW) [3,6]
            } else if ((tmp.lp == 3) & (tmp.rp == 6)) {
                tmp.suits   <- c("x","y","z","z")
                tmp.color   <- c("ss.2o")
            ## (XYZ)W [1,5]
            } else if ((tmp.lp == 1) & (tmp.rp == 5)) {
                tmp.suits   <- c("x","x","x","y")
                tmp.color   <- c("ss.1o")
            ## X(YZW) [2,6]
            } else if ((tmp.lp == 2) & (tmp.rp == 6)) {
                tmp.suits   <- c("x","y","y","y")
                tmp.color   <- c("ss.1o")
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
        hand.shape[i,1]     <- appendSuits(tmp.ranks, tmp.suits)
        suit.shape[i,1]     <- tmp.color


        ##------------------------------------------------------------------
        ## Create homogeneous suit combos
        ##------------------------------------------------------------------
        tmp.combo   <- paste0(tmp.suits, collapse="")
        
        if (tmp.combo %in% c("xxxx")) {
            suit.combo[i,1] <- c("aaaa")
        } else if (tmp.combo %in% c("xxxy","xyyy")) {
            suit.combo[i,1] <- c("aaab")
        } else if (tmp.combo %in% c("xxyy")) {
            suit.combo[i,1] <- c("aabb")
        } else if (tmp.combo %in% c("xxyz","xyyz","xyzz")) {
            suit.combo[i,1] <- c("aabc")
        } else if (tmp.combo %in% c("xyzw")) {
            suit.combo[i,1] <- c("abcd")
        }


        ##------------------------------------------------------------------
        ## Identify pair types
        ##------------------------------------------------------------------
        if (max(tmp.tbl) == 4) {
            tmp.count   <- c("4k")
            tmp.hipair  <- glob.rank[min(which(glob.rank %in% names(tmp.tbl[tmp.tbl >= 2])))]
            tmp.lopair  <- NA
        } else if (max(tmp.tbl) == 3) {
            tmp.count   <- c("3k")
            tmp.hipair  <- glob.rank[min(which(glob.rank %in% names(tmp.tbl[tmp.tbl >= 2])))]
            tmp.lopair  <- NA
        } else if (max(tmp.tbl == 2)) {
            if (tmp.unq == 2) {
                tmp.count   <- c("2p")
                tmp.hipair  <- glob.rank[min(which(glob.rank %in% names(tmp.tbl[tmp.tbl >= 2])))]
                tmp.lopair  <- glob.rank[max(which(glob.rank %in% names(tmp.tbl[tmp.tbl >= 2])))]
            } else {
                tmp.count   <- c("1p")
                tmp.hipair  <- glob.rank[min(which(glob.rank %in% names(tmp.tbl[tmp.tbl >= 2])))]
                tmp.lopair  <- NA
            }
        } else {
            tmp.count   <- c("0p")
            tmp.hipair  <- NA
            tmp.lopair  <- NA
        }
        pair.shape[i,1]     <- tmp.count
        pair.ranks[i,]      <- c(tmp.hipair, tmp.lopair)
 
        ##------------------------------------------------------------------
        ## Identify shape types
        ##------------------------------------------------------------------
        if (pair.shape[i,1] == "4k") {
            ## XaXbXcXd	4K::abcd
            tmp.shape   <- "XaXbXcXd"
            tmp.wgt     <- (13/13)
        } else if (pair.shape[i,1] == "3k") {
            ## XaXbXcYa	3K::aabc
            if (suit.combo[i,1] == "aabc") {
                tmp.shape   <- "XaXbXcYa"
                tmp.wgt     <- (1872/156)
            ## XaXbXcYd	3K::abcd
            } else if (suit.combo[i,1] == "abcd") {
                tmp.shape   <- "XaXbXcYd"
                tmp.wgt     <- (624/156)
            }
        } else if (pair.shape[i,1] == "2p") {
            ## XaXbYaYb	2P::aabb
            if (suit.combo[i,1] == "aabb") {
                tmp.shape   <- "XaXbYaYb"
                tmp.wgt     <- (468/78)
            ## XaXbYaYc	2P::aabc
            } else if (suit.combo[i,1] == "aabc") {
                tmp.shape   <- "XaXbYaYc"
                tmp.wgt     <- (1872/78)
            ## XaXbYcYd	2P::abcd
            } else if (suit.combo[i,1] == "abcd") {
                tmp.shape   <- "XaXbYcYd"
                tmp.wgt     <- (468/78)
            }
        } else if (pair.shape[i,1] == "1p") {
            ## XaXbYaZa	1P::aaab
            if (suit.combo[i,1] == "aaab") {
                tmp.shape   <- "XaXbYaZa"
                tmp.wgt     <- (10296/858)
            ## XaXbYaZb	1P::aabb
            } else if (suit.combo[i,1] == "aabb") {
                tmp.shape   <- "XaXbYaZb"
                tmp.wgt     <- (10296/858)
            ## XaXbYaZc	1P::aabc
            } else if (suit.combo[i,1] == "aabc") {
                tmp.shape   <- "XaXbYaZc"
                tmp.wgt     <- (41184+10296)/(1716+858)
            ## XaXbYcZc	1P::aabc
            #} else if (suit.combo[i,1] == "ss.2o") {
            #    tmp.shape   <- "XaXbYaZc"
            ## XaXbYcZd	1P::abcd
            } else if (suit.combo[i,1] == "abcd") {
                tmp.shape   <- "XaXbYcZd"
                tmp.wgt     <- (10296/858)
            }
            
        } else if (pair.shape[i,1] == "0p") {

            ## XaYaZaRa	NP::aaaa
            if (suit.combo[i,1] == "aaaa") {
                tmp.shape   <- "XaYaZaRa"
                tmp.wgt     <- (2860/715)
            ## XaYaZaRb	NP::aaab
            } else if (suit.combo[i,1] == "aaab") {
                tmp.shape   <- "XaYaZaRb"
                tmp.wgt     <- (34320/2860)
            ## XaYaZbRb	NP::aabb
            } else if (suit.combo[i,1] == "aabb") {
                tmp.shape   <- "XaYaZbRb"
                tmp.wgt     <- (25740/2145)
            ## XaYaZbRc	NP::aabc
            } else if (suit.combo[i,1] == "aabc") {
                tmp.shape   <- "XaYaZbRc"
                tmp.wgt     <- (102960/4290)
            ## XaYbZcRd	NP::abcd
            } else if (suit.combo[i,1] == "abcd") {
                tmp.shape   <- "XaYbZcRd"
                tmp.wgt     <- (17160/715)
            }

        } else {
            tmp.shape <- "err"
        }
        shape.type[i,]  <- tmp.shape
        shape.wgt[i,]   <- tmp.wgt
        

        ##------------------------------------------------------------------
        ## Identify high card
        ##------------------------------------------------------------------
        high.card[i,]  <- glob.rank[which(glob.num == max(tmp.nums))]


        ##------------------------------------------------------------------
        ## Identify high card
        ##------------------------------------------------------------------
        ## get suits tied to the high card
        hi.suits        <- tmp.suits[which(tmp.ranks %in% high.card[i,])]
        non.hi.suits    <- tmp.suits[which(!(tmp.ranks %in% high.card[i,]))]
        if (length(which(hi.suits %in% non.hi.suits)) > 0) {
            high.suited[i,] <- "Y"
        } else {
            high.suited[i,] <- "N"
        }
  
  
        ##------------------------------------------------------------------
        ## Identify connectedness
        ##------------------------------------------------------------------
        
        ## pure rundown variants
        if (all(tmp.gaps == 1)) {
            tmp.conn    <- "rd.0g"
        } else if ( all(tmp.gaps == 2)) {
            tmp.conn    <- "rd.1g"
        } else if (all(tmp.gaps == 3)) {
            tmp.conn    <- "rd.2g"
            
        ## connected 2-pair
        } else if ( (tmp.gaps[1] == 0) & (tmp.gaps[3] == 0) & (tmp.gaps[2] %in% c(1,2,3)) ) {
            tmp.conn    <- "2p.conn"
    
        ## rundown plus pair
        } else if ( (sum(tmp.gaps==0)==1) & (sum(tmp.gaps==1)==2) ) {
            tmp.conn    <- "rd.1p"
            
        ## single gap rundown
        } else if ( (sum(tmp.gaps==2)==1) & (sum(tmp.gaps==1)==2) ) {
            tmp.conn    <- "rd.1g"

        ## double gap rundown
        } else if ( (sum(tmp.gaps==3)==1) & (sum(tmp.gaps==1)==2) ) {
            tmp.conn    <- "rd.2g"

        ## triple gap rundown
        } else if ( (sum(tmp.gaps==4)==1) & (sum(tmp.gaps==1)==2) ) {
            tmp.conn    <- "rd.3g"

        ## dangler gap rundown
        } else if ( (sum(tmp.gaps>4)==1) & (sum(tmp.gaps==1)==2) ) {
            tmp.conn    <- "rd.dg"

        ## 2x single gap rundown
        } else if ( (sum(tmp.gaps==1)==1) & (sum(tmp.gaps==2)==2) ) {
            tmp.conn    <- "rd.2x1g"

        ## 2x double gap rundown
        } else if ( (sum(tmp.gaps==1)==1) & (sum(tmp.gaps==3)==2) ) {
            tmp.conn    <- "rd.2x2g"
  
        ## connected single-pair
        } else if ( (sum(tmp.gaps==0)==1) & (sum(tmp.gaps==1)==1) & (sum(tmp.gaps==2)==1) ) {
            tmp.conn    <- "1p.conn"
 
        ## default
        } else {
            tmp.conn    <- NA
        }
        rank.conn[i,]   <- tmp.conn


        ##------------------------------------------------------------------
        ## Consolidate gap data
        ##------------------------------------------------------------------
        rank.gaps[i,]   <- paste(tmp.gaps, collapse=":")

    }


    ## consolidate
    hand.data   <- data.frame(  hand=hand.shape,
                                suit=suit.shape,
                                suitCombo=suit.combo,
                                shapeType=shape.type,
                                shapeWgt=shape.wgt,
                                pair=pair.shape,
                                ranks=pair.ranks,
                                highCard=high.card,
                                highSuited=high.suited,
                                conn=rank.conn,
                                gaps=rank.gaps)

    colnames(hand.data) <- c("hand","suit","suitCombo","shapeType","shapeWgt","pair","hiPair","loPair","hiCard","hiSuited","conn","gaps")
    
    ## return the results
    return(list(ho=hand.shape, hd=hand.data))
}


##------------------------------------------------------------------
## <main> process hands
##------------------------------------------------------------------

plo_3max.list   <- processHandFile("ppt_plo_handrankings_03handed.txt")
save(plo_3max.list, file="ppt_plo_handrankings_03handed.Rdata")

plo_6max.list   <- processHandFile("ppt_plo_handrankings_06handed.txt")
save(plo_6max.list, file="ppt_plo_handrankings_06handed.Rdata")

plo_10max.list  <- processHandFile("ppt_plo_handrankings_10handed.txt")
save(plo_10max.list, file="ppt_plo_handrankings_10handed.Rdata")

plo_random.list <- processHandFile("ppt_plo_handrankings_preflop_vs_random.txt")
save(plo_random.list, file="ppt_plo_handrankings_preflop_vs_random.Rdata")


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

