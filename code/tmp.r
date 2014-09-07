
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





## Range categories


## double-suited Ace-high broadways
(A[K-T][K-T][K-T]:xxyy)
## strong double-suited high-card hands
([K-9][K-9][K-9][K-9]:xxyy)
## double-suited big pairs
KK, QQ:xxyy
## unpaired double-suited rundowns
([Q-7][Q-7][Q-7][Q-7]:xxyy!RR)
## double-suited middle-pairs
(JJ-77:JJ-77):xxyy!RRR

## single-suited Ace-hig broadways
(A[K-T][K-T][K-T]:xx!xxyy)
## rainbow ace-high broadways
(A[K-T][K-T][K-T]:!xx)
## strong single-suited high-card hands
([K-9][K-9][K-9][K-9]:xx)
## single-suited big pairs
KK, QQ:xx!xxyy
## rainbow big pairs
KK, QQ!xx
## unpaired single-suited rundowns
([Q-7][Q-7][Q-7][Q-7]:xx!RR)
## single-suited double middle-paurs
(JJ-77:JJ-77):xx!RRR
## rainbow double middle-pairs
(JJ-77:JJ-77)!(xx, RRR)

## villain ranges

## Case 1 (10% ; 14300 hands)
AA, KK, ([T+][T+][T+][T+]):xxyy

## Case 2 (24728 hands)
AA, KK, ([T+][T+][T+][T+]):xxyy,
(Ax*x*y*y),
([K-8][K-8][K-8][K-8]):xxyy,
RROO:xxyy


## Case 3 (33068 hands)
AA, KK, ([T+][T+][T+][T+]):xxyy,
(Ax*x*y*y),
([K-8][K-8][K-8][K-8]):xxyy,
RROO:xxyy,
([Q-7][Q-7][Q-7][Q-7]):xx!RRR

## Case 4 25% (67676 hands)
25%

## Case 5 (79377 hands)
AA, KK, QQ, JJ:xx,
xxyy,
RROO,
([T+][T+][T+][T+]),
([8+][8+][8+][8+]):xx,
AKQJ-,
15%,
Ax*x:([T+][T+][T+])

## Case 6 (108289 hands)
40%





## Potential 3-bet range

AA,
KK:xx,
QQ:xx,
KK:([A-9][A-9])!RRR,
KK:([8-6][8-6]),
QQ:([A-9][A-9])!RRR,
KKRR,
QQRR,
A[T+][T+][T+]!RRR,
[K-9][K-9][K-9][K-9]:xx!RRR,
[7+][7+][7+][7+]:RROO:xx,
[2-9][2-9][2-9][2-9]:RROO,
A[K-T][K-T][9-2]:Ax*x,
[K-9][K-9][K-9][8-2]:xxyy!RRR,
9T+:56+:xxyy,
J9+:56+:xxyy,
[7-Q][7-Q][7-Q][7-Q]!RRR,
7654+:xxyy,
8764+:xxyy,
8754+:xxyy,
8654+:xxyy,
9864+:xxyy




AA,
KK:xx,
QQ:xx,
KK:([A-9][A-9])!RRR,
QQ:([A-9][A-9])!RRR,
KKRR,
QQRR,
A[T+][T+][T+]!RRR,
[K-9][K-9][K-9][K-9]:xx!RRR,
[7+][7+][7+][7+]:RROO:xx,
A[K-T][K-T][9-2]:Ax*x,
[K-9][K-9][K-9][8-2]:xxyy!RRR,
8765+:xxyy,
9875+:xxyy,
9865+:xxyy



## Example
select /* Start equity stats */
avg(riverEquity(PLAYER_1)) as PLAYER_1_equity1,
avg(riverEquity(PLAYER_2)) as PLAYER_2_equity1,
avg(riverEquity(PLAYER_3)) as PLAYER_3_equity1
/* End equity stats */
from game='omahahi', syntax='Generic',
PLAYER_1='100%',
PLAYER_2='100%',
PLAYER_3='10%'



