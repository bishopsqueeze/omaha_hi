##------------------------------------------------------------------
## The purpose of this script is to step through the entire list
## of unique Omaha Hi starting hand combinations and to make a
## system call to the ProPokerTools OddsOracle (PPTOO).  For each
## hand the script runs hot/cold equity calculations for a series of
## heads-up opponent hand ranges.  Specifically:
## {5%,10%,15%,20%,25%,30%,35%,40%,50%,75%,100%}
##------------------------------------------------------------------

##------------------------------------------------------------------
## The following presents an example command-line call to the
##------------------------------------------------------------------
## Usage - RunPQL [-options] PQLQuery(ies)
##
## Separate multiple queries with a semicolon
##
## Options include:
## -mt max-trials   (default  600000)
## -ms max-seconds  (default 10)
## -tc thread-count (default 8)
## -c              (causes query to be read from stdin)
## Examples:
## RunPQL "select avg(riverEquity(p1)) from game='holdem', p1='AK', p2='JT'"
## RunPQL -mt 100000 -ms 100 -tc 1 'select ....'
## RunPQL -c
## RunPQL 'select avg(6) from game='holdem'; select avg(1) from game='omahahi'
##------------------------------------------------------------------

##------------------------------------------------------------------
## Clear the workspace
##------------------------------------------------------------------
rm(list=ls())

##------------------------------------------------------------------
## Set the PPT data directory
##------------------------------------------------------------------
setwd("/Users/alexstephens/Development/poker/omaha_hi/data")

##------------------------------------------------------------------
## Read an PPT hand file
##------------------------------------------------------------------
hf.6max <- read.table("ppt_plo_handrankings_06handed_genericSyntax.txt", header=FALSE)

##------------------------------------------------------------------
## Change to the directory that holds the PPT OO java executables
##------------------------------------------------------------------
setwd("/Applications/PPTOddsOracle.app/Contents/java/app/ui_jar")

##------------------------------------------------------------------
## Define the command-line interface (cli) paramters
##------------------------------------------------------------------
nsim <- 1000000
tlen <- 100
ncpu <- 1
cli <- sprintf("java -cp p2.jar propokertools.cli.RunPQL -mt %d -ms %d -tc %d", nsim, tlen, ncpu)

##------------------------------------------------------------------
## PLO simulation set-up
##------------------------------------------------------------------
pct.vec <- c(seq(5, 40, 5), seq(50, 100, 25))
res.vec <- vector(,length=length(pct.vec))
res.mat <- matrix(,nrow=dim(hf.6max)[1], ncol=length(res.vec))

##------------------------------------------------------------------
## Loop over each hand/villain combo and run the cli
##------------------------------------------------------------------

## loop over hands
for (j in 1:dim(hf.6max)[1]) {
    
    ## grab the hand
    tmp.h   <- as.character(droplevels(hf.6max[j, 1]))
    
    ## loop over villain ranges
    for (i in 1:length(pct.vec)) {
    
        ## the villain percentage
        tmp.pct <- paste0(pct.vec[i],"%")

        ## construct a hot/cold equity simulation
        qry <- sprintf("\"select avg(riverEquity(PLAYER_1)) as P1_EQUITY from game='omahahi', syntax='Generic', PLAYER_1='%s', PLAYER_2='%s'\"", tmp.h, tmp.pct)
    
        ## concatenate the command with the query
        cli.qry         <- paste(cli, qry, sep=" ")

        ## execute the query
        tmp.res         <- try(system(cli.qry, intern = TRUE))

        ## extract results
        tmp.p1_equity   <- as.numeric(unlist(strsplit(tmp.res, "="))[2])
        res.vec[i]      <- tmp.p1_equity
    }
    
    ## load a matrix that holds the results
    res.mat[j, ] <- res.vec
    cat("j iteration = ", j, "\n")
}
colnames(res.mat) <- paste0(pct.vec, "%")
rownames(res.mat) <- as.character(hf.6max[,1])


##------------------------------------------------------------------
## Save the results
##------------------------------------------------------------------
##save.image(file="/Users/alexstephens/Development/poker/omaha_hi/data/hotColdEquity_6max_v2.Rdata")



