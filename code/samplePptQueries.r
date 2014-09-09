# Usage - RunPQL [-options] PQLQuery(ies)
# Separate multiple queries with a semicolon
# Options include:
# -mt max-trials   (default  600000)
# -ms max-seconds  (default 10)
# -tc thread-count (default 8)
# -c              (causes query to be read from stdin)
# Examples:
# RunPQL "select avg(riverEquity(p1)) from game='holdem', p1='AK', p2='JT'"
# RunPQL -mt 100000 -ms 100 -tc 1 'select ....'
# RunPQL -c
# RunPQL 'select avg(6) from game='holdem'; select avg(1) from game='omahahi'

rm(list=ls())

## read the hand file
setwd("/Users/alexstephens/Development/poker/omaha_hi/data")
hf.6max <- read.table("ppt_plo_handrankings_06handed_genericSyntax.txt", header=FALSE)


## change to the PPT directory
setwd("/Applications/PPTOddsOracle.app/Contents/java/app/ui_jar")

## define the command-line interface (cli) prefix
nsim <- 1000000
tlen <- 100
ncpu <- 4
cli <- sprintf("java -cp p2.jar propokertools.cli.RunPQL -mt %d -ms %d -tc %d", nsim, tlen, ncpu)


## PLO simulation set-up
pct.vec <- c(seq(5, 40, 5), seq(50, 100, 25))
res.vec <- vector(,length=length(pct.vec))
res.mat <- matrix(,nrow=dim(hf.6max)[1], ncol=length(res.vec))

for (j in 1:2) {
    
    tmp.h   <- as.character(droplevels(hf.6max[j, 1]))
    
    for (i in 1:length(pct.vec)) {
    
        tmp.pct <- paste0(pct.vec[i],"%")


        ## construct a hot/cold equity simulation
        qry <- sprintf("\"select avg(riverEquity(PLAYER_1)) as P1_EQUITY from game='omahahi', syntax='Generic', PLAYER_1='%s', PLAYER_2='%s'\"", tmp.h, tmp.pct)
    
        cli.qry         <- paste(cli, qry, sep=" ")

        tmp.res         <- try(system(cli.qry, intern = TRUE))

        tmp.p1_equity   <- as.numeric(unlist(strsplit(tmp.res, "="))[2])
        res.vec[i]      <- tmp.p1_equity

    }
    
    res.mat[j, ] <- res.vec
    
    cat("j iteration = ", j, "\n")
}

colnames(res.mat) <- paste0(pct.vec, "%")
rownames(res.mat) <- as.character(hf.6max[,1])



## save the results
save.image(file="/Users/alexstephens/Development/poker/omaha_hi/data/hotColdEquity_6max.Rdata")



