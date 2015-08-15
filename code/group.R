# MOMOpack for R version 0.1

# Originally MOMOpack V 4.3 for Stata, 
# created by Bernadette Gergonne, SSI-EpiLife for Euro MOMO. 
# Ported into R by Theodore Lytras <thlytras@gmail.com>


# DEFINITION OF the GROUP
# other age group or any kind of group can be defined with the same method
# and can overlap if needed.

mfile$GRPTotal <- mfile$age >= 0 | is.na(mfile$age)
mfile$GRP0to4 <-  mfile$age >= 0 & mfile$age <=4
mfile$GRP5to14 <- mfile$age >= 5 & mfile$age <=14
mfile$GRP15to64 <- mfile$age >= 15 & mfile$age <=64
mfile$GRP65P <- mfile$age >= 65 | is.na(mfile$age)

if (glb$DEBUG) write.dta(mfile, sprintf("%s/tempGRP.dta", glb$WDIR))


# *** CHOICE of the Model by group  ***
# Place below the group to run according to the appropriate model to run
# Do no change the choice by default for EuroMOMO age groups, 
# unless you can justify your choice.

# At least one and only one group must be called "Total"
# if you do not have age groups or other population subgroups but only one single group to run, 
# please name it "Total" or the Append.R may not run properly. 

# GROUP1: linear straight trend, no seasonality
glb$LINE <- list("0to4", "5to14")

# GROUP2: spline trend, no seasonality
glb$SPLINE <- list()

# GROUP3: linear-straight trend, seasonality
glb$LINE_SIN <- list("15to64", "65P", "Total")

# GROUP4: spline trend, seasonality
glb$SPLINE_SIN <- list()



# ******** DO NOT MODIFY BELOW THIS LINE ********

# Placeholder for the datasets that will be merged in Append.R
toBeMerged <- list(euromomoComplete = list(), euromomoRestricted = list(), table = list(), 
    cumChoice = list(), cumWinter = list(), cumSummer = list(), cumYear = list(), cumSeason = list())

# Iterating over the age groups
cat("Iterating over the age groups:\n")

mfile$nb[is.na(mfile$nb)] <- 0

for (N in with(glb, c(LINE, SPLINE, LINE_SIN, SPLINE_SIN))) {
  t2 <- system.time({ # benchmarking
    cat("- Iterating over group ");cat(N);cat("... ")
    glb$GROUP <- N
    
    # Little trick to conserve memory: If group is Total, do NOT make a useless identical copy of the entire mortality data file.
    if (sum(mfile[,paste("GRP", N, sep="")], na.rm=TRUE) == nrow(mfile)) {
      groupfile <- mfile
    } else {
      groupfile <- mfile[mfile[,paste("GRP", N, sep="")] & !is.na(mfile[,paste("GRP", N, sep="")]),]
    }

    # This is now executed just before the loop (for the whole of 'mfile', so that if group is Total, we avoid creating a useless identical copy of 'mfile' in 'groupfile').
    #groupfile$nb[is.na(groupfile$nb)] <- 0

    if (glb$DEBUG) write.dta(groupfile, sprintf("%s/group.dta", glb$WDIR))
    cat("(aggregation) "); source(sprintf("%s/aggregation.R", glb$CODEDIR))
    #source(sprintf("%s/directories.R", glb$CODEDIR)) # This is now executed at MOMOmaster.R instead of here.
    cat("(delay) "); source(sprintf("%s/delay.R", glb$CODEDIR))
    cat("(excess) "); source(sprintf("%s/excess.R", glb$CODEDIR))
    cat("(table) "); source(sprintf("%s/table.R", glb$CODEDIR))
    cat("(EUROMOMO) "); source(sprintf("%s/EUROMOMO.R", glb$CODEDIR))
    cat("(Period) "); source(sprintf("%s/Period.R", glb$CODEDIR))
    cat("(Control_graphs) "); source(sprintf("%s/Control_graphs.R", glb$CODEDIR))
    cat("(excess_graphs) "); source(sprintf("%s/excess_graphs.R", glb$CODEDIR))
    cat("(Fit_graphs) "); source(sprintf("%s/Fit_graphs.R", glb$CODEDIR))
    cat("(CUSUM_graphs) "); source(sprintf("%s/CUSUM_graphs.R", glb$CODEDIR))
  })
  cat("DONE (in "); cat(round(t2[3],1)); cat(" seconds)\n")
}

