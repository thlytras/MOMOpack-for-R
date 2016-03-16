# MOMOpack for R version 0.2

# Originally MOMOpack V 4.3 for Stata, 
# created by Bernadette Gergonne, SSI-EpiLife for Euro MOMO. 
# Port to R and further development by Theodore Lytras <thlytras@gmail.com>


# Placeholder list for all the following options
opts <- list()


# CHANGE THE INFORMATION BETWEEN BRACKETS
# ACCORDING TO YOUR NEEDS and your working directories

# DATE OF AGGREGATION (see specifications, the only information to change weekly)
# (Provided in ISO format, i.e. YYYY-MM-DD)
opts$DoA <- as.Date("2015-8-10")

# DATE OF START of a regular MOMO registration (see specifications)
# (Provided in ISO format, i.e. YYYY-MM-DD)
opts$DoPR <- as.Date("2008-1-1")



# COMPARISON of CUMULATIVE EXCESS 
# Chose the period of interest:   
# Week of start and Week of end of the period to study EVERY YEAR or "SEASON"
# e.g. "influenza season" as define by EISS will be defined as WStart = 40, WEnd= 20
# e.g. summer could be defined as WStart = 26, WEnd= 40
# at the end, you will get a summary table for the period chosen. 
opts$WStart <- 1
opts$WEnd <- 52

# COUNTRY NAME and SOURCE of data
opts$country = "Greece"
opts$source = "UoP"


# FILES NEEDED FOR ANALYSIS 
# (Directories must exist, no \ at the end.)
# (Both relative and absolute pathnames work, but absolute are recommended.)

# name of your mortality file, stata format
opts$MFILE <- "greece_m.dta"

# name of file containing bank Holidays (see specifications)
opts$HFILE <- "holidayfilegreece.dta"

# INPUT DIRECTORY (where the above two files can be found)
opts$INPUTDIR <- "./input"

# CODE DIRECTORY (where all the scripts -except this one- are to be found)
opts$CODEDIR <- "./code"

# OUTPUT DIRECTORY (all output will go here)
opts$WDIR <- "./output"


# CHOICE OF PARAMETERS FOR THE ANALYSIS

# chose the number of weeks to remove for modeling delay
# = the part of the series that require delay correction (see specifications)
opts$back <- 3

# choose length of retrospective historical study period in weeks
opts$WWW <- 290

# START OF CUSUM CHART: Week for CUSUM to be set to 0
opts$Ysum = 2009
opts$Wsum = 34


# ADDITIONAL OPTIONS

# Use glm2() if package glm2 is available, in order to improve convergence properties ?
# (This is equivalent to using irls option in Stata glm)
opts$USEglm2 <- TRUE

# Keep using the column name "Automn" (instead of "Autumn") as in Stata MOMOpack ?
opts$useAUTOMN <- FALSE

# When saving dates in text files, use ISO format (standard in R) instead of the Stata "%d" format ?
opts$datesISO <- TRUE

# Setting this to FALSE suppressess the plotting of the various graphs (and saves time)
opts$plotGraphs <- TRUE


# ******** DO NOT MODIFY BELOW THIS LINE ********

# DEFINITION OF the GROUPS to be analyzed
# other age group or any kind of group can be defined with the same method
# and can overlap if needed.

MOMOgroups <- list(
  "0to4" =  "age >= 0 & age <=4",
  "5to14" = "age >= 5 & age <=14",
  "15to64" = "age >= 15 & age <=64",
  "65P" = "age >= 65 | is.na(age)",
  "Total" = "age >= 0 | is.na(age)"
)

# Names in the following vector should correspond to the groups above,
# and the corresponding values (model to use for each group)
# should be one of "LINE", "SPLINE", "LINE_SIN", "SPLINE_SIN"

MOMOmodels <- c(
  "0to4" = "LINE",
  "5to14" = "LINE",
  "15to64" = "LINE_SIN",
  "65P" = "LINE_SIN",
  "Total" = "LINE_SIN"
)



# Analysis
# National level
# by population subgroup

cat("Welcome to MOMOpack for R.\n\n")

t0 <- system.time({

# Load the MOMO functions and any required packages
cat("Loading MOMO functions and required packages... ")
source(sprintf("%s/loadMOMOpack.R", opts$CODEDIR))
library(foreign, quietly=TRUE)
cat("DONE\n")

# Read in the input files in Stata format
# (This section can be modified appropriately if input file is in another format)
cat("Reading in input files... ")
t1 <- system.time({
  MOMOfile <- read.dta(paste(opts$INPUTDIR, opts$MFILE, sep="/"))
  #MOMOfile <- MOMOfile[,c("DoD", "DoR", "age")]
  MOMOfile$DoD <- as.Date(MOMOfile$DoD, origin="1960-1-1")
  MOMOfile$DoR <- as.Date(MOMOfile$DoR, origin="1960-1-1")
  hfile <- read.dta(paste(opts$INPUTDIR, opts$HFILE, sep="/"))[,c("date", "closed")]
})
cat(sprintf("DONE (in %s seconds)\n", round(t1[3], 2)))


cat("\nCreating MOMO input... ")
t2 <- system.time({
  MOMOinput <- makeMOMOinput(MOMOfile, opts$DoA, opts$DoPR, hfile, 
    country=opts$country, source=opts$source, colnames=c("DoD", "DoR", "age"),
    WStart=opts$WStart, WEnd=opts$WEnd, Ysum=opts$Ysum, Wsum=opts$Wsum,
    groups=MOMOgroups, models=MOMOmodels, delayCorr=opts$back, histPer=opts$WWW, 
    compatibility.mode=TRUE)
})
cat(sprintf("DONE (in %s seconds)\n", round(t2[3], 2)))


cat("Iterating over age groups:\n")
MOMOoutput <- analyzeMOMO(MOMOinput, datesISO=opts$datesISO, useAUTOMN=opts$useAUTOMN, 
	USEglm2=opts$USEglm2, compatibility.mode=TRUE, verbose=TRUE)


cat("Joining output... ")
MOMOjoinedOutput <- joinMOMOoutput(MOMOoutput)
cat("DONE\n")


cat("Creating MOMO directories and writing all output to disk... ")
MOMOdirs <- createMOMOdirectories(MOMOoutput, opts$WDIR)
writeMOMOoutput(MOMOjoinedOutput, MOMOdirs, MOMOoutput)
cat("DONE\n")


if (opts$plotGraphs) {
  t3 <- system.time({
    cat("\nPlotting graphs:")
    cat(" (Control graphs)");controlGraphsMOMO(MOMOoutput, MOMOdirs)
    cat(" (Excess graphs)");excessGraphsMOMO(MOMOoutput, MOMOdirs)
    cat(" (Fit graphs)");fitGraphsMOMO(MOMOoutput, MOMOdirs)
    cat(" (CUSUM graphs)");CUSUMgraphsMOMO(MOMOoutput, MOMOdirs)
  })
  cat(sprintf(" DONE \n\t(in %s seconds)\n", round(t3[3], 1)))
}


})

cat("\nCompleted the analysis in "); cat(round(t0[3],1)); cat(" seconds total.\n")

