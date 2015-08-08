# MOMOpack for R version 0.1

# Originally MOMOpack V 4.3 for Stata, 
# created by Bernadette Gergonne, SSI-EpiLife for Euro MOMO. 
# Ported into R by Theodore Lytras <thlytras@gmail.com>


# Loading required packages
library(foreign)

# Placeholder list for all global variables
glb <- list()


# CHANGE THE INFORMATION BETWEEN BRACKETS
# ACCORDING TO YOUR NEEDS and your working directories

# DATE OF AGGREGATION (see specifications, the only information to change weekly)
glb$YEAR <- 2015
glb$MONTH <- 07
glb$DAY <- 27

# COMPARISON of CUMULATIVE EXCESS 
# Chose the period of interest:   
# Week of start and Week of end of the period to study EVERY YEAR or "SEASON"
# e.g. "influenza season" as define by EISS will be defined as WStart = 40, WEnd= 20
# e.g. summer could be defined as WStart = 26, WEnd= 40
# at the end, you will get a summary table for the period chosen. 
glb$WStart <- 1
glb$WEnd <- 52

# COUNTRY NAME and SOURCE of data
glb$country = "Greece"
glb$source = "UoP"

# DATE OF START of a regular MOMO registration (see specifications)
glb$RYEAR = 2008
glb$RMONTH = 01
glb$RDAY = 01

# Setting this to TRUE saves all intermediate files
glb$DEBUG <- FALSE

# Use glm2() if package glm2 is available, in order to improve convergence properties ?
glb$USEglm2 <- FALSE



# FILES NEEDED FOR ANALYSIS 
# (Directories must exist, no \ at the end.)
# (Both relative and absolute pathnames work, but absolute are recommended.)

# name of your mortality file, stata format
glb$MFILE <- "greece_m.dta"

# name of file containing bank Holidays (see specifications)
glb$HFILE <- "holidayfilegreece.dta"

# INPUT DIRECTORY (where the above two files can be found)
glb$INPUTDIR <- "./input"

# CODE DIRECTORY (where all the scripts -except this one- are to be found)
glb$CODEDIR <- "./code"

# OUTPUT DIRECTORY (all output will go here)
glb$WDIR <- "./output"



# CHOICE OF PARAMETERS FOR THE ANALYSIS

# chose the number of weeks to remove for modeling delay
# = the part of the series that require delay correction (see specifications)
glb$back <- 3

# choose length of retrospective historical study period in weeks
glb$WWW <- 290


# START OF CUSUM CHART: Week for CUSUM to be set to 0
glb$Ysum = 2009
glb$Wsum = 34



# ******** DO NOT MODIFY BELOW THIS LINE ********

# Year and week after which data are not accunted for in the model 
# This must not be changed currently thank you. 
glb$Ydrop = 2014
glb$Wdrop = 40

# Analysis
# National level
# by population subgroup

cat("Welcome to MOMOpack for R.\n\n")

t0 <- system.time({

source(sprintf("%s/include.R", glb$CODEDIR)) # Some necessary helper functions 
source(sprintf("%s/data1.R", glb$CODEDIR))
source(sprintf("%s/directories.R", glb$CODEDIR)) # Create output sub-directories
source(sprintf("%s/group.R", glb$CODEDIR))
source(sprintf("%s/Append.R", glb$CODEDIR))

})

cat("\nCompleted the analysis in "); cat(round(t0[3],1)); cat(" seconds total.\n")

