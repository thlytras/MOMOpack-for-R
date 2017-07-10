# MOMOpack for R version 0.2

# Originally MOMOpack V 4.3 for Stata,
# created by Bernadette Gergonne, SSI-EpiLife for Euro MOMO.
# Port to R and further development by Theodore Lytras <thlytras@gmail.com>

#' Placeholder list for all the following options
#' @export SetOpts
SetOpts <- function(
  DoA=as.Date("2015-8-10"),
  DoPR=as.Date("2008-1-1"),
  WStart=1,
  WEnd=52,
  country = "Greece",
  source = "UoP",
  MFILE = "greece_m.dta",
  HFILE = "holidayfilegreece.dta",
  INPUTDIR = "./input",
  WDIR = "./output",
  back = 3,
  WWW = 290,
  Ysum = 2009,
  Wsum = 34,
  USEglm2 = TRUE,
  useAUTOMN = TRUE,
  datesISO = FALSE,
  plotGraphs = TRUE){

  opts$DoA <- DoA
  opts$DoPR <- DoPR
  opts$WStart <- WStart
  opts$WEnd <- WEnd
  opts$country <- country
  opts$source <- source
  opts$MFILE <- MFILE
  opts$HFILE <- HFILE
  opts$INPUTDIR <- INPUTDIR
  opts$WDIR <- WDIR
  opts$back <- back
  opts$WWW <- WWW
  opts$Ysum <- Ysum
  opts$Wsum <- Wsum
  opts$USEglm2 <- USEglm2
  opts$useAUTOMN <- useAUTOMN
  opts$datesISO <- datesISO
  opts$plotGraphs <- plotGraphs
}

#' @import foreign
#' @import data.table
#' @export RunMoMo
RunMoMo <- function(){

  # Analysis
  # National level
  # by population subgroup

  cat("Welcome to MOMOpack for R.\n\n")

  t0 <- system.time({

  # Load the MOMO functions and any required packages
  cat("Loading MOMO functions and required packages... ")
  #source(sprintf("%s/loadMOMOpack.R", opts$CODEDIR))
  library(foreign, quietly=TRUE)
  cat("DONE\n")

  # Read in the input files in Stata format
  # (This section can be modified appropriately if input file is in another format)
  cat("Reading in input files... ")
  t1 <- system.time({
    if(stringr::str_detect(opts$MFILE,".dta$")){
      MOMOfile <- read.dta(paste(opts$INPUTDIR, opts$MFILE, sep="/"))
    } else if(stringr::str_detect(opts$MFILE,".txt$")){
      MOMOfile <- as.data.frame(data.table::fread(paste(opts$INPUTDIR, opts$MFILE, sep="/")))
    } else {
      stop("unknown file type for MFILE")
    }
    #MOMOfile <- MOMOfile[,c("DoD", "DoR", "age")]
    MOMOfile$DoD <- as.Date(MOMOfile$DoD, origin="1960-1-1")
    MOMOfile$DoR <- as.Date(MOMOfile$DoR, origin="1960-1-1")
    MOMOfile <- MOMOfile[MOMOfile$DoD<=opts$DoA & MOMOfile$DoR<=opts$DoA,]

    if(stringr::str_detect(opts$HFILE,".dta$")){
      hfile <- read.dta(paste(opts$INPUTDIR, opts$HFILE, sep="/"))[,c("date", "closed")]
    } else if(stringr::str_detect(opts$HFILE,".txt$")){
      hfile <- as.data.frame(data.table::fread(paste(opts$INPUTDIR, opts$HFILE, sep="/"))[,c("date", "closed")])
    } else {
      stop("unknown file type for HFILE")
    }
    hfile$date <- as.Date(hfile$date)

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
}
