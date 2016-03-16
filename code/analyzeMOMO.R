# MOMOpack for R

# Originally MOMOpack V 4.3 for Stata, 
# created by Bernadette Gergonne, SSI-EpiLife for Euro MOMO. 
# Port to R and further development by Theodore Lytras <thlytras@gmail.com>


analyzeMOMO <- function(mi, version="v4-3", datesISO=TRUE, useAUTOMN=FALSE, USEglm2=TRUE, zvalue=1.96, compatibility.mode=FALSE, verbose=TRUE) {
  if (!("MOMOinput" %in% class(mi)))
    stop("Argument 'mi' should have class \"MOMOinput\".")
  return(lapply(attr(mi, "groups"), function(x) {
    if (verbose) cat(sprintf("- Iterating over group %s... ", x))
    ret <- analyzeMOMOgroup(mi, x, version, datesISO, useAUTOMN, USEglm2, zvalue, compatibility.mode)
    if (verbose) cat("DONE\n")
    ret
  }))
}

analyzeMOMOgroup <- function(mi, group, version="v4-3", datesISO=TRUE, useAUTOMN=FALSE, USEglm2=TRUE, zvalue=1.96, compatibility.mode=FALSE) {
  if (!("MOMOinput" %in% class(mi)))
    stop("Argument 'mi' should have class \"MOMOinput\".")
  if (sum(mi[,sprintf("GRP%s", group)], na.rm=TRUE) == nrow(mi)) {
    groupfile <- mi
  } else {
    groupfile <- mi[mi[,sprintf("GRP%s", group)] & !is.na(mi[,sprintf("GRP%s", group)]),]
  }
  aggr <- aggregateMOMO(groupfile, group, compatibility.mode)
  aggr_fullDelay <- delayMOMO(aggr, zvalue)
  aggr_delay <- trimDelayMOMO(aggr_fullDelay)
  final <- excessMOMO(aggr_delay, version, useAUTOMN, USEglm2, zvalue)
  table <- tableMOMO(final)
  EUROMOMO <- EUROMOMOoutput(final, useAUTOMN, datesISO)
  periods <- list(
    cumChoice = calcPeriodMOMO(final, attr(mi, "WStart"), attr(mi, "WEnd")),   # 1. using the chosen Period
    cumWinter = calcPeriodMOMO(final, 40, 20),   # 2. WINTER EXCESS WEEK 40 to 20
    cumSummer = calcPeriodMOMO(final, 21, 39),   # 3. SUMMER EXCESS WEEK 21 to 39
    cumYear = calcPeriodMOMO(final, 1, 53, attr(mi, "WStart")),   # 4. EXCESS FULL YEAR WEEK 1 to 53
    cumSeason = calcPeriodMOMO(final, 27, 26))   # 5. EXCESS FULL SEASON w27 to w26
  attr(periods, "WStart") <- attr(mi, "WStart")
  attr(periods, "WEnd") <- attr(mi, "WEnd")
  return(list(
    aggregate = aggr, aggregate_fullDelay = aggr_fullDelay, aggregate_delay = aggr_delay, 
    finalDataset = final, MOMOtable=table, EUROMOMOcomplete = EUROMOMO$COMPLETE, 
    EUROMOMOrestricted = EUROMOMO$RESTRICTED, periods = periods))

}

