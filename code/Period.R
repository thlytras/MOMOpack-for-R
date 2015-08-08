# MOMOpack for R version 0.1

# Originally MOMOpack V 4.3 for Stata, 
# created by Bernadette Gergonne, SSI-EpiLife for Euro MOMO. 
# Ported into R by Theodore Lytras <thlytras@gmail.com>


# Function to automate all the period calculations done in this script

calcPeriod <- function(aggr, WStart, WEnd){
  if (WStart == WEnd) stop("WStart and WEnd cannot be identical!")
  pfr <- aggr[order(aggr$YoDi, aggr$WoDi),]
  MODEL <- unique(pfr$Model)

  # if the period is in the same year
  if (WStart < WEnd) {
    pfr$Period[pfr$WoDi >= WStart & pfr$WoDi <= WEnd] <-1
    pfr$YearStart <- pfr$YoDi
    pfr$YearEnd <- pfr$YoDi
    pfr$WeekEnd <- WEnd
    pfr$WeekStart <- WStart
    pfr2 <- aggregate(pfr[pfr$Period==1, c("nbc", "Pnb", "excess", "zscore")], 
      pfr[pfr$Period==1, "YoDi", drop=FALSE], sum, na.rm=TRUE)
    colnames(pfr2) <- c("YoDi", "sumTotal", "sumBaseline", "sumExcess", "sumZscore")
    pfr2$Duration_week <- with(pfr[pfr$Period==1 & !is.na(pfr$zscore),], tapply(zscore, YoDi, length))[as.character(pfr2$YoDi)]
    pfr <- cbind(pfr, pfr2[match(pfr$YoDi, pfr2$YoDi),])
    pfr$Zzscore <- pfr$sumZscore / sqrt(pfr$Duration_week)
    pfr[,c("sumTotal", "sumBaseline", "sumExcess")] <- round(pfr[,c("sumTotal", "sumBaseline", "sumExcess")])
    pfr$Zzscore <- round(pfr$Zzscore, 2)
    pfr <- pfr[pfr$WoDi==WStart,]
  }

  # if the Period is over 2 years
  if (WStart > WEnd) {
    pfr$Period <- NA
    for (x in unique(pfr$season)) {
      pfr$Period[pfr$WoDi >= WStart & pfr$YoDi == pfr$season] <- 1
      pfr$Period[pfr$WoDi <= WEnd & pfr$YoDi == pfr$season+1] <- 1
    }
    pfr$YearStart <- pfr$YoDi - as.integer(pfr$WoDi <= WStart)
    pfr$YearEnd <- pfr$YoDi - as.integer(pfr$WoDi <= WStart) + 1
    pfr$WeekEnd[pfr$Period==1] <- WEnd
    pfr$WeekStart[pfr$Period==1] <- WStart
    pfr2 <- aggregate(pfr[pfr$Period==1, c("nbc", "Pnb", "excess", "zscore")], 
      pfr[pfr$Period==1, "season", drop=FALSE], sum, na.rm=TRUE)
    colnames(pfr2) <- c("season", "sumTotal", "sumBaseline", "sumExcess", "sumZscore")
    pfr2$Duration_week <- with(pfr[pfr$Period==1 & !is.na(pfr$zscore),], tapply(zscore, season, length))[as.character(pfr2$season)]
    pfr <- cbind(pfr, pfr2[match(pfr$season, pfr2$season),])
    pfr$Zzscore <- pfr$sumZscore / sqrt(pfr$Duration_week)
    pfr[,c("sumTotal", "sumBaseline", "sumExcess")] <- round(pfr[,c("sumTotal", "sumBaseline", "sumExcess")])
    pfr$Zzscore <- round(pfr$Zzscore, 2)
    pfr <- pfr[pfr$WoDi==1,]
  }

  pfr <- pfr[,c("YearStart", "WeekStart", "YearEnd", "WeekEnd", "Duration_week", "sumTotal", "sumBaseline", "sumExcess", "Zzscore")]
  pfr <- as.data.frame(t(pfr))
  colnames(pfr) <- paste("v", 1:ncol(pfr), sep="")
  pfr$Group <- glb$GROUP
  pfr$Version <- glb$VERSION
  pfr$Model <- MODEL
  pfr$Indicator <- c("Year of Period Start", "Week of Period Start", "Year of Period End", "Week of Period End", 
	  "Duration of the Study Period", "Total number of deaths", "Expected number of death (baseline)", 
	  "Crude variation around the baseline", "Standardised variation around the baseline (Zscore)")
  pfr <- pfr[,c("Version", "Group", "Model", "Indicator", paste("v", 1:(ncol(pfr)-4), sep=""))]

  return(pfr)
}



# 1. using the chosen Period
temp <- calcPeriod(aggr5, glb$WStart, glb$WEnd)
toBeMerged$cumChoice[[length(toBeMerged$cumChoice)+1]] <- temp   # Keep for later merging
if (glb$DEBUG) write.dta(temp, 
  sprintf("%s/CUMULATIVE-Choice-w%s-w%s-MOMO-%s-%s-%s-%s.dta", glb$CUMULATIVE, glb$WStart, glb$WEnd, glb$country, glb$GROUP, glb$YOSI, glb$WOSI))


# 2. WINTER EXCESS WEEK 40 to 20

temp <- calcPeriod(aggr5, 40, 20)
toBeMerged$cumWinter[[length(toBeMerged$cumWinter)+1]] <- temp   # Keep for later merging
write.dta(temp, 
  sprintf("%s/CUMULATIVE-WINTER-w40-w20-MOMO-%s-%s-%s-%s.dta", glb$CUMULATIVE, glb$country, glb$GROUP, glb$YOSI, glb$WOSI))


# 3. SUMMER EXCESS WEEK 21 to 39

temp <- calcPeriod(aggr5, 21, 39)
toBeMerged$cumSummer[[length(toBeMerged$cumSummer)+1]] <- temp   # Keep for later merging
write.dta(temp, 
  sprintf("%s/CUMULATIVE-SUMMER-w21-w39-MOMO-%s-%s-%s-%s.dta", glb$CUMULATIVE, glb$country, glb$GROUP, glb$YOSI, glb$WOSI))


# 4. EXCESS FULL YEAR WEEK 1 to 53

temp <- calcPeriod(aggr5, 1, 53)
toBeMerged$cumYear[[length(toBeMerged$cumYear)+1]] <- temp   # Keep for later merging
write.dta(temp, 
  sprintf("%s/CUMULATIVE-YEAR-w1-w53-MOMO-%s-%s-%s-%s.dta", glb$CUMULATIVE, glb$country, glb$GROUP, glb$YOSI, glb$WOSI))


# 5. EXCESS FULL SEASON w27 to w26

temp <- calcPeriod(aggr5, 27, 26)
toBeMerged$cumSeason[[length(toBeMerged$cumSeason)+1]] <- temp   # Keep for later merging
write.dta(temp, 
  sprintf("%s/CUMULATIVE-SEASON-w27-w26-MOMO-%s-%s-%s-%s.dta", glb$CUMULATIVE, glb$country, glb$GROUP, glb$YOSI, glb$WOSI))

