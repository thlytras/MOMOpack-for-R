# MOMOpack for R

# Originally MOMOpack V 4.3 for Stata, 
# created by Bernadette Gergonne, SSI-EpiLife for Euro MOMO. 
# Ported into R by Theodore Lytras <thlytras@gmail.com>


# REMINDER: 
# $WEEK = Week number to study according to the date of aggregation 
#      = complete ISO week, (From monday to Sunday) preceding the date of aggregation 
# $PRWEEK = the first week when the MOMO registration became smooth and regular
# $back = the number of week to remove from the series to model 
# $WEEK2 is the week number until which we want model the series
# WRxx = the number of death registered xx week (FULL WEEK) after the week of death, ACCORDING TO WHAT WE KNOW THE DAY OF AGGREGATION
#      = number of death registered at WoS who died XX week(s) before
# WRxxA = what is registered xx week after (incomplete week) until the day of Aggregation 
#       = what we know in addition if we aggregate i.e. on wednesday instead of sunday. 
# YW = the ID of the week (concatenation of iso Year and iso week of death)
# WoDi = Week of Death Iso
# YoDi = Year of Death Iso
# closed = the number of day off during the iso week
# closed2 = the number of day off during the following week from Monday until the day of Aggregation
# nb = weekly number of death in the series provided
# nb2 = weekly number of death known (already registered) at the date of Aggregation
# wk = iterative number of the week 
# Ywk = the stata format week number (drop week 53 !) as Stata cannot work sith ISO weeks
# nbc = corrected number of death 
# nbr = registered numbre of death
# UCIc = Upper Confidence Interval of the corrected number of deaths
# LCIc = Lower Confidence Interval of the corrected number of deaths
# UPIc = Upper Prediction Interval of the corrected number of deaths
# LPIc = Lower Prediction Interval of the corrected number of deaths

delayMOMO <- function(aggr, zvalue=1.96) {
  aggr <- aggr[order(aggr$wk),]

  # the period of registration for a death week XX
  aggr$closed0 <- aggr$closed + vecshift(aggr$closedA, 1)
  for (VV in 1:attr(aggr, "delayCorr")) {
    aggr[[paste("closed", VV, sep="")]] <- aggr[[paste("closed", VV-1, sep="")]] + vecshift(aggr$closed0, -VV)
  }

  # CORRECTION FOR DELAY
  # FIRST we model what we know about the previous week.
  aggr$pred <- NA
  aggr$UCIc <- NA
  aggr$LCIc <- NA 
  aggr$UPIc <- NA
  aggr$LPIc <- NA
  aggr$GROUP <- attr(aggr, "group")

  for (XX in 0:attr(aggr, "delayCorr")) {
    aggr[[paste("CCC", XX, sep="")]] <- vecshift(aggr[[paste("closed", XX, sep="")]], XX)
    aggr[[paste("a", XX, sep="")]] <- ifelse((aggr$wk>attr(aggr, "PRWEEK") & aggr$wk<=attr(aggr, "WEEK2")), (aggr[[paste("WR", XX, sep="")]]/aggr$nb), NA)

    m1 <- suppressWarnings(glm(as.formula(paste("a", XX, " ~ CCC", XX, " + wk", sep="")), data=subset(aggr, wk>attr(aggr, "PRWEEK") & wk<attr(aggr, "WEEK2")), family=binomial))
    aggr[[paste("Pa", XX, sep="")]] <- predict(m1, aggr, type="response")
    aggr[[paste("Pa", XX, sep="")]][which(aggr$wk<=attr(aggr, "PRWEEK") | aggr$wk>attr(aggr, "WEEK"))] <- NA

    aggr[[paste("temp", XX, sep="")]] <- aggr[[paste("WR", XX, sep="")]] / aggr[[paste("Pa", XX, sep="")]]

    m1 <- glm(as.formula(paste("nb2 ~ WR", XX, " + Pa", XX, " + wk", sep="")), data=subset(aggr, wk>attr(aggr, "PRWEEK") & wk<attr(aggr, "WEEK2")), family=poisson)
    od <- max(1,sum(m1$weights * m1$residuals^2)/m1$df.r)
    if (od > 1) m1 <- glm(as.formula(paste("nb2 ~ WR", XX, " + Pa", XX, " + wk", sep="")), data=subset(aggr, wk>attr(aggr, "PRWEEK") & wk<attr(aggr, "WEEK2")), family=quasipoisson)

    tryCatch(
      aggr[[paste("pred", XX, sep="")]] <- predict(m1, aggr, type="response"),
      warning = function(w) 
	if (conditionMessage(w)=="prediction from a rank-deficient fit may be misleading") 
	  warning(   # Giving a more informative warning
	  "In group '", attr(aggr, "group"), "', the delay Poisson model fit for lag ", XX, " week(s)\n",
	  "        is rank deficient. Prediction may be misleading.", call.=FALSE) else warning(w)
    )
    aggr[[paste("pred", XX, sep="")]][aggr$wk<=attr(aggr, "PRWEEK") | aggr$wk>attr(aggr, "WEEK")-XX] <- NA

    tryCatch(
      aggr[[paste("stdp", XX, sep="")]] <- predict(m1, aggr, se.fit=TRUE)$se.fit,
      warning = function(w) 
	# If we have the same warning about rank deficiency as above, there's no reason to print it twice
	if (conditionMessage(w)!="prediction from a rank-deficient fit may be misleading") 
	  warning(w)
    )
    aggr[[paste("stdp", XX, sep="")]][aggr$wk<=attr(aggr, "PRWEEK") | aggr$wk>attr(aggr, "WEEK")-XX] <- NA

    aggr[[paste("N", XX, sep="")]] <- sum(!is.na(aggr[[paste("stdp", XX, sep="")]]))
    aggr[[paste("temp", XX, sep="")]] <- NULL

    # Prediction Interval
    aggr[[paste("UPI", XX, sep="")]] <- (aggr[[paste("pred", XX, sep="")]]^(2/3) + zvalue*((4/9)*(aggr[[paste("pred", XX, sep="")]]^(1/3))*(od+(aggr[[paste("stdp", XX, sep="")]]^2)*(aggr[[paste("pred", XX, sep="")]])))^(1/2))^(3/2) 
    aggr[[paste("LPI", XX, sep="")]] <- (aggr[[paste("pred", XX, sep="")]]^(2/3) - zvalue*((4/9)*(aggr[[paste("pred", XX, sep="")]]^(1/3))*(od+(aggr[[paste("stdp", XX, sep="")]]^2)*(aggr[[paste("pred", XX, sep="")]])))^(1/2))^(3/2) 

    aggr[[paste("UCI", XX, sep="")]] <- aggr[[paste("pred", XX, sep="")]] + zvalue*aggr[[paste("stdp", XX, sep="")]]
    aggr[[paste("LCI", XX, sep="")]] <- aggr[[paste("pred", XX, sep="")]] - zvalue*aggr[[paste("stdp", XX, sep="")]]

    aggr$pred[aggr$wk == attr(aggr, "WEEK")-XX] <- aggr[[paste("pred", XX, sep="")]][aggr$wk == attr(aggr, "WEEK")-XX]
    aggr$UCIc[aggr$wk == attr(aggr, "WEEK")-XX] <- aggr[[paste("UCI", XX, sep="")]][aggr$wk == attr(aggr, "WEEK")-XX]
    aggr$LCIc[aggr$wk == attr(aggr, "WEEK")-XX] <- aggr[[paste("LCI", XX, sep="")]][aggr$wk == attr(aggr, "WEEK")-XX]
    aggr$UPIc[aggr$wk == attr(aggr, "WEEK")-XX] <- aggr[[paste("UPI", XX, sep="")]][aggr$wk == attr(aggr, "WEEK")-XX]
    aggr$LPIc[aggr$wk == attr(aggr, "WEEK")-XX] <- aggr[[paste("LPI", XX, sep="")]][aggr$wk == attr(aggr, "WEEK")-XX]

    aggr[[paste("UCI", XX, sep="")]][aggr$wk < attr(aggr, "WEEK2")] <- NA
    aggr[[paste("LCI", XX, sep="")]][aggr$wk < attr(aggr, "WEEK2")] <- NA
    aggr[[paste("UPI", XX, sep="")]][aggr$wk < attr(aggr, "WEEK2")] <- NA
    aggr[[paste("LPI", XX, sep="")]][aggr$wk < attr(aggr, "WEEK2")] <- NA

  }


  # we generate the CORRECTED number of death
  aggr$nbc[aggr$wk < attr(aggr, "WEEK2")] <- aggr$nb[aggr$wk < attr(aggr, "WEEK2")]
  aggr$nbc[aggr$wk >= attr(aggr, "WEEK2") & aggr$wk <= attr(aggr, "WEEK")] <- pmax(aggr$pred[aggr$wk >= attr(aggr, "WEEK2") & aggr$wk <= attr(aggr, "WEEK")], aggr$nb[aggr$wk >= attr(aggr, "WEEK2") & aggr$wk <= attr(aggr, "WEEK")], na.rm=TRUE)

  return(aggr)
}



trimDelayMOMO <- function(aggr) {
  for (XX in 1:attr(aggr, "delayCorr")) {
    aggr[[paste("pred", XX, sep="")]] <- NULL
    aggr[[paste("UCI", XX, sep="")]] <- NULL
    aggr[[paste("LCI", XX, sep="")]] <- NULL
    aggr[[paste("UPI", XX, sep="")]] <- NULL
    aggr[[paste("LPI", XX, sep="")]] <- NULL
  }
  aggr$nbc[is.na(aggr$nbc)] <- 0

  ret <- aggr[,c("GROUP", "WoDi", "YoDi", "wk", "wk2", "nb", "nb2", "nbr", "nbc", "pred", "UCIc", "LCIc", "UPIc", "LPIc")]

  # We must preserve the attributes we need
  transferMOMOattributes(ret, aggr)
}

