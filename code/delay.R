# MOMOpack for R version 0.1

# Originally MOMOpack V 4.3 for Stata, 
# created by Bernadette Gergonne, SSI-EpiLife for Euro MOMO. 
# Ported into R by Theodore Lytras <thlytras@gmail.com>


aggr5 <- aggr5[order(aggr5$wk),]


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


# the period of registration for a death week XX

aggr5$closed0 <- aggr5$closed + vecshift(aggr5$closedA, 1)
for (VV in 1:glb$back) {
  aggr5[[paste("closed", VV, sep="")]] <- aggr5[[paste("closed", VV-1, sep="")]] + vecshift(aggr5$closed0, -VV)
}


# CORRECTION FOR DELAY
# FIRST we model what we know about the previous week.

aggr5$pred <- NA
aggr5$UCIc <- NA
aggr5$LCIc <- NA 
aggr5$UPIc <- NA
aggr5$LPIc <- NA
aggr5$GROUP <- glb$GROUP


for (XX in 0:glb$back) {
  aggr5[[paste("CCC", XX, sep="")]] <- vecshift(aggr5[[paste("closed", XX, sep="")]], XX)
  aggr5[[paste("a", XX, sep="")]] <- ifelse((aggr5$wk>glb$PRWEEK & aggr5$wk<=glb$WEEK2), (aggr5[[paste("WR", XX, sep="")]]/aggr5$nb), NA)

  m1 <- suppressWarnings(glm(as.formula(paste("a", XX, " ~ CCC", XX, " + wk", sep="")), data=subset(aggr5, wk>glb$PRWEEK & wk<glb$WEEK2), family=binomial))
  aggr5[[paste("Pa", XX, sep="")]] <- predict(m1, aggr5, type="response")

  aggr5[[paste("temp", XX, sep="")]] <- aggr5[[paste("WR", XX, sep="")]] / aggr5[[paste("Pa", XX, sep="")]]

  m1 <- glm(as.formula(paste("nb2 ~ WR", XX, " + Pa", XX, " + wk", sep="")), data=subset(aggr5, wk>glb$PRWEEK & wk<glb$WEEK2), family=poisson)
  od <- max(1,sum(m1$weights * m1$residuals^2)/m1$df.r)
  if (od > 1) m1 <- glm(as.formula(paste("nb2 ~ WR", XX, " + Pa", XX, " + wk", sep="")), data=subset(aggr5, wk>glb$PRWEEK & wk<glb$WEEK2), family=quasipoisson)

  aggr5[[paste("pred", XX, sep="")]] <- predict(m1, aggr5, type="response")
  aggr5[[paste("pred", XX, sep="")]][aggr5$wk<=glb$PRWEEK | aggr5$wk>glb$WEEK-XX] <- NA
  aggr5[[paste("stdp", XX, sep="")]] <- predict(m1, aggr5, se.fit=TRUE)$se.fit
  aggr5[[paste("stdp", XX, sep="")]][aggr5$wk<=glb$PRWEEK | aggr5$wk>glb$WEEK-XX] <- NA

  aggr5[[paste("N", XX, sep="")]] <- sum(!is.na(aggr5[[paste("stdp", XX, sep="")]]))
  aggr5[[paste("temp", XX, sep="")]] <- NULL

  glb$zvalue <- 1.96

  # Prediction Interval
  aggr5[[paste("UPI", XX, sep="")]] <- (aggr5[[paste("pred", XX, sep="")]]^(2/3) + glb$zvalue*((4/9)*(aggr5[[paste("pred", XX, sep="")]]^(1/3))*(od+(aggr5[[paste("stdp", XX, sep="")]]^2)*(aggr5[[paste("pred", XX, sep="")]])))^(1/2))^(3/2) 
  aggr5[[paste("LPI", XX, sep="")]] <- (aggr5[[paste("pred", XX, sep="")]]^(2/3) - glb$zvalue*((4/9)*(aggr5[[paste("pred", XX, sep="")]]^(1/3))*(od+(aggr5[[paste("stdp", XX, sep="")]]^2)*(aggr5[[paste("pred", XX, sep="")]])))^(1/2))^(3/2) 

  aggr5[[paste("UCI", XX, sep="")]] <- aggr5[[paste("pred", XX, sep="")]] + glb$zvalue*aggr5[[paste("stdp", XX, sep="")]]
  aggr5[[paste("LCI", XX, sep="")]] <- aggr5[[paste("pred", XX, sep="")]] - glb$zvalue*aggr5[[paste("stdp", XX, sep="")]]

  aggr5$pred[aggr5$wk == glb$WEEK-XX] <- aggr5[[paste("pred", XX, sep="")]][aggr5$wk == glb$WEEK-XX]
  aggr5$UCIc[aggr5$wk == glb$WEEK-XX] <- aggr5[[paste("UCI", XX, sep="")]][aggr5$wk == glb$WEEK-XX]
  aggr5$LCIc[aggr5$wk == glb$WEEK-XX] <- aggr5[[paste("LCI", XX, sep="")]][aggr5$wk == glb$WEEK-XX]
  aggr5$UPIc[aggr5$wk == glb$WEEK-XX] <- aggr5[[paste("UPI", XX, sep="")]][aggr5$wk == glb$WEEK-XX]
  aggr5$LPIc[aggr5$wk == glb$WEEK-XX] <- aggr5[[paste("LPI", XX, sep="")]][aggr5$wk == glb$WEEK-XX]

  aggr5[[paste("UCI", XX, sep="")]][aggr5$wk < glb$WEEK2] <- NA
  aggr5[[paste("LCI", XX, sep="")]][aggr5$wk < glb$WEEK2] <- NA
  aggr5[[paste("UPI", XX, sep="")]][aggr5$wk < glb$WEEK2] <- NA
  aggr5[[paste("LPI", XX, sep="")]][aggr5$wk < glb$WEEK2] <- NA

}


# we generate the CORRECTED number of death
aggr5$nbc[aggr5$wk < glb$WEEK2] <- aggr5$nb[aggr5$wk < glb$WEEK2]
aggr5$nbc[aggr5$wk >= glb$WEEK2 & aggr5$wk <= glb$WEEK] <- pmax(aggr5$pred[aggr5$wk >= glb$WEEK2 & aggr5$wk <= glb$WEEK], aggr5$nb[aggr5$wk >= glb$WEEK2 & aggr5$wk <= glb$WEEK])


write.dta(aggr5, sprintf("%s/delay-%s-%s-%s-%s.dta", glb$CONTROL, glb$GROUP, glb$country, glb$YOSI, glb$WOSI))

aggr5del <- aggr5 # We need that for Control_graphs.R

for (XX in 1:glb$back) {
  aggr5[[paste("pred", XX, sep="")]] <- NULL
  aggr5[[paste("UCI", XX, sep="")]] <- NULL
  aggr5[[paste("LCI", XX, sep="")]] <- NULL
  aggr5[[paste("UPI", XX, sep="")]] <- NULL
  aggr5[[paste("LPI", XX, sep="")]] <- NULL
}

aggr5$nbc[is.na(aggr5$nbc)] <- 0

aggr5 <- aggr5[,c("GROUP", "WoDi", "YoDi", "wk", "wk2", "nb", "nb2", "nbr", "nbc", "pred", "UCIc", "LCIc", "UPIc", "LPIc")]

if (glb$DEBUG) write.dta(aggr5, sprintf("%s/delay.dta", glb$WDIR))

