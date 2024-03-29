# MOMOpack for R

# Originally MOMOpack V 4.3 for Stata, 
# created by Bernadette Gergonne, SSI-EpiLife for Euro MOMO. 
# Ported into R by Theodore Lytras <thlytras@gmail.com>


excessMOMO <- function(aggr, version, useAUTOMN, USEglm2, zvalue=1.96) {
  temp <- cbind(aggr, lspline(aggr$wk, nknots=2, names=c("Swk1", "Swk2", "Swk3")))
  aggr <- transferMOMOattributes(temp, aggr)

  # DEFINITION OF SPRING AND AUTUMN
  attr(aggr, "SPRING") <- c(15, 26)
  attr(aggr, "AUTUMN") <- c(36, 45)

  # data management
  aggr$season <- NA
  for (YYY in unique(aggr$YoDi)) {
    aggr$season[aggr$WoDi > 26 & aggr$YoDi == YYY] <- YYY
    aggr$season[aggr$WoDi <= 26 & aggr$YoDi == YYY] <- YYY - 1
  }

  aggr <- aggr[order(aggr$season, aggr$YoDi, aggr$WoDi),]
  aggr$Sweek <- unlist(lapply(table(aggr$season), function(x)1:x))
  aggr$sin <- sin(2*pi*aggr$wk/52.18)
  aggr$cos <- cos(2*pi*aggr$wk/52.18)

  # generation model and indicators of excess
  aggr$excess <- NA
  aggr$zscore <- NA
  aggr$DOTm <- NA
  aggr$DOTc <- NA
  aggr$DOTb <- NA
  aggr$DOTz <- NA
  aggr$DOTzm <- NA
  aggr$UCIe <- NA
  aggr$LCIe <- NA


  # we do not use data before WWW week before the Drop date
  # for modelling baseline without the influence of A-FLU
  attr(aggr, "DROP") <- aggr$wk[aggr$YoDi == attr(aggr, "Ydrop") & aggr$WoDi == attr(aggr, "Wdrop")]


  # Conditions for modelling
  # We remove "winter" and "summer"...actually we keep spring and autumn
  aggr$COND3[(aggr$WoDi>attr(aggr, "SPRING")[1] & aggr$WoDi<attr(aggr, "SPRING")[2]) | (aggr$WoDi>attr(aggr, "AUTUMN")[1] & aggr$WoDi<attr(aggr, "AUTUMN")[2])] <- 1
  aggr$COND3[eval(parse(text=attr(aggr, "DropPeriods")), aggr)] <- NA
  # We remove the previous weeks if there is no unusual excess observed
  aggr$COND4 <- as.integer(aggr$YoDi<attr(aggr, "Ydrop") | (aggr$YoDi==attr(aggr, "Ydrop") & aggr$WoDi<attr(aggr, "Wdrop")))
  #we remove the period with delay 
  aggr$COND5[aggr$wk < attr(aggr, "WEEK2")] <- 1
  # we keep only valid historical data
  aggr$COND6[(aggr$wk > attr(aggr, "WEEK") - attr(aggr, "histPer")) & (aggr$wk <= attr(aggr, "WEEK"))] <- 1

  aggr$CONDmodel[with(aggr, COND3==1 & COND4==1 & COND5==1 & COND6==1)] <- 1
  aggr$CONDpred[aggr$COND6 == 1] <- 1



  # DETECTION OF ANY EXCESS adapted Serfling MODEL

  # Use glm2() if package glm2 is available, in order to improve convergence properties
  # (This is equivalent to the option irls in stata glm)
  glmToUse <- c("glm", "glm2")[(USEglm2 && suppressWarnings(require(glm2, quietly=TRUE)))+1]

  if (attr(aggr,"model")=="LINE") {
    m1 <- do.call(glmToUse, list(nbc ~ wk, data=subset(aggr, CONDmodel==1), family=poisson))
    od <- max(1,sum(m1$weights * m1$residuals^2)/m1$df.r)
    if (od > 1) m1 <- do.call(glmToUse, list(nbc ~ wk, data=subset(aggr, CONDmodel==1), family=quasipoisson))
    aggr$Model <- "LINE"
  }

  if (attr(aggr,"model")=="SPLINE") {
    m1 <- do.call(glmToUse, list(nbc ~ Swk1 + Swk2 + Swk3, data=subset(aggr, CONDmodel==1), family=poisson))
    od <- max(1,sum(m1$weights * m1$residuals^2)/m1$df.r)
    if (od > 1) m1 <- do.call(glmToUse, list(nbc ~ Swk1 + Swk2 + Swk3, data=subset(aggr, CONDmodel==1), family=quasipoisson))
    aggr$Model <- "SPLINES"
  }

  if (attr(aggr,"model")=="LINE_SIN") {
    m1 <- do.call(glmToUse, list(nbc ~ sin + cos + wk, data=subset(aggr, CONDmodel==1), family=poisson))
    od <- max(1,sum(m1$weights * m1$residuals^2)/m1$df.r)
    if (od > 1) m1 <- do.call(glmToUse, list(nbc ~ sin + cos + wk, data=subset(aggr, CONDmodel==1), family=quasipoisson))
    aggr$Model <- "LINES_SIN"
  }

  if (attr(aggr,"model")=="SPLINE_SIN") {
    m1 <- do.call(glmToUse, list(nbc ~ sin + cos + Swk1 + Swk2 + Swk3, data=subset(aggr, CONDmodel==1), family=poisson))
    od <- max(1,sum(m1$weights * m1$residuals^2)/m1$df.r)
    if (od > 1) m1 <- do.call(glmToUse, list(nbc ~ sin + cos + Swk1 + Swk2 + Swk3, data=subset(aggr, CONDmodel==1), family=quasipoisson))
    aggr$Model <- "SPLINES_SIN"
  }

  aggr$Pnb <- NA
  aggr$resi <- NA
  aggr$stdp <- NA
  aggr$Pnb[which(aggr$CONDpred==1)] <- predict(m1, aggr[which(aggr$CONDpred==1),], type="response")
  aggr$resi[which(aggr$CONDmodel==1)] <- aggr$nbc[which(aggr$CONDmodel==1)] - predict(m1, aggr[which(aggr$CONDmodel==1),], type="response")
  aggr$stdp[which(aggr$CONDpred==1)] <- predict(m1, aggr[which(aggr$CONDpred==1),], se.fit=TRUE)$se.fit


  for(Z in seq(2,20,2)) {
    aggr[[paste("UPIb",Z,sep="")]] <- (aggr$Pnb^(2/3)+ Z*((4/9)*(aggr$Pnb^(1/3))*(od+(aggr$stdp^2)*(aggr$Pnb)))^(1/2))^(3/2)

    # We drop the variables UPI if they are not crossed by the data
    if (Z>2 && sum(aggr$nbc > aggr[[paste("UPIb",Z,sep="")]], na.rm=TRUE)==0) {
      #aggr[[paste("UPIb",Z,sep="")]] <- NULL ## RICHARD CHANGE
      #break
    }
  }

  aggr$zscore <- (aggr$nbc^(2/3) - aggr$Pnb^(2/3)) / ((4/9)*(aggr$Pnb^(1/3))*(od+aggr$Pnb*(aggr$stdp^2)))^(1/2)

  aggr$UCIb <- aggr$Pnb + zvalue*aggr$stdp
  aggr$LCIb <- aggr$Pnb - zvalue*aggr$stdp

  # Variations around the baseline
  aggr$excess <- aggr$nbc - aggr$Pnb
  aggr$UCIe <- aggr$UCIc - aggr$Pnb
  aggr$LCIe <- aggr$LCIc - aggr$Pnb


  # To better observe the week under study in the graph, we create new variables
  aggr$DOTb[which(aggr$wk>=attr(aggr, "WEEK2") & aggr$wk<=attr(aggr, "WEEK"))] <- aggr$Pnb[which(aggr$wk>=attr(aggr, "WEEK2") & aggr$wk<=attr(aggr, "WEEK"))]
  aggr$DOTc[which(aggr$wk>=attr(aggr, "WEEK2") & aggr$wk<=attr(aggr, "WEEK"))] <- aggr$nbc[which(aggr$wk>=attr(aggr, "WEEK2") & aggr$wk<=attr(aggr, "WEEK"))]
  aggr$DOTz[which(aggr$wk>=attr(aggr, "WEEK2") & aggr$wk<=attr(aggr, "WEEK"))] <- aggr$zscore[which(aggr$wk>=attr(aggr, "WEEK2") & aggr$wk<=attr(aggr, "WEEK"))]
  aggr$DOTm[which(aggr$COND3==1 & aggr$COND4==1 & aggr$COND5==1)] <- aggr$nbc[which(aggr$COND3==1 & aggr$COND4==1 & aggr$COND5==1)]
  aggr$DOTzm[which(aggr$COND3==1 & aggr$COND4==1 & aggr$COND5==1)] <- aggr$zscore[which(aggr$COND3==1 & aggr$COND4==1 & aggr$COND5==1)]

  aggr <- aggr[aggr$wk<=attr(aggr, "WEEK")+1,]

  aggr$Version <- version
  aggr$Spring <- paste("(WoDi>", attr(aggr, "SPRING")[1], " & WoDi<", attr(aggr, "SPRING")[2], ")", sep="")
  aggr[,c("Autumn","Automn")[useAUTOMN+1]] <- paste("(WoDi>", attr(aggr, "AUTUMN")[1], " & WoDi<", attr(aggr, "AUTUMN")[2], ")", sep="")


  # CUSUM

  # DEFINITION OF REFERENCE PARAMETER

  # k is the reference value or allowance parameter, it is set up here /// 
  # to detect a 1.5 SD shift over 3 weeks => 0.5 SD / week for 3 weeks
  # in that case, the best k would be 0.5 SD/2 = .25
  aggr$k <- 0.25 

  # ARL0: we consider an acceptable level of 5 % of false alarm, 
  # considering a risk similar to an alpha risk
  # which lead to and ARL0 of 20 weeks
  # the meaning is that on a controled processed, 
  # an alarm will be raised due to random variation every 20 weeks. 
  aggr$ARL0 = 20

  # h is computed according to ARL0
  # h is the decision limit, computed according to Rogerson A. et al. 2003 
  aggr$h <- (((aggr$ARL0+4)/(aggr$ARL0+2))*log((aggr$ARL0/2)+1))-1.16666

  # CUM is the week to start and set the CUSUM at 0 
  attr(aggr, "CUM") <- aggr$wk[aggr$YoDi==attr(aggr, "Ysum") & aggr$WoDi==attr(aggr, "Wsum")]
  if (length(attr(aggr, "CUM"))==0) attr(aggr, "CUM") <- aggr$wk[1]

  aggr$CUSUM <- 0
  cu <- 0
  zsc <- aggr$zscore
  zsc[is.na(zsc)] <- 0
  zsc <- zsc - aggr$k
  ti <- system.time({
    for (i in which(aggr$wk>=attr(aggr, "CUM") & aggr$wk <= attr(aggr, "WEEK"))) {
      cu <- max(0, cu + zsc[i])
      aggr$CUSUM[i] <- cu
    }
  })
  aggr$FLAG[aggr$CUSUM>=aggr$h] <- 1

  # we save the FINAL DATA SET
  ret <- aggr[,c("Version", "Model", "GROUP", "WoDi", "YoDi", "wk", "wk2", 
    "season", "Sweek", "nb", "nb2", "nbr", "nbc", "UCIc", "LCIc", "UPIc", "LPIc", "Pnb", 
    names(aggr)[grep("UPIb", names(aggr), fixed=TRUE)], "LCIb", "UCIb", "excess", 
    "UCIe", "LCIe", "resi", "zscore", "DOTm", "DOTc", "DOTb", "DOTz", "DOTzm",
    "COND6", "CONDmodel", "Spring", c("Autumn","Automn")[useAUTOMN+1], 
    "CUSUM", "FLAG", "k", "ARL0", "h")]
  transferMOMOattributes(ret, aggr)
}

