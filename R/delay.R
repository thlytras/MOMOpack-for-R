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

delayMOMO_original <- function(aggr, zvalue=1.96) {
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

  #* Drop obs in week of aggregation # NEW!!
  aggr <- aggr[-nrow(aggr),]

  return(aggr)
}

#' @importFrom MASS polr
delayMOMO_2017_12_internal <- function(runData, zvalue=1.96){
  runData <- copy(runData)
  #
  #runData=aggr[aggr$WoDi %in% 1:52,]
  #fit <- rms::orm(delay~WR+closed,data=runData[(momoAttr$PRWEEK < runData$wk) & (runData$wk < momoAttr$WEEK2),])
  fit <- MASS::polr(delay~WR+closed,data=runData[(momoAttr$PRWEEK < runData$wk) & (runData$wk < momoAttr$WEEK2),])

  p <- predict(fit,newdata=runData,type="prob")
  for(r in 0:momoAttr$delayCorr){
    var <- sprintf("p%s",r)
    runData[,(var):=p[,r+1]]
  }
  for(i in 1:momoAttr$delayCorr){
    j=i-1
    newVar <- sprintf("p%s",i)
    oldVar <- sprintf("p%s",j)
    runData[,(newVar):=get(newVar)+get(oldVar)]
  }

  for(r in 0:momoAttr$delayCorr){
    form <- sprintf("nb~WR+p%s",r)
    fit <- glm(as.formula(form),data=runData[delay==r],family="poisson")

    od <- max(1,sum(fit$weights * fit$residuals^2)/fit$df.r)

    p <- predict(fit,newdata=runData[(momoAttr$PRWEEK < runData$wk) & (runData$wk <= momoAttr$WEEK - r),],type="response")
    stdp <- predict(fit,newdata=runData[(momoAttr$PRWEEK < runData$wk) & (runData$wk <= momoAttr$WEEK - r),],se.fit=T)$se.fit

    predvar <- sprintf("pred%s",r)

    UPIvar <- sprintf("UPI%s",r)
    LPIvar <- sprintf("LPI%s",r)

    UCIvar <- sprintf("UCI%s",r)
    LCIvar <- sprintf("LCI%s",r)

    runData[(momoAttr$PRWEEK < runData$wk) & (runData$wk <= momoAttr$WEEK - r),
         (predvar):=p]

    runData[(momoAttr$PRWEEK < runData$wk) & (runData$wk <= momoAttr$WEEK - r),
         (UPIvar):=(p^(2/3)+ zvalue*((4/9)*(p^(1/3))*(od+(stdp^2)*(p)))^(1/2))^(3/2)]
    runData[(momoAttr$PRWEEK < runData$wk) & (runData$wk <= momoAttr$WEEK - r),
         (LPIvar):=(p^(2/3)- zvalue*((4/9)*(p^(1/3))*(od+(stdp^2)*(p)))^(1/2))^(3/2)]

    runData[(momoAttr$PRWEEK < runData$wk) & (runData$wk <= momoAttr$WEEK - r),(UCIvar):=p + zvalue*stdp]
    runData[(momoAttr$PRWEEK < runData$wk) & (runData$wk <= momoAttr$WEEK - r),(LCIvar):=p - zvalue*stdp]

    var <- sprintf("p%s",r)
    runData[,(var):=NULL]
  }
  return(runData)
}

#' @importFrom MASS polr
delayMOMO_2017_12 <- function(aggr, zvalue=1.96) {
  #aggr <- readRDS("test.RDS")
  aggrMaster <- copy(aggr)
  aggr <- aggr[order(aggr$wk),]

  #* Drop obs in week of aggregation
  aggr <- aggr[-nrow(aggr),]

  setDT(aggr)
  aggr[,closedA:=NULL]
  setnames(aggr,"closed","closed0")

  aggr[closed0<0,closed0:=0]
  for(VV in 1:momoAttr$delayCorr){
    newVar <- sprintf("closed%s",VV)
    oldVar <- sprintf("closed%s",VV-1)
    aggr[,(newVar):=get(oldVar)+shift(closed0,VV)]
    #aggr[get(newVar)<0,(newVar):=0]
  }

  namesToKeep1 <- names(aggr)[stringr::str_detect(names(aggr),"^nb")]
  namesToKeep2 <- names(aggr)[stringr::str_detect(names(aggr),"^wk")]
  namesToKeep3 <- names(aggr)[stringr::str_detect(names(aggr),"^WR")]
  namesToKeep4 <- names(aggr)[stringr::str_detect(names(aggr),"^closed")]

  aggr <- aggr[,c(namesToKeep1, "YoDi", "WoDi", namesToKeep2, namesToKeep3, namesToKeep4), with=F]

  #* Set weeks wg´hre nothing has been reported yet to 0
  # NOTE: talk to jens about this, mine is different to his
  for(r in 1:momoAttr$delayCorr){
    var <- sprintf("WR%s",r)
    aggr[is.na(get(var)),(var):=0]
  }

  # * Set the missing triangle
  for(r in 1:momoAttr$delayCorr){
    for(i in r:momoAttr$delayCorr){
      var <- sprintf("WR%s",i)
      aggr[seq_len(.N)==(.N+1-r),(var):=NA]

      var <- sprintf("closed%s",i)
      aggr[seq_len(.N)==(.N+1-r),(var):=NA]
    }
  }

  idVars1 <- names(aggr)[stringr::str_detect(names(aggr),"^wk")]
  idVars2 <- names(aggr)[stringr::str_detect(names(aggr),"^nb")]
  aggr <- melt.data.table(aggr,id.vars=c("YoDi","WoDi",idVars1,idVars2), measure.vars=patterns("closed","WR"), variable.factor=F)
  setnames(aggr,"value1","WR")
  setnames(aggr,"value2","closed")
  setnames(aggr,"variable","delay")
  aggr[,delay:=as.numeric(delay)-1]
  aggr <- aggr[!is.na(WR)]

  aggr[,delay:=factor(delay)]

  # internal
  aggr <- delayMOMO_2017_12_internal(runData=aggr,zvalue=zvalue)
  # end internal

  setorder(aggr,YoDi,WoDi,delay)
  aggr[,rowNum:=1:.N,by=.(YoDi,WoDi)]
  aggr[,maxNum:=.N,by=.(YoDi,WoDi)]
  aggr <- aggr[rowNum==maxNum]
  aggr[,rowNum:=NULL]
  aggr[,maxNum:=NULL]

  aggr[,pred:=as.numeric(NA)]
  aggr[,UPIc:=as.numeric(NA)]
  aggr[,LPIc:=as.numeric(NA)]
  aggr[,UCIc:=as.numeric(NA)]
  aggr[,LCIc:=as.numeric(NA)]

  for(r in 0:momoAttr$delayCorr){
    var <- sprintf("pred%s",r)
    aggr[delay==r,pred:=get(var)]
    #aggr[,(var):=NULL]

    var <- sprintf("UPI%s",r)
    aggr[delay==r,UPIc:=get(var)]
    #aggr[,(var):=NULL]

    var <- sprintf("LPI%s",r)
    aggr[delay==r,LPIc:=get(var)]
    #aggr[,(var):=NULL]

    var <- sprintf("UCI%s",r)
    aggr[delay==r,UCIc:=get(var)]
    #aggr[,(var):=NULL]

    var <- sprintf("LCI%s",r)
    aggr[delay==r,LCIc:=get(var)]
    #aggr[,(var):=NULL]
  }

  #** we generate the CORRECTED number of death
  # maybe this is incorrect?
  aggr[wk<=momoAttr$WEEK2,nbc:=nb]
  aggr[momoAttr$WEEK2 < wk & wk<= momoAttr$WEEK, nbc:=pmax(0,pred,nb)]

  nam <- names(aggrMaster)[stringr::str_detect(names(aggrMaster),"^WR")]
  retval <- merge(aggr,aggrMaster[,c("YoDi","WoDi",nam)],by=c("YoDi","WoDi"))
  retval[,GROUP:=momoAttr$group]

  retval <- as.data.frame(retval)
  return(retval)
}

delayMOMO_original_plus_season <- function(aggr, zvalue=1.96) {
  aggr <- aggr[order(aggr$wk),]

  #* Drop obs in week of aggregation # NEW!!
  aggr <- aggr[-nrow(aggr),]

  # the period of registration for a death week XX
  aggr$closed0 <- aggr$closed + vecshift(aggr$closedA, 1)
  for (VV in 1:attr(aggr, "delayCorr")) {
    aggr[[paste("closed", VV, sep="")]] <- aggr[[paste("closed", VV-1, sep="")]] + vecshift(aggr$closed0, -VV)
  }

  aggr$sin52 <- sin(aggr$WoDi*2*pi/52)
  aggr$cos52 <- cos(aggr$WoDi*2*pi/52)

  aggr$sin26 <- sin(aggr$WoDi*2*pi/26)
  aggr$cos26 <- cos(aggr$WoDi*2*pi/26)

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

    m1 <- glm(as.formula(paste("nb2 ~ sin52 + cos52 + WR", XX, " + Pa", XX, " + wk", sep="")), data=subset(aggr, wk>attr(aggr, "PRWEEK") & wk<attr(aggr, "WEEK2")), family=poisson)
    od <- max(1,sum(m1$weights * m1$residuals^2)/m1$df.r)
    if (od > 1) m1 <- glm(as.formula(paste("nb2 ~ sin52 + cos52 + WR", XX, " + Pa", XX, " + wk", sep="")), data=subset(aggr, wk>attr(aggr, "PRWEEK") & wk<attr(aggr, "WEEK2")), family=quasipoisson)

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

#' @import glmnet
#' @importFrom MASS polr
delayMOMO_richard <- function(aggr, zvalue=1.96) {
  #aggr <- readRDS("test.RDS")
  aggrMaster <- copy(aggr)
  setDT(aggr)
  aggr <- aggr[order(aggr$wk),]

  #* Drop obs in week of aggregation
  aggr <- aggr[-nrow(aggr),]

  modellingWeeks <- momo::momoAttr$PRWEEK:momo::momoAttr$WEEK2
  modellingWeeks <- modellingWeeks[modellingWeeks > min(aggr$wk)+momo::momoAttr$delayCorr*2]

  aggr$sin52 <- sin(aggr$WoDi*2*pi/52)
  aggr$cos52 <- cos(aggr$WoDi*2*pi/52)

  aggr$sin26 <- sin(aggr$WoDi*2*pi/26)
  aggr$cos26 <- cos(aggr$WoDi*2*pi/26)

  aggr[,diff_WR1_minus_WR0:=WR1-WR0]

  for(r in 0:(momo::momoAttr$delayCorr*2)){
    aggr[,(sprintf("WR0_lag%s",r)):=shift(WR0,n=r)]
    aggr[,(sprintf("WR1_lag%s",r)):=shift(WR1,n=r)]
    aggr[,(sprintf("WR2_lag%s",r)):=shift(WR2,n=r)]

    aggr[,(sprintf("diff_WR1_minus_WR0_lag%s",r)):=shift(diff_WR1_minus_WR0,n=r)]
  }

  WR <- c()
  for(r in 0:momo::momoAttr$delayCorr){
    WR <- c(WR,sprintf("WR%s",r))
    WR0Lags <- WR1Lags <- diff_WR1_minus_WR0Lags <- c()
    for(rx in 1:momo::momoAttr$delayCorr) WR0Lags <- c(WR0Lags,sprintf("WR0_lag%s",rx))
    for(rx in 1:momo::momoAttr$delayCorr) WR1Lags <- c(WR1Lags,sprintf("WR1_lag%s",rx))
    for(rx in 1:momo::momoAttr$delayCorr) diff_WR1_minus_WR0Lags <- c(diff_WR1_minus_WR0Lags,sprintf("diff_WR1_minus_WR0_lag%s",rx))
    #WR1Lags <- c(WR1Lags,sprintf("WR1_lag%s",r+1))
    #WR2Lags <- c(WR2Lags,sprintf("WR2_lag%s",r))

    xVars <- c("sin52","cos52","sin26","cos26",WR,WR0Lags,WR1Lags,diff_WR1_minus_WR0Lags)

    x <- as.matrix(aggr[,xVars,with=F])
    xTest <- as.matrix(aggr[aggr$wk %in% modellingWeeks,xVars,with=F])
    y <- aggr[aggr$wk %in% modellingWeeks,]$nb

    s <- ScaleCreate(xTest)

    cvm <- c()
    for(a in 0:10){
      fit <- glmnet::cv.glmnet(x=ScaleApply(xTest,s),y=y,family="gaussian",alpha=a/10)
      cvm <- c(cvm,min(fit$cvm))
    }
    print(cvm)
    chosenAlplha <- which(cvm==min(cvm))/10
    fit <- glmnet::cv.glmnet(x=ScaleApply(xTest,s),y=y,family="gaussian",alpha=chosenAlplha)

    print(r)
    print(coef(fit))
    p <- predict(fit,ScaleApply(x,s))

    aggr[,(sprintf("pred%s",r)):=p]

    UPIvar <- sprintf("UPI%s",r)
    LPIvar <- sprintf("LPI%s",r)

    UCIvar <- sprintf("UCI%s",r)
    LCIvar <- sprintf("LCI%s",r)

    aggr[,(UPIvar):=as.numeric(NA)]
    aggr[,(LPIvar):=as.numeric(NA)]

    aggr[,(UCIvar):=as.numeric(NA)]
    aggr[,(LCIvar):=as.numeric(NA)]
  }

  aggr[,delay:=max(wk)-wk]
  for(r in 0:momo::momoAttr$delayCorr){
    aggr[delay==r,pred:=get(sprintf("pred%s",r))]
    aggr[delay==r,UPIc:=get(sprintf("UPI%s",r))]
    aggr[delay==r,LPIc:=get(sprintf("LPI%s",r))]
    aggr[delay==r,UCIc:=get(sprintf("UCI%s",r))]
    aggr[delay==r,LCIc:=get(sprintf("LCI%s",r))]
  }
  #** we generate the CORRECTED number of death
  print(1)
  aggr[wk<=momoAttr$WEEK2,nbc:=nb]
  aggr[momoAttr$WEEK2 < wk & wk<= momoAttr$WEEK, nbc:=pmax(0,pred,nb)]

  aggr[,GROUP:=momoAttr$group]

  aggr <- as.data.frame(aggr)
  return(aggr)
}

#' @importFrom MASS polr
delayMOMO_richard <- function(aggr, zvalue=1.96) {
  #aggr <- readRDS("test.RDS")
  aggrMaster <- copy(aggr)
  setDT(aggr)
  aggr <- aggr[order(aggr$wk),]

  #* Drop obs in week of aggregation
  aggr <- aggr[-nrow(aggr),]

  modellingWeeks <- momo::momoAttr$PRWEEK:momo::momoAttr$WEEK2
  modellingWeeks <- modellingWeeks[modellingWeeks > min(aggr$wk)+momo::momoAttr$delayCorr*2]

  aggr$sin52 <- sin(aggr$WoDi*2*pi/52)
  aggr$cos52 <- cos(aggr$WoDi*2*pi/52)

  aggr$sin26 <- sin(aggr$WoDi*2*pi/26)
  aggr$cos26 <- cos(aggr$WoDi*2*pi/26)

  aggr[,diff_WR1_minus_WR0:=WR1-WR0]

  for(r in 0:(momo::momoAttr$delayCorr*2)){
    aggr[,(sprintf("WR0_lag%s",r)):=shift(WR0,n=r)]
    aggr[,(sprintf("WR1_lag%s",r)):=shift(WR1,n=r)]
    aggr[,(sprintf("WR2_lag%s",r)):=shift(WR2,n=r)]

    aggr[,(sprintf("diff_WR1_minus_WR0_lag%s",r)):=shift(diff_WR1_minus_WR0,n=r)]
  }

  for(r in 0:momo::momoAttr$delayCorr){
    fit <- glm2::glm2(as.formula(sprintf("nb~WR%s*as.factor(YoDi)",r)),data=aggr[aggr$wk %in% modellingWeeks,])
    summary(fit)
    p <- predict(fit,aggr)

    aggr[,(sprintf("pred%s",r)):=p]

    UPIvar <- sprintf("UPI%s",r)
    LPIvar <- sprintf("LPI%s",r)

    UCIvar <- sprintf("UCI%s",r)
    LCIvar <- sprintf("LCI%s",r)

    aggr[,(UPIvar):=as.numeric(NA)]
    aggr[,(LPIvar):=as.numeric(NA)]

    aggr[,(UCIvar):=as.numeric(NA)]
    aggr[,(LCIvar):=as.numeric(NA)]
  }

  aggr[,delay:=max(wk)-wk]
  for(r in 0:momo::momoAttr$delayCorr){
    aggr[delay==r,pred:=get(sprintf("pred%s",r))]
    aggr[delay==r,UPIc:=get(sprintf("UPI%s",r))]
    aggr[delay==r,LPIc:=get(sprintf("LPI%s",r))]
    aggr[delay==r,UCIc:=get(sprintf("UCI%s",r))]
    aggr[delay==r,LCIc:=get(sprintf("LCI%s",r))]
  }
  #** we generate the CORRECTED number of death
  print(1)
  aggr[wk<=momoAttr$WEEK2,nbc:=nb]
  aggr[momoAttr$WEEK2 < wk & wk<= momoAttr$WEEK, nbc:=pmax(0,pred,nb)]

  aggr[,GROUP:=momoAttr$group]

  aggr <- as.data.frame(aggr)
  return(aggr)
}

#' @importFrom MASS polr
delayMOMO_richard1 <- function(aggr, zvalue=1.96) {
  #aggr <- readRDS("test.RDS")
  aggrMaster <- copy(aggr)
  setDT(aggr)
  aggr <- aggr[order(aggr$wk),]

  #* Drop obs in week of aggregation
  aggr <- aggr[-nrow(aggr),]

  modellingWeeks <- momo::momoAttr$PRWEEK:momo::momoAttr$WEEK2
  modellingWeeks <- modellingWeeks[modellingWeeks > min(aggr$wk)+momo::momoAttr$delayCorr*2]

  aggr$sin52 <- sin(aggr$WoDi*2*pi/52)
  aggr$cos52 <- cos(aggr$WoDi*2*pi/52)

  aggr$sin26 <- sin(aggr$WoDi*2*pi/26)
  aggr$cos26 <- cos(aggr$WoDi*2*pi/26)

  aggr[,diff_WR1_minus_WR0:=WR1-WR0]

  for(r in 0:(momo::momoAttr$delayCorr*2)){
    aggr[,(sprintf("WR0_lag%s",r)):=shift(WR0,n=r)]
    aggr[,(sprintf("WR1_lag%s",r)):=shift(WR1,n=r)]
    aggr[,(sprintf("WR2_lag%s",r)):=shift(WR2,n=r)]

    aggr[,(sprintf("diff_WR1_minus_WR0_lag%s",r)):=shift(diff_WR1_minus_WR0,n=r)]
  }

  for(r in 0:momo::momoAttr$delayCorr){
    fit <- glm2::glm2(as.formula(sprintf("nb~WR%s*as.factor(YoDi)+closed+sin52+cos52",r)),data=aggr[aggr$wk %in% modellingWeeks,])
    summary(fit)
    p <- predict(fit,aggr)

    aggr[,(sprintf("pred%s",r)):=p]

    UPIvar <- sprintf("UPI%s",r)
    LPIvar <- sprintf("LPI%s",r)

    UCIvar <- sprintf("UCI%s",r)
    LCIvar <- sprintf("LCI%s",r)

    aggr[,(UPIvar):=as.numeric(NA)]
    aggr[,(LPIvar):=as.numeric(NA)]

    aggr[,(UCIvar):=as.numeric(NA)]
    aggr[,(LCIvar):=as.numeric(NA)]
  }

  aggr[,delay:=max(wk)-wk]
  for(r in 0:momo::momoAttr$delayCorr){
    aggr[delay==r,pred:=get(sprintf("pred%s",r))]
    aggr[delay==r,UPIc:=get(sprintf("UPI%s",r))]
    aggr[delay==r,LPIc:=get(sprintf("LPI%s",r))]
    aggr[delay==r,UCIc:=get(sprintf("UCI%s",r))]
    aggr[delay==r,LCIc:=get(sprintf("LCI%s",r))]
  }
  #** we generate the CORRECTED number of death
  print(1)
  aggr[wk<=momoAttr$WEEK2,nbc:=nb]
  aggr[momoAttr$WEEK2 < wk & wk<= momoAttr$WEEK, nbc:=pmax(0,pred,nb)]

  aggr[,GROUP:=momoAttr$group]

  aggr <- as.data.frame(aggr)
  return(aggr)
}

#' @importFrom MASS polr
delayMOMO_richard2 <- function(aggr, zvalue=1.96) {
  #aggr <- readRDS("test.RDS")
  aggrMaster <- copy(aggr)
  setDT(aggr)
  aggr <- aggr[order(aggr$wk),]

  #* Drop obs in week of aggregation
  aggr <- aggr[-nrow(aggr),]

  modellingWeeks <- momo::momoAttr$PRWEEK:momo::momoAttr$WEEK2
  modellingWeeks <- modellingWeeks[modellingWeeks > min(aggr$wk)+momo::momoAttr$delayCorr*2]

  aggr$sin52 <- sin(aggr$WoDi*2*pi/52)
  aggr$cos52 <- cos(aggr$WoDi*2*pi/52)

  aggr$sin26 <- sin(aggr$WoDi*2*pi/26)
  aggr$cos26 <- cos(aggr$WoDi*2*pi/26)

  aggr[,diff_WR1_minus_WR0:=WR1-WR0]

  for(r in 0:(momo::momoAttr$delayCorr*2)){
    aggr[,(sprintf("WR0_lag%s",r)):=shift(WR0,n=r)]
    aggr[,(sprintf("WR1_lag%s",r)):=shift(WR1,n=r)]
    aggr[,(sprintf("WR2_lag%s",r)):=shift(WR2,n=r)]

    aggr[,(sprintf("diff_WR1_minus_WR0_lag%s",r)):=shift(diff_WR1_minus_WR0,n=r)]
  }

  for(r in momo::momoAttr$delayCorr:0){
    form <- sprintf("nb~(WR%s)*as.factor(YoDi)+(WR%s)*closed+sin52+cos52",r,r)

    fit <- glm2::glm2(as.formula(form),data=aggr[aggr$wk %in% modellingWeeks,])
    print(summary(fit))
    p <- predict(fit,aggr)

    aggr[,(sprintf("pred%s",r)):=p]

    UPIvar <- sprintf("UPI%s",r)
    LPIvar <- sprintf("LPI%s",r)

    UCIvar <- sprintf("UCI%s",r)
    LCIvar <- sprintf("LCI%s",r)

    aggr[,(UPIvar):=as.numeric(NA)]
    aggr[,(LPIvar):=as.numeric(NA)]

    aggr[,(UCIvar):=as.numeric(NA)]
    aggr[,(LCIvar):=as.numeric(NA)]
  }


  aggr[,delay:=max(wk)-wk]
  for(r in 0:momo::momoAttr$delayCorr){
    aggr[delay==r,pred:=get(sprintf("pred%s",r))]
    aggr[delay==r,UPIc:=get(sprintf("UPI%s",r))]
    aggr[delay==r,LPIc:=get(sprintf("LPI%s",r))]
    aggr[delay==r,UCIc:=get(sprintf("UCI%s",r))]
    aggr[delay==r,LCIc:=get(sprintf("LCI%s",r))]
  }
  #** we generate the CORRECTED number of death
  print(1)
  aggr[wk<=momoAttr$WEEK2,nbc:=nb]
  aggr[momoAttr$WEEK2 < wk & wk<= momoAttr$WEEK, nbc:=pmax(0,pred,nb)]

  aggr[,GROUP:=momoAttr$group]

  aggr <- as.data.frame(aggr)
  return(aggr)
}

delayMOMO <- function(aggr, zvalue=1.96){
  if(opts$delayVersion=="original"){
    delayMOMO_original(aggr=aggr, zvalue=zvalue)
  } else if(opts$delayVersion=="original+season"){
    delayMOMO_original_plus_season(aggr=aggr, zvalue=zvalue)
  } else if(opts$delayVersion=="2017-12"){
    delayMOMO_2017_12(aggr=aggr, zvalue=zvalue)
  } else if(opts$delayVersion=="richard"){
    delayMOMO_richard(aggr=aggr, zvalue=zvalue)
  } else if(opts$delayVersion=="richard1"){
    delayMOMO_richard1(aggr=aggr, zvalue=zvalue)
  } else if(opts$delayVersion=="richard2"){
    delayMOMO_richard2(aggr=aggr, zvalue=zvalue)
  }
}


trimDelayMOMO <- function(aggr) {
  predNames <- names(aggr)[stringr::str_detect(names(aggr),"^pred")]

  ret <- aggr[,c("GROUP", "WoDi", "YoDi", "wk", "wk2", "nb", "nb2", "nbr", "nbc", predNames, "UCIc", "LCIc", "UPIc", "LPIc")]

  # We must preserve the attributes we need
  transferMOMOattributes(ret, aggr)
}

