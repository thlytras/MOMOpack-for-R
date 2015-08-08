# MOMOpack for R version 0.1

# Originally MOMOpack V 4.3 for Stata, 
# created by Bernadette Gergonne, SSI-EpiLife for Euro MOMO. 
# Ported into R by Theodore Lytras <thlytras@gmail.com>


aggr5 <- cbind(aggr5, lspline(aggr5$wk, nknots=2, names=c("Swk1", "Swk2", "Swk3")))


# DEFINITION OF SPRING AND AUTUMN
glb$SPRING <- c(15, 26)
glb$AUTUMN <- c(36, 45)


# data management
aggr5$season <- NA
for (YYY in unique(aggr5$YoDi)) {
  aggr5$season[aggr5$WoDi > 26 & aggr5$YoDi == YYY] <- YYY
  aggr5$season[aggr5$WoDi <= 26 & aggr5$YoDi == YYY] <- YYY - 1
}


aggr5 <- aggr5[order(aggr5$season, aggr5$YoDi, aggr5$WoDi),]
aggr5$Sweek <- unlist(lapply(table(aggr5$season), function(x)1:x))
aggr5$sin <- sin(2*pi*aggr5$wk/52.18)
aggr5$cos <- cos(2*pi*aggr5$wk/52.18)


# generation model and indicators of excess

aggr5$excess <- NA
aggr5$zscore <- NA
aggr5$DOTm <- NA
aggr5$DOTc <- NA
aggr5$DOTb <- NA
aggr5$DOTz <- NA
aggr5$DOTzm <- NA
aggr5$UCIe <- NA
aggr5$LCIe <- NA


# we do not use data before WWW week before the Drop date
# for modelling baseline without the influence of A-FLU
glb$DROP <- aggr5$wk[aggr5$YoDi == glb$Ydrop & aggr5$WoDi == glb$Wdrop]


# Conditions for modelling
# We remove "winter" and "summer"...actually we keep spring and autumn
aggr5$COND3[(aggr5$WoDi>glb$SPRING[1] & aggr5$WoDi<glb$SPRING[2]) | (aggr5$WoDi>glb$AUTUMN[1] & aggr5$WoDi<glb$AUTUMN[2])] <- 1
# We remove the previous weeks if there is no unusual excess observed
aggr5$COND4 <- as.integer(aggr5$YoDi<glb$Ydrop | (aggr5$YoDi==glb$Ydrop & aggr5$WoDi<glb$Wdrop))
#we remove the period with delay 
aggr5$COND5[aggr5$wk < glb$WEEK2] <- 1
# we keep only valid historical data
aggr5$COND6[(aggr5$wk > glb$WEEK - glb$WWW) & (aggr5$wk <= glb$WEEK)] <- 1

aggr5$CONDmodel[with(aggr5, COND3==1 & COND4==1 & COND5==1 & COND6==1)] <- 1
aggr5$CONDpred[aggr5$COND6 == 1] <- 1



# DETECTION OF ANY EXCESS adapted Serfling MODEL

# Use glm2() if package glm2 is available, in order to improve convergence properties
# (This is equivalent to the option irls in stata glm)
glmToUse <- c("glm", "glm2")[(glb$USEglm2 && suppressWarnings(require(glm2, quietly=TRUE)))+1]

if (unique(aggr5$GROUP) %in% glb$LINE) {
  m1 <- do.call(glmToUse, list(nbc ~ wk, data=subset(aggr5, CONDmodel==1), family=poisson))
  od <- max(1,sum(m1$weights * m1$residuals^2)/m1$df.r)
  if (od > 1) m1 <- do.call(glmToUse, list(nbc ~ wk, data=subset(aggr5, CONDmodel==1), family=quasipoisson))
  aggr5$Model <- "LINE"
}

if (unique(aggr5$GROUP) %in% glb$SPLINE) {
  m1 <- do.call(glmToUse, list(nbc ~ Swk1 + Swk2 + Swk3, data=subset(aggr5, CONDmodel==1), family=poisson))
  od <- max(1,sum(m1$weights * m1$residuals^2)/m1$df.r)
  if (od > 1) m1 <- do.call(glmToUse, list(nbc ~ Swk1 + Swk2 + Swk3, data=subset(aggr5, CONDmodel==1), family=quasipoisson))
  aggr5$Model <- "SPLINES"
}

if (unique(aggr5$GROUP) %in% glb$LINE_SIN) {
  m1 <- do.call(glmToUse, list(nbc ~ sin + cos + wk, data=subset(aggr5, CONDmodel==1), family=poisson))
  od <- max(1,sum(m1$weights * m1$residuals^2)/m1$df.r)
  if (od > 1) m1 <- do.call(glmToUse, list(nbc ~ sin + cos + wk, data=subset(aggr5, CONDmodel==1), family=quasipoisson))
  aggr5$Model <- "LINES_SIN"
}

if (unique(aggr5$GROUP) %in% glb$SPLINE_SIN) {
  m1 <- do.call(glmToUse, list(nbc ~ sin + cos + Swk1 + Swk2 + Swk3, data=subset(aggr5, CONDmodel==1), family=poisson))
  od <- max(1,sum(m1$weights * m1$residuals^2)/m1$df.r)
  if (od > 1) m1 <- do.call(glmToUse, list(nbc ~ sin + cos + Swk1 + Swk2 + Swk3, data=subset(aggr5, CONDmodel==1), family=quasipoisson))
  aggr5$Model <- "SPLINES_SIN"
}

aggr5$Pnb <- NA
aggr5$resi <- NA
aggr5$stdp <- NA
aggr5$Pnb[which(aggr5$CONDpred==1)] <- predict(m1, aggr5[which(aggr5$CONDpred==1),], type="response")
aggr5$resi[which(aggr5$CONDmodel==1)] <- aggr5$nbc[which(aggr5$CONDmodel==1)] - predict(m1, aggr5[which(aggr5$CONDmodel==1),], type="response")
aggr5$stdp[which(aggr5$CONDpred==1)] <- predict(m1, aggr5[which(aggr5$CONDpred==1),], se.fit=TRUE)$se.fit


for(Z in seq(2,20,2)) {
  aggr5[[paste("UPIb",Z,sep="")]] <- (aggr5$Pnb^(2/3)+ Z*((4/9)*(aggr5$Pnb^(1/3))*(od+(aggr5$stdp^2)*(aggr5$Pnb)))^(1/2))^(3/2)

  # We drop the variables UPI if they are not crossed by the data
  if (Z>2 && sum(aggr5$nbc > aggr5[[paste("UPIb",Z,sep="")]], na.rm=TRUE)==0) {
    aggr5[[paste("UPIb",Z,sep="")]] <- NULL
    break
  }
}

aggr5$zscore <- (aggr5$nbc^(2/3) - aggr5$Pnb^(2/3)) / ((4/9)*(aggr5$Pnb^(1/3))*(od+aggr5$Pnb*(aggr5$stdp^2)))^(1/2)

aggr5$UCIb <- aggr5$Pnb + glb$zvalue*aggr5$stdp
aggr5$LCIb <- aggr5$Pnb - glb$zvalue*aggr5$stdp

# Variations around the baseline
aggr5$excess <- aggr5$nbc - aggr5$Pnb
aggr5$UCIe <- aggr5$UCIc - aggr5$Pnb
aggr5$LCIe <- aggr5$LCIc - aggr5$Pnb


# To better observe the week under study in the graph, we create new variables
aggr5$DOTb[which(aggr5$wk>=glb$WEEK2 & aggr5$wk<=glb$WEEK)] <- aggr5$Pnb[which(aggr5$wk>=glb$WEEK2 & aggr5$wk<=glb$WEEK)]
aggr5$DOTc[which(aggr5$wk>=glb$WEEK2 & aggr5$wk<=glb$WEEK)] <- aggr5$nbc[which(aggr5$wk>=glb$WEEK2 & aggr5$wk<=glb$WEEK)]
aggr5$DOTz[which(aggr5$wk>=glb$WEEK2 & aggr5$wk<=glb$WEEK)] <- aggr5$zscore[which(aggr5$wk>=glb$WEEK2 & aggr5$wk<=glb$WEEK)]
aggr5$DOTm[which(aggr5$COND3==1 & aggr5$COND4==1 & aggr5$COND5==1)] <- aggr5$nbc[which(aggr5$COND3==1 & aggr5$COND4==1 & aggr5$COND5==1)]
aggr5$DOTzm[which(aggr5$COND3==1 & aggr5$COND4==1 & aggr5$COND5==1)] <- aggr5$zscore[which(aggr5$COND3==1 & aggr5$COND4==1 & aggr5$COND5==1)]

aggr5 <- aggr5[aggr5$wk<=glb$WEEK+1,]

aggr5$Version <- glb$VERSION
aggr5$Spring <- paste("(WoDi>", glb$SPRING[1], " & WoDi<", glb$SPRING[2], ")", sep="")
aggr5[,c("Autumn","Automn")[glb$useAUTOMN+1]] <- paste("(WoDi>", glb$AUTUMN[1], " & WoDi<", glb$AUTUMN[2], ")", sep="")


# CUSUM

# DEFINITION OF REFERENCE PARAMETER

# k is the reference value or allowance parameter, it is set up here /// 
# to detect a 1.5 SD shift over 3 weeks => 0.5 SD / week for 3 weeks
# in that case, the best k would be 0.5 SD/2 = .25
aggr5$k <- 0.25 


# ARL0: we consider an acceptable level of 5 % of false alarm, 
# considering a risk similar to an alpha risk
# which lead to and ARL0 of 20 weeks
# the meaning is that on a controled processed, 
# an alarm will be raised due to random variation every 20 weeks. 
aggr5$ARL0 = 20

# h is computed according to ARL0
# h is the decision limit, computed according to Rogerson A. et al. 2003 

aggr5$h <- (((aggr5$ARL0+4)/(aggr5$ARL0+2))*log((aggr5$ARL0/2)+1))-1.16666


# CUM is the week to start and set the CUSUM at 0 
glb$CUM <- aggr5$wk[aggr5$YoDi==glb$Ysum & aggr5$WoDi==glb$Wsum]
if (length(glb$CUM)==0) glb$CUM <- aggr5$wk[1]

aggr5$CUSUM <- 0
cu <- 0
zsc <- aggr5$zscore
zsc[is.na(zsc)] <- 0
zsc <- zsc - aggr5$k
ti <- system.time({
  for (i in which(aggr5$wk>=glb$CUM & aggr5$wk <= glb$WEEK)) {
    cu <- max(0, cu + zsc[i])
    aggr5$CUSUM[i] <- cu
  }
})
aggr5$FLAG[aggr5$CUSUM>=aggr5$h] <- 1


# we save the FINAL DATA SET

aggr5 <- aggr5[,c("Version", "Model", "GROUP", "WoDi", "YoDi", "wk", "wk2", 
  "season", "Sweek", "nb", "nb2", "nbr", "nbc", "UCIc", "LCIc", "UPIc", "LPIc", "Pnb", 
  names(aggr5)[grep("UPIb", names(aggr5), fixed=TRUE)], "LCIb", "UCIb", "excess", 
  "UCIe", "LCIe", "resi", "zscore", "DOTm", "DOTc", "DOTb", "DOTz", "DOTzm",
  "COND6", "CONDmodel", "Spring", c("Autumn","Automn")[glb$useAUTOMN+1], 
  "CUSUM", "FLAG", "k", "ARL0", "h")]

write.dta(aggr5, sprintf("%s/DATA-MOMO%s-%s-%s-%s-%s.dta", glb$FINAL, glb$VERSION, glb$country, glb$GROUP, glb$YOSI, glb$WOSI))
write.table(aggr5, row.names=FALSE, sep="\t", na="", file=sprintf("%s/DATA-MOMO%s-%s-%s-%s-%s.txt", glb$FINAL, glb$VERSION, glb$country, glb$GROUP, glb$YOSI, glb$WOSI))

