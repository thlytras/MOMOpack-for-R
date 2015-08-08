# MOMOpack for R version 0.1

# Originally MOMOpack V 4.3 for Stata, 
# created by Bernadette Gergonne, SSI-EpiLife for Euro MOMO. 
# Ported into R by Theodore Lytras <thlytras@gmail.com>


glb$DATA <- "nb"
glb$RESI <- "resi"
glb$MODEL <- "Pnb"
glb$ZSCO <- "zscore"

xwk <- with(aggr5, (floor(min(wk)/50)*50):(ceiling(max(wk)/50)*50))
xlabels <- as.character(aggr5$wk2[match(xwk, as.integer(aggr5$wk2))])

# TEST MODEL FIT

png(sprintf("%s/GRAPH-FIT-%s-%s-%s-%s.png", glb$FIT, glb$country, glb$GROUP, glb$YOSI, glb$WOSI),
    width=4400, height=5000, pointsize=21, res=144)
par(oma=c(0,6,10,0))
layout(matrix(c(1:10, rep(0, 2), 11:18, rep(0, 4)),ncol=4, nrow=6))


# Column 1: Graph series total

with(subset(aggr5, wk <= glb$WEEK), {
  plot(y=get(glb$DATA), x=wk, type="l", xlim=range(xwk), bty="l", 
    ylim=c(0, max(get(glb$DATA), na.rm=TRUE)),
    xlab=NA, ylab=NA, xaxt="n", lwd=2, col="navyblue")
  axis(1, at=xwk[grep("-01", xlabels)], labels=xlabels[grep("-01", xlabels)])
  points(y=get(glb$MODEL), x=wk, type="l", lwd=3, col="red4")
})
mtext("MORTALITY SERIES", side=3, cex=1.8, line=3, xpd=TRUE)
mtext("TIME SERIES", side=2, cex=1.5, line=5, xpd=TRUE)

# histogram series
with(subset(aggr5, wk <= glb$WEEK), {
  h <- hist(get(glb$DATA), col="khaki3", border="white", bty="l", xlab=NA, ylab=NA, main=NA)
  curve(dnorm(x, 
	  mean=mean(get(glb$DATA), na.rm=TRUE), 
	  sd=sd(get(glb$DATA), na.rm=TRUE)) * (max(h$counts)/2)*sqrt(2*pi)*sd(get(glb$DATA), na.rm=TRUE), 
      from=min(get(glb$DATA), na.rm=TRUE), to=max(get(glb$DATA), na.rm=TRUE), add=TRUE, col="navyblue")
})
mtext("HISTOGRAMS", side=2, cex=1.5, line=5, xpd=TRUE)

# periodogramme series
with(subset(aggr5, wk<=glb$WEEK), {
  Density <- Mod(fft(get(glb$DATA) - mean(get(glb$DATA))))^2
  Period <- (1/((1:length(wk))-1))*length(wk)
  Period[Inf] <- NA
  plot(y=log(Density)[Period<200], x=Period[Period<200], 
    type="l", ylab=NA, xlab="Period", bty="l", col="navyblue")
  abline(v=c(13.045, 26.09, 52.18), col="red")
})
mtext("PERIODOGRAM", side=2, cex=1.5, line=5, xpd=TRUE)

# according to the prediction
with(subset(aggr5, wk <= glb$WEEK), {
  plot(y=get(glb$DATA), x=get(glb$MODEL), pch=20, xlab=NA, ylab=NA, col="navyblue", bty="l")
  points(y=predict(lm(get(glb$DATA) ~ get(glb$MODEL)), newdata=subset(aggr5, wk <= glb$WEEK)), x=get(glb$MODEL), 
    type="l", col="darkred", lwd=3)
})
mtext("SERIES v MODEL\n(variance stability)", side=2, cex=1.5, line=4, xpd=TRUE)

# autocorrelation and partial autocorrelation
ac1 <- acf(subset(aggr5, wk <= glb$WEEK)[,glb$DATA], lag.max=75, plot=FALSE)
ac1$acf[1] <- 0
plot(ac1, ci.type="ma", main=NA, xlab=NA, ylab=NA, bty="l")
mtext("AUTO-CORRELATION", side=2, cex=1.5, line=5, xpd=TRUE)

ac1 <- pacf(subset(aggr5, wk <= glb$WEEK)[,glb$DATA], lag.max=75, plot=FALSE)
ac1$acf[1] <- 0
plot(ac1, main=NA, xlab=NA, ylab=NA, bty="l")
mtext("PARTIAL\nAUTO-CORRELATION", side=2, cex=1.5, line=4, xpd=TRUE)


# Column 2: Graph series used in the model

with(aggr5, {
  plot(y=get(glb$DATA)[CONDmodel==1], x=wk[CONDmodel==1], type="l", xlim=range(xwk), bty="l", 
    ylim=c(0, max(get(glb$DATA), na.rm=TRUE)),
    xlab=NA, ylab=NA, xaxt="n", lwd=2, col="navyblue")
  axis(1, at=xwk[grep("-01", xlabels)], labels=xlabels[grep("-01", xlabels)])
  points(y=get(glb$MODEL)[CONDmodel==1], x=wk[CONDmodel==1], type="l", lwd=3, col="red4")
})
mtext("DATA USED", side=3, cex=1.8, line=3, xpd=TRUE)

# histogram series used in the model
with(subset(aggr5, CONDmodel==1), {
  h <- hist(get(glb$DATA), col="khaki3", border="white", bty="l", xlab=NA, ylab=NA, main=NA)
  curve(dnorm(x, 
	  mean=mean(get(glb$DATA), na.rm=TRUE), 
	  sd=sd(get(glb$DATA), na.rm=TRUE)) * (max(h$counts)/2)*sqrt(2*pi)*sd(get(glb$DATA), na.rm=TRUE), 
      from=min(get(glb$DATA), na.rm=TRUE), to=max(get(glb$DATA), na.rm=TRUE), add=TRUE, col="navyblue")
})

# periodogramme series used in the model
with(aggr5, {
  nb3 <- get(glb$DATA)
  nb3[CONDmodel==1] <- 0
  Density <- Mod(fft(nb3 - mean(nb3)))^2
  Period <- (1/((1:length(wk))-1))*length(wk)
  Period[Inf] <- NA
  plot(y=log(Density)[Period<200], x=Period[Period<200], 
    type="l", ylab=NA, xlab="Period", bty="l", col="navyblue")
  abline(v=c(13.045, 26.09, 52.18), col="red")
})

with(aggr5, {
  plot(y=get(glb$DATA)[CONDmodel==1], x=get(glb$MODEL)[CONDmodel==1], pch=20, xlab=NA, ylab=NA, col="navyblue", bty="l")
  m1 <- lm(get(glb$DATA)[CONDmodel==1] ~ get(glb$MODEL)[CONDmodel==1])
  lines(x=range(get(glb$MODEL)[CONDmodel==1], na.rm=TRUE), 
    y=coef(m1)[1]+range(get(glb$MODEL)[CONDmodel==1], na.rm=TRUE)*coef(m1)[2], 
    col="darkred", lwd=3)
})


# Column 3: Analysis of residuals

# GRAPHICAL presentation of residuals according to time
with(aggr5, {
  plot(y=get(glb$RESI), x=wk, type="l", xlim=range(xwk), bty="l", 
    xlab=NA, ylab=NA, xaxt="n", lwd=2, col="navyblue")
  axis(1, at=xwk[grep("-01", xlabels)], labels=xlabels[grep("-01", xlabels)])
  points(y=predict(lm(get(glb$RESI) ~ wk), newdata=aggr5), x=wk, type="l", lwd=3, col="red4")
})
mtext("RESIDUALS", side=3, cex=1.8, line=3, xpd=TRUE)

# histogram
with(aggr5, {
  resi <- get(glb$RESI)[(WoDi>glb$SPRING[1] & WoDi<glb$SPRING[2]) | (WoDi>glb$AUTUMN[1] & WoDi<glb$AUTUMN[2])]
  h <- hist(resi, col="khaki3", border="white", bty="l", xlab=NA, ylab=NA, main=NA)
  curve(dnorm(x, 
	  mean=mean(resi, na.rm=TRUE), 
	  sd=sd(resi, na.rm=TRUE)) * (max(h$counts)/2)*sqrt(2*pi)*sd(resi, na.rm=TRUE), 
      from=min(resi, na.rm=TRUE), to=max(resi, na.rm=TRUE), add=TRUE, col="navyblue")
})

# periodogramme residuals
with(aggr5, {
  Density <- Mod(fft(NAto0(get(glb$RESI)) - mean(get(glb$RESI), na.rm=TRUE)))^2
  Period <- (1/((1:length(wk))-1))*length(wk)
  Period[Inf] <- NA
  plot(y=log(Density)[Period<200], x=Period[Period<200], 
    type="l", ylab=NA, xlab="Period", bty="l", col="navyblue")
  abline(v=c(13.045, 26.09, 52.18), col="red")
})

# according to the prediction
with(aggr5, {
  plot(y=get(glb$RESI), x=get(glb$MODEL), pch=20, xlab=NA, ylab=NA, col="navyblue", bty="l")
  m1 <- lm(get(glb$RESI) ~ get(glb$MODEL))
  lines(x=range(get(glb$MODEL), na.rm=TRUE), 
    y=coef(m1)[1]+range(get(glb$MODEL), na.rm=TRUE)*coef(m1)[2], 
    col="darkred", lwd=3)
})

# analysis autocorrelation residuals
ac1 <- acf(NAto0(aggr5[,glb$RESI]), lag.max=75, plot=FALSE)
ac1$acf[1] <- 0
plot(ac1, ci.type="ma", main=NA, xlab=NA, ylab=NA, bty="l")

ac1 <- pacf(NAto0(aggr5[,glb$RESI]), lag.max=75, plot=FALSE)
ac1$acf[1] <- 0
plot(ac1, main=NA, xlab=NA, ylab=NA, bty="l")


# Column 4: plot Z-score

with(aggr5, {
  zsco <- get(glb$ZSCO)
  zsco[is.na(CONDmodel)] <- NA
  plot(y=zsco, x=wk, type="l", xlim=range(xwk), bty="l", 
    xlab=NA, ylab=NA, xaxt="n", lwd=2, col="navyblue")
  axis(1, at=xwk[grep("-01", xlabels)], labels=xlabels[grep("-01", xlabels)])
  abline(h=0, lwd=3, col="red4")
})
mtext("Z-SCORES SERIES", side=3, cex=1.8, line=3, xpd=TRUE)

# Histogram Z-score
with(aggr5, {
  zsco <- get(glb$ZSCO)[CONDmodel==1]
  h <- hist(zsco, col="khaki3", border="white", bty="l", xlab=NA, ylab=NA, main=NA)
  curve(dnorm(x, 
	  mean=mean(zsco, na.rm=TRUE), 
	  sd=sd(zsco, na.rm=TRUE)) * (max(h$counts)/2)*sqrt(2*pi)*sd(zsco, na.rm=TRUE), 
      from=min(zsco, na.rm=TRUE), to=max(zsco, na.rm=TRUE), add=TRUE, col="navyblue")
})


mtext(sprintf("Analysis Model Fit\n%s-%s years-%s-%s", glb$country, glb$GROUP, glb$YOSI, glb$WOSI),
    side=3, outer=TRUE, cex=2, line=3)

dev.off()
