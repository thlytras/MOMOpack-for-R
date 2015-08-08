# MOMOpack for R version 0.1

# Originally MOMOpack V 4.3 for Stata, 
# created by Bernadette Gergonne, SSI-EpiLife for Euro MOMO. 
# Ported into R by Theodore Lytras <thlytras@gmail.com>


# MOMO TABLES

# we keep $back of corrected data and 6 week before. 

aggr6 <- aggr5[(aggr5$wk > glb$WEEK-glb$back-6 & aggr5$wk <= glb$WEEK), c("wk", "YoDi", "WoDi", "nb2", "nbc", "Pnb", "excess", "zscore", "FLAG")]
aggr6$nbc[aggr6$wk <= glb$WEEK - glb$back] <- NA
aggr6$wk <- NULL
aggr6$nbc <- round(aggr6$nbc)
aggr6$Pnb <- round(aggr6$Pnb)
aggr6$excess <- round(aggr6$excess)
aggr6$zscore <- round(aggr6$zscore, 2)

aggr6 <- aggr6[order(aggr6$YoDi, aggr6$WoDi),]

aggr6 <- cbind(glb$VERSION, glb$GROUP, names(aggr6), t(aggr6))
colnames(aggr6) <- c("Version", "Group", "_varname", paste("v", 1:(ncol(aggr6)-3), sep=""))

aggr6[,3] <- c("Year of death", "Week of death", "Number of deaths registered", 
      "Corrected number of deaths", "Expected number of death (baseline)", 
      "Crude variation around the baseline", "zscore", "Cusum > threshold")
aggr6 <- as.data.frame(aggr6)

if (glb$DEBUG) write.dta(aggr6, sprintf("%s/TABLE-MOMO%s-%s-%s-%s-%s.dta", glb$FINAL, glb$VERSION, glb$country, glb$GROUP, glb$YOSI, glb$WOSI))

# Keep aggr6 for later merging
toBeMerged$table[[length(toBeMerged$table)+1]] <- aggr6
