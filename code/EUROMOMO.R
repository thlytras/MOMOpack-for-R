# MOMOpack for R version 0.1

# Originally MOMOpack V 4.3 for Stata, 
# created by Bernadette Gergonne, SSI-EpiLife for Euro MOMO. 
# Ported into R by Theodore Lytras <thlytras@gmail.com>


aggr7 <- aggr5

# Country and source identifiers
aggr7$source <- glb$source
aggr7$country <- glb$country
names(aggr7)[names(aggr7)=="GROUP"] <- "group"

aggr7 <- aggr7[,c("Version", "Model", "source", "country", "group", "wk", "WoDi", "YoDi", 
      "wk2", "nb", "nbr", "nbc", "UCIc", "LCIc", "UPIc", "LPIc", "Pnb", "UPIb2", "LCIb", 
      "UCIb", "excess", "UCIe", "LCIe", "zscore", "DOTm", "DOTc", "DOTb", "DOTz", "DOTzm", 
      "Spring", "Autumn", "CUSUM", "FLAG", "k", "ARL0", "h")]
  
# DateoA, YoAi and WoAi gives the time when the data where aggregated,
# which should ideally be the week of output transfer to EuroMOMO

aggr7$DateoA <- as.Date(with(glb, paste(YEAR, MONTH, DAY, sep="-")))
aggr7$YoAi <- glb$YOAI
aggr7$WoAi <- glb$WOAI

aggr7$flag[aggr7$wk>glb$WEEK2 - 7] <- 1
aggr7$flag[aggr7$wk<glb$WEEK2 - 6] <- 0

aggr7 <- aggr7[,c("Version", "Model", "source", "country", "group", "DateoA", "WoAi", "YoAi", "WoDi", "YoDi", "wk2", "UCIc", "LCIc", "nbr", "excess", "zscore", "DOTc", "DOTb", "DOTz", "Pnb", "Spring", "Autumn", "nb", "nbc", "UPIc", "LPIc", "UPIb2", "LCIb", "UCIb", "UCIe", "LCIe", "DOTm", "DOTzm", "CUSUM", "FLAG", "k", "ARL0", "h")]
if (glb$DEBUG) write.dta(aggr7, sprintf("%s/EUROMOMO%s-COMP-%s-%s-%s-%s.dta", glb$COMPLETE, glb$VERSION, glb$country, glb$GROUP, glb$YOSI, glb$WOSI))
toBeMerged$euromomoComplete[[length(toBeMerged$euromomoComplete)+1]] <- aggr7 # Keep for later merging

aggr7 <- aggr7[,c("Version", "Model", "source", "country", "group", "DateoA", "WoAi", "YoAi", "WoDi", "YoDi", "wk2", "zscore", "DOTz", "Spring", "Autumn", "FLAG", "k", "ARL0", "h")]
if (glb$DEBUG) write.dta(aggr7, sprintf("%s/EUROMOMO%s-REST-%s-%s-%s-%s.dta", glb$RESTRICTED, glb$VERSION, glb$country, glb$GROUP, glb$YOSI, glb$WOSI))
toBeMerged$euromomoRestricted[[length(toBeMerged$euromomoRestricted)+1]] <- aggr7 # Keep for later merging

