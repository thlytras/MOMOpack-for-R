# MOMOpack for R version 0.1

# Originally MOMOpack V 4.3 for Stata, 
# created by Bernadette Gergonne, SSI-EpiLife for Euro MOMO. 
# Ported into R by Theodore Lytras <thlytras@gmail.com>


# AGGREGATION

# We make the AGGREGATION by week of death 
groupfile <- groupfile[order(groupfile$YoDi, groupfile$WoDi),]
aggr1 <- aggregate(groupfile[,c("nb", "nb2", "WR0", "WR1", "WR2", "WR3")], by=groupfile[,c("YoDi","WoDi")], sum, na.rm=TRUE)
aggr1 <- aggr1[order(aggr1$YoDi, aggr1$WoDi),]

if (glb$DEBUG) write.dta(aggr1, sprintf("%s/temp.dta", glb$WDIR))


# Creation total number of registration 
# for the numbre of registration, we make an aggregation the week of registration
aggr2 <- aggregate(groupfile[,c("nb")], by=groupfile[,c("YoRi","WoRi")], sum, na.rm=TRUE)
aggr2 <- aggr2[order(aggr2$YoRi, aggr2$WoRi),]
names(aggr2)[3] <- "nbr"
if (glb$DEBUG) write.dta(aggr2, sprintf("%s/temp1.dta", glb$WDIR))


aggr3 <- merge(aggr1, aggr2, all=TRUE, by.x=c("YoDi","WoDi"), by.y=c("YoRi","WoRi"))
aggr3 <- aggr3[order(aggr3$YoDi, aggr3$WoDi),]


if (glb$DEBUG) write.dta(aggr3, sprintf("%s/temp.dta", glb$WDIR))



# COMPLETION OF TIME SERIES
# we ensure that there is no missing weeks in the series
# by merging with a complete time series from 1960 until 2020

time.txt <- seq.Date(as.Date("1961-1-5"), as.Date("2021-1-5"), by="week")
time.txt <- as.data.frame(isoweek(time.txt, "matrix")[,2:1])
names(time.txt) <- c("YoDi", "WoDi")

aggr4 <- merge(time.txt, aggr3, all=TRUE)
aggr4 <- aggr4[order(aggr4$YoDi, aggr4$WoDi),]

if (glb$DEBUG) write.dta(aggr4, sprintf("%s/temp.dta", glb$WDIR))

# We merge with the file containing bank holidays

hfile <- read.dta(paste(glb$INPUTDIR, glb$HFILE, sep="/"))[,c("date", "closed")]

# we make a full time series until the 31-12 of the year under study. 
# (31-12-20xx DOES NOT NEED to be in the bank holiday data base) 
hfile <- merge(data.frame(
    date=seq.Date(from=min(hfile$date), 
		to=as.Date(paste(format(max(hfile$date), "%Y"), 12, 31, sep="-")), 
		by="day")
    ), hfile, all=TRUE)

# we generate the name of the day of the week 
# and set up days off for Saturday and Sunday
# This can be changed for Israel and Arabic countries. 
hfile$NoW = format(hfile$date, "%u") # That's inconsistent. On data1.do, the weekday was 1-7(Mon-Sun). Now it is 0-6(Sun-Sat)
hfile$NoW[hfile$NoW==7] <- 0

hfile$closed[is.na(hfile$closed)] <- 0
hfile$closed[hfile$NoW==0 | hfile$NoW==6] <- 1

hfile$month = format(hfile$date, "%m")
hfile$year = format(hfile$date, "%Y")

hfile[,c("WoWi","YoWi")] <- isoweek(hfile$date, "matrix")

hfile <- hfile[order(hfile$YoWi, hfile$WoWi),]

# for each week we MUST compute the number of day off 
# from monday=0 to the day of aggregation= $NOA
hfile$closedA <- NA
hfile$closedA[hfile$NoW>0 & hfile$NoW<=glb$NOA] <- hfile$closed[hfile$NoW>0 & hfile$NoW<=glb$NOA]

# number of day off (bank holiday) per ISO week
hfile2 <- aggregate(hfile[,c("closed","closedA")], by=hfile[,c("YoWi","WoWi")], sum, na.rm=TRUE)
hfile2 <- hfile2[order(hfile2$YoWi, hfile2$WoWi),]

if (glb$DEBUG) write.dta(hfile2, sprintf("%s/temp2.dta", glb$WDIR))


# merging with the mortality file temporarily saved as temp.dta
aggr5 <- merge(aggr4, hfile2, all=TRUE, by.x=c("YoDi","WoDi"), by.y=c("YoWi","WoWi"))

# we keep only the valid part of the data set
aggr5$YoDi[is.na(aggr5$YoDi) & aggr5$YoDi<=glb$YOAI+1] <- aggr5$YoDi[is.na(aggr5$YoDi) & aggr5$YoDi<=glb$YOAI+1]
aggr5$WoDi[is.na(aggr5$WoDi) & aggr5$YoDi<=glb$YOAI+1] <- aggr5$WoDi[is.na(aggr5$WoDi) & aggr5$YoDi<=glb$YOAI+1]
aggr5 <- aggr5[order(aggr5$YoDi, aggr5$WoDi),]

aggr5$YW <- paste(aggr5$YoDi, aggr5$WoDi, sep="")


aggr5 <- aggr5[,c("YW","WoDi","YoDi","closed","closedA","nb","nb2","nbr", colnames(aggr5)[grep("WR", colnames(aggr5), fixed=TRUE)])]



# we create the time variable of the time series
aggr5 <- aggr5[order(aggr5$YoDi, aggr5$WoDi),]
aggr5$wk <- 1:nrow(aggr5)


# GLOBAL: WEEK NUMBER to STUDY according to the date of aggregation 
# = complete ISO week, (From monday to Sunday) PRECEDING the date of aggregation 
# for week from Monday to Sunday and aggregation from Monday the week after. 
glb$WEEK <- which(aggr5$YoDi==glb$YOAI & aggr5$WoDi==glb$WOAI) - 1

# GLOBAL: Study week and year 
#glb$WOSI <- aggr5$WoDi[glb$WEEK]   # Already calculated at directories.R
#glb$YOSI <- aggr5$YoDi[glb$WEEK]   # Already calculated at directories.R

# GLOBAL: Week number, earlier limit for studying delay 
# accroding to the date of "perfect registration" PR
glb$PRWEEK <- which(aggr5$YoDi==glb$YOPRI & aggr5$WoDi==glb$WOPRI)


# label for ISO weeks

aggr5$labels <- paste(aggr5$YoDi, sprintf("%02.0f", aggr5$WoDi), sep="-")
aggr5$wk2 <- factor(aggr5$labels)


# $WEEK2 is the week number until which we want to model the series
glb$WEEK2 <- glb$WEEK - glb$back


# We clean the data set
aggr5$nb[is.na(aggr5$nb)] <- 0
aggr5$nb2[is.na(aggr5$nb2)] <- 0
aggr5 <- subset(aggr5, wk>=(glb$WEEK - glb$WWW) & wk<=(glb$WEEK+1))

# THIS IS THE AGGREGATED FILE
if (glb$DEBUG) write.dta(aggr5, sprintf("%s/aggregation.dta", glb$WDIR))

