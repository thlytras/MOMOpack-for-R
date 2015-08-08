# MOMOpack for R version 0.1

# Originally MOMOpack V 4.3 for Stata, 
# created by Bernadette Gergonne, SSI-EpiLife for Euro MOMO. 
# Ported into R by Theodore Lytras <thlytras@gmail.com>

t1 <- system.time({ # benchmarking

cat("Reading input files and prepping data... ")

mfile <- read.dta(paste(glb$INPUTDIR, glb$MFILE, sep="/"))

# cleaning DK file
mfile <- mfile[,c("DoD", "DoR", "age")]
mfile$DoD <- as.Date(mfile$DoD, origin="1960-1-1")
mfile$DoR <- as.Date(mfile$DoR, origin="1960-1-1")

# DATE VARIABLES RELATED TO AGGREGATION

# Date of aggregation 
mfile$DoA <- as.Date(with(glb, paste(YEAR, MONTH, DAY, sep="-")))

# Name of the day of Aggregation (Monday =1 to Sunday = 7)
mfile$NoA <- format(mfile$DoA, "%u")

# Day Month and Year of Aggregation
mfile$DaoA = format(mfile$DoA, "%d")
mfile$MoA = format(mfile$DoA, "%m")
mfile$YoA = format(mfile$DoA, "%Y")

# ISO week & year of Aggregation
mfile[,c("WoAi","YoAi")] <- isoweek(mfile$DoA, "matrix")

# GLOBAL: We keep year and week of aggregation as global variable
glb$YOAI = mfile$YoAi[1]
glb$WOAI = mfile$WoAi[1]
glb$NOA = mfile$NoA[1]



# DATE OF "PERFECT" REGISTRATION = PR (when data are transmitted smoothly)

mfile$DoPR <- as.Date(with(glb, paste(RYEAR, RMONTH, RDAY, sep="-")))

# Day Month and Year of PR

mfile$DaoPR = format(mfile$DoPR, "%d")
mfile$MoPR = format(mfile$DoPR, "%m")
mfile$YoPR = format(mfile$DoPR, "%Y")

# ISO week & year of Aggregation

mfile[,c("WoPRi", "YoPRi")] <- isoweek(mfile$DoPR, "matrix")

# GLOBAL: We keep year and week of aggregation as global variable

glb$YOPRI = mfile$YoPRi[1]
glb$WOPRI = mfile$WoPRi[1]



# DATE VARIABLES RELATED TO THE DEATH

# Day Month and Year of Death 
mfile$DaoD = format(mfile$DoD, "%d")
mfile$MoD = format(mfile$DoD, "%m")
mfile$YoD = format(mfile$DoD, "%Y")
# ISO week & year of Death
mfile[,c("WoDi","YoDi")] <- isoweek(mfile$DoD, "matrix")



# DATE VARIABLES RELATED TO THE REGISTRATION

# Day Month and Year of Registration
mfile$DaoR = format(mfile$DoR, "%d")
mfile$MoR = format(mfile$DoR, "%m")
mfile$YoR = format(mfile$DoR, "%Y")

# ISO week & year of Registration
mfile[,c("WoRi", "YoRi")] <- isoweek(mfile$DoR, "matrix")
mfile$YoRi[mfile$YoRi <= glb$YOPRI & mfile$WoRi <= glb$WOPRI] <- NA

# Name of the day of Registration (Monday = 1 to Sunday =7)
mfile$NoR <- format(mfile$DoR, "%u") 



# DEATHS
# We generate nb = total number of death known in the series
# that will later be aggregated (indeed ==1 per record...)
mfile$nb <- 1

# To Study the delay
# For an aggregation a specific day, week and year, 
# We know only n2: what is REGISTERED before the date chosen for the aggregation DoR >= DoPR
mfile$nb2 <- NA
mfile$nb2[mfile$DoR <= mfile$DoA] = mfile$nb[mfile$DoR <= mfile$DoA]



# DELAYS
# diff = number of weeks between week of death and week of registration
mfile <- mfile[order(mfile$YoDi, mfile$WoDi, mfile$DoD),]

mfile$diff <- mfile$WoRi - mfile$WoDi
mfile$diff[(mfile$YoRi - mfile$YoDi) == 1] <- mfile$diff[(mfile$YoRi - mfile$YoDi) == 1] + 52 + as.integer(mfile$WoDi[(mfile$YoRi - mfile$YoDi) == 1]==53)
mfile$diff[(mfile$YoRi - mfile$YoDi) == 2] <- mfile$diff[(mfile$YoRi - mfile$YoDi) == 2] + 104 + as.integer(mfile$WoDi[(mfile$YoRi - mfile$YoDi) == 2]==53)
mfile$diff[(mfile$YoRi - mfile$YoDi) == 3] <- mfile$diff[(mfile$YoRi - mfile$YoDi) == 3] + 156 + as.integer(mfile$WoDi[(mfile$YoRi - mfile$YoDi) == 3]==53)
mfile$diff[(mfile$YoRi - mfile$YoDi) == 4] <- mfile$diff[(mfile$YoRi - mfile$YoDi) == 4] + 208 + as.integer(mfile$WoDi[(mfile$YoRi - mfile$YoDi) == 4]==53)
mfile$diff[(mfile$YoRi - mfile$YoDi) == 5] <- mfile$diff[(mfile$YoRi - mfile$YoDi) == 5] + 260 + as.integer(mfile$WoDi[(mfile$YoRi - mfile$YoDi) == 5]==53)


# we must model for each week, what we know at the day of aggregation. 

# After aggregation, WRxx will be = number of death registered after xx FULL weeks among total death occured one week 
# threfore == 1 if diff <= `XX'
# according to what we know the day of aggregation
for (XX in 0:glb$back) {
  mfile[,paste("WR",XX,sep="")] <- with(mfile, as.integer((diff <= XX | (diff == XX+1  & (NoR >=1 & NoR <= NoA) & !is.na(nb2))) & !is.na(nb2)))
}


if (glb$DEBUG) write.dta(mfile, sprintf("%s/data1.dta", glb$WDIR))


}) # End of benchmark

cat("DONE (in "); cat(round(t1[3],1)); cat(" seconds)\n")

