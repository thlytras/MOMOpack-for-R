# MOMOpack for R version 0.1

# Originally MOMOpack V 4.3 for Stata, 
# created by Bernadette Gergonne, SSI-EpiLife for Euro MOMO. 
# Ported into R by Theodore Lytras <thlytras@gmail.com>


finalOutput <- list()


finalOutput$euromomoComplete <- do.call(rbind, toBeMerged$euromomoComplete)
write.dta(finalOutput$euromomoComplete, 
  sprintf("%s/EUROMOMO%s-COMPLETE-%s-%s-%s.dta", glb$COMPLETE, glb$VERSION, glb$country, glb$YOSI, glb$WOSI))
write.table(finalOutput$euromomoComplete, row.names=FALSE, sep=",", quote=FALSE, na="",
  file=sprintf("%s/EUROMOMO%s-COMPLETE-%s-%s-%s.txt", glb$COMPLETE, glb$VERSION, glb$country, glb$YOSI, glb$WOSI))


finalOutput$euromomoRestricted <- do.call(rbind, toBeMerged$euromomoRestricted)
write.dta(finalOutput$euromomoRestricted, 
  sprintf("%s/EUROMOMO%s-RESTRICTED-%s-%s-%s.dta", glb$RESTRICTED, glb$VERSION, glb$country, glb$YOSI, glb$WOSI))
write.table(finalOutput$euromomoRestricted, row.names=FALSE, sep=",", quote=FALSE, na="",
  file=sprintf("%s/EUROMOMO%s-RESTRICTED-%s-%s-%s.txt", glb$RESTRICTED, glb$VERSION, glb$country, glb$YOSI, glb$WOSI))


finalOutput$table <- do.call(rbind, toBeMerged$table)
write.dta(finalOutput$table, 
  sprintf("%s/TABLE-MOMO%s-%s-%s-%s.dta", glb$FINAL, glb$VERSION, glb$country, glb$YOSI, glb$WOSI))
write.table(finalOutput$table, row.names=FALSE, sep="\t", na="",
  file=sprintf("%s/TABLE-MOMO%s-%s-%s-%s.txt", glb$FINAL, glb$VERSION, glb$country, glb$YOSI, glb$WOSI))


finalOutput$cumChoice <- do.call(rbind, toBeMerged$cumChoice)
write.dta(finalOutput$cumChoice, 
  sprintf("%s/CUMULATIVE-Choice-w%s-w%s-MOMO-%s-%s-%s.dta", glb$CUMULATIVE, glb$WStart, glb$WEnd, glb$country, glb$YOSI, glb$WOSI))
write.table(finalOutput$cumChoice, row.names=FALSE, sep="\t", na="",
  file=sprintf("%s/CUMULATIVE-Choice-w%s-w%s-MOMO-%s-%s-%s.txt", glb$CUMULATIVE, glb$WStart, glb$WEnd, glb$country, glb$YOSI, glb$WOSI))


finalOutput$cumWinter <- do.call(rbind, toBeMerged$cumWinter)
write.dta(finalOutput$cumWinter, 
  sprintf("%s/CUMULATIVE-WINTER-w40-w20-MOMO-%s-%s-%s.dta", glb$CUMULATIVE, glb$country, glb$YOSI, glb$WOSI))
write.table(finalOutput$cumWinter, row.names=FALSE, sep="\t", na="",
  file=sprintf("%s/CUMULATIVE-WINTER-w40-w20-MOMO-%s-%s-%s.txt", glb$CUMULATIVE, glb$country, glb$YOSI, glb$WOSI))


finalOutput$cumSummer <- do.call(rbind, toBeMerged$cumSummer)
write.dta(finalOutput$cumSummer, 
  sprintf("%s/CUMULATIVE-SUMMER-w21-w39-MOMO-%s-%s-%s.dta", glb$CUMULATIVE, glb$country, glb$YOSI, glb$WOSI))
write.table(finalOutput$cumSummer, row.names=FALSE, sep="\t", na="",
  file=sprintf("%s/CUMULATIVE-SUMMER-w21-w39-MOMO-%s-%s-%s.txt", glb$CUMULATIVE, glb$country, glb$YOSI, glb$WOSI))


finalOutput$cumYear <- do.call(rbind, toBeMerged$cumYear)
write.dta(finalOutput$cumYear, 
  sprintf("%s/CUMULATIVE-YEAR-w1-w53-MOMO-%s-%s-%s.dta", glb$CUMULATIVE, glb$country, glb$YOSI, glb$WOSI))
write.table(finalOutput$cumYear, row.names=FALSE, sep="\t", na="",
  file=sprintf("%s/CUMULATIVE-YEAR-w1-w53-MOMO-%s-%s-%s.txt", glb$CUMULATIVE, glb$country, glb$YOSI, glb$WOSI))


finalOutput$cumSeason <- do.call(rbind, toBeMerged$cumSeason)
write.dta(finalOutput$cumSeason, 
  sprintf("%s/CUMULATIVE-SEASON-w27-w26-MOMO-%s-%s-%s.dta", glb$CUMULATIVE, glb$country, glb$YOSI, glb$WOSI))
write.table(finalOutput$cumSeason, row.names=FALSE, sep="\t", na="",
  file=sprintf("%s/CUMULATIVE-SEASON-w27-w26-MOMO-%s-%s-%s.txt", glb$CUMULATIVE, glb$country, glb$YOSI, glb$WOSI))

