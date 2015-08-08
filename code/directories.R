# WE CREATE THE DIRECTORIES NEEDED TO STORE THE OUTFILES
# ACCORDING TO THE WEEK UNDER STUDY

glb$VERSION <- "v4-3"

# GLOBAL: WEEK NUMBER to STUDY according to the date of aggregation 
# = complete ISO week, (From monday to Sunday) PRECEDING the date of aggregation 
# for week from Monday to Sunday and aggregation from Monday the week after. 
glb$WOSI <- isoweek(as.Date(with(glb, paste(YEAR, MONTH, DAY, sep="-"))) - 7, "week")
glb$YOSI <- isoweek(as.Date(with(glb, paste(YEAR, MONTH, DAY, sep="-"))) - 7, "year")


glb$OUTPUT <- sprintf("%s/MOMO%s-%s-%s-%s", glb$WDIR, glb$VERSION, glb$country, glb$YOSI, glb$WOSI)
glb$CONTROL <- sprintf("%s/CONTROL-MOMO-%s-%s-%s", glb$OUTPUT, glb$country, glb$YOSI, glb$WOSI)
glb$FINAL <- sprintf("%s/FINAL-MOMO-%s-%s-%s", glb$OUTPUT, glb$country, glb$YOSI, glb$WOSI)
glb$COMPLETE <- sprintf("%s/EUROMOMO-COMPLETE-%s-%s-%s", glb$OUTPUT, glb$country, glb$YOSI, glb$WOSI)
glb$RESTRICTED <- sprintf("%s/EUROMOMO-RESTRICTED-%s-%s-%s", glb$OUTPUT, glb$country, glb$YOSI, glb$WOSI)
glb$CUMULATIVE <- sprintf("%s/CUMULATIVE-MOMO-%s-%s-%s", glb$OUTPUT, glb$country, glb$YOSI, glb$WOSI)
glb$FIT <- sprintf("%s/FIT-MOMO-%s-%s-%s", glb$OUTPUT, glb$country, glb$YOSI, glb$WOSI)


suppressWarnings(   # Suppress the warnings if directories already exist
  lapply(glb[c("OUTPUT", "CONTROL", "FINAL", "COMPLETE", "RESTRICTED", "CUMULATIVE", "FIT")], dir.create)
)
