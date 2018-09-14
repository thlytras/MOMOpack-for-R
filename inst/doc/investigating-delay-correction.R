## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=TRUE, include=FALSE--------------------------------------------
devtools::load_all()
library(data.table)
library(ggplot2)

# Functions
TP <- function(var){
  sum(var=="TP",na.rm=T)
}

FP <- function(var){
  sum(var=="FP",na.rm=T)
}

TN <- function(var){
  sum(var=="TN",na.rm=T)
}

FN <- function(var){
  sum(var=="FN",na.rm=T)
}

PPV <- function(var){
  return(TP(var)/(TP(var)+FP(var)))
}

NPV <- function(var){
  return(TN(var)/(TN(var)+FN(var)))
}

SENS <- function(var){
  return(TP(var)/(TP(var)+FN(var)))
}

SPEC <- function(var){
  return(TN(var)/(TN(var)+FP(var)))
}


masterData <- fread("/docs/dashboards/data_raw/normomo/FHIDOD2_20180814.txt")
masterData[,DoD:=as.Date(as.character(DODS_DATO),format="%Y%m%d")]
masterData[,DoR:=as.Date(as.character(ENDR_DATO),format="%Y%m%d")]
masterData[,DoB:=as.Date(as.character(FDATO_YYYYMMDD),format="%Y%m%d")]
masterData[,age:=floor(as.numeric(difftime(DoD,DoB,units="days"))/365.25)]
masterData[is.na(DoR),DoR:=DoD+1]
masterData[DoR>="2015-09-03",DoR:=DoR+1]

masterData[,ageCat:=cut(age,c(0,4,14,64,200),include.lowest = TRUE)]
masterData[,deathWeek:=RAWmisc::WeekN(masterData$DoD)]
masterData[,deathYear:=RAWmisc::YearN(masterData$DoD)]
MDATA <- as.data.frame(masterData[!is.na(age),c("DoD","DoR","age")])

HDATA <- data.frame(readxl::read_excel(system.file("extdata", "bank_holidays.xlsx", package = "normomo"))[,c("date", "closed")])
HDATA$date <- as.Date(HDATA$date)

SetOpts(
  DoA=as.Date("2018-08-14"),
  DoPR=as.Date("2012-01-01"),
  WStart=1,
  WEnd=52,
  country = "Norway",
  source = "FHI",
  MDATA = MDATA,
  HDATA = HDATA,
  INPUTDIR = tempdir(),
  WDIR = tempdir(),
  back = 7,
  WWW = 290,
  Ysum = 2018,
  Wsum = 40,
  USEglm2 = TRUE,
  useAUTOMN = TRUE,
  datesISO = FALSE,
  plotGraphs = FALSE,
  delayVersion = "original",
  MOMOgroups = list("Total"="age >= 0 | is.na(age)"),
  MOMOmodels = list("Total"="LINE_SIN"))

x <- BenchmarkDelay()

d <- x[delayVersion %in% c("original","original+season"),c(
  "YoDi","WoDi","delayVersion","nb","zscore",
  "predzscore0","predzscore1","predzscore2","predzscore3","predzscore4","predzscore5",
  "pred0","pred1","pred2","pred3","pred4","pred5",
  "WR0","WR1","WR2","WR3","WR4","WR5"
  )]
setnames(d,c("predzscore0","predzscore1","predzscore2","predzscore3","predzscore4","predzscore5"),c("pzscore0","pzscore1","pzscore2","pzscore3","pzscore4","pzscore5"))
d <- melt.data.table(d[delayVersion=="original"],id.vars=c("YoDi","WoDi","delayVersion","nb","zscore"),measure=patterns("^pzscore","^pred","^WR"))
setnames(d,c("value1","value2","value3"),c("pzscore","pnb","WR"))
d[,delay:=as.numeric(variable)-1]

d[,error_pnb:=pnb-nb]

d[,truePos:=zscore>2]
d[,testPos:=pzscore>2]

d[,test_results:=as.character(NA)]
d[truePos==TRUE & testPos==TRUE, test_results:="TP"]
d[truePos==FALSE & testPos==FALSE, test_results:="TN"]
d[truePos==FALSE & testPos==TRUE, test_results:="FP"]
d[truePos==TRUE & testPos==FALSE, test_results:="FN"]


## ----echo=TRUE, fig.height=5, fig.width=7, message=FALSE, warning=FALSE----
q <- ggplot(d,aes(x=delay,y=WR/nb*100,group=delay))
q <- q + geom_boxplot()
q <- q + scale_x_continuous("Lag")
q <- q + scale_y_continuous("Percent of deaths recorded")
q

## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------
d[,.(
  corr_nb=cor(nb,pnb,use="pairwise.complete.obs"),
  corr_zscore=cor(zscore,pzscore,use="pairwise.complete.obs")
),keyby=.(delayVersion,delay)]

## ----echo=TRUE, fig.height=5, fig.width=7, message=FALSE, warning=FALSE----
q <- ggplot(d,aes(x=pzscore,y=zscore))
q <- q + geom_point()
q <- q + facet_grid(delayVersion~delay,scales="free")
q <- q + scale_x_continuous("Z-score using predicted number of deaths at different lags")
q <- q + scale_y_continuous("True Z-score")
q

## ----echo=TRUE, fig.height=5, fig.width=7, message=FALSE, warning=FALSE----
q <- ggplot(d,aes(x=pnb,y=nb))
q <- q + geom_point()
q <- q + facet_grid(delayVersion~delay,scales="free")
q <- q + scale_x_continuous("Predicted number of deaths at different lags")
q <- q + scale_y_continuous("True number of recorded deaths")
q

## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------
xtabs(~d$truePos+d$testPos+d$delay)

## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------
results <- d[,.(
  tp=TP(test_results),
  tn=TN(test_results),
  fp=FP(test_results),
  fn=FN(test_results),
  ppv=RAWmisc::Format(PPV(test_results)*100,0),
  npv=RAWmisc::Format(NPV(test_results)*100,0),
  sens=RAWmisc::Format(SENS(test_results)*100,0),
  spec=RAWmisc::Format(SPEC(test_results)*100,0)
),keyby=.(delay)]

print(results)

## ----echo=TRUE, fig.height=5, fig.width=7, message=FALSE, warning=FALSE----
q <- ggplot(d,aes(x=zscore,y=error_pnb))
q <- q + geom_point()
q <- q + facet_grid(delayVersion~delay,scales="free")
q <- q + scale_x_continuous("True Z-Score")
q <- q + scale_y_continuous("Bias (predicted number of deaths minus actual number of deaths)")
q

