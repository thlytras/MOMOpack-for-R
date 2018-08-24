## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE, include=FALSE-------------------------------------------
#  devtools::load_all()
#  library(data.table)
#  masterData <- fread("/docs/dashboards/data_raw/normomo/FHIDOD2_20180814.txt")
#  masterData[,DoD:=as.Date(as.character(DODS_DATO),format="%Y%m%d")]
#  masterData[,DoR:=as.Date(as.character(ENDR_DATO),format="%Y%m%d")]
#  masterData[,DoB:=as.Date(as.character(FDATO_YYYYMMDD),format="%Y%m%d")]
#  masterData[,age:=floor(as.numeric(difftime(DoD,DoB,units="days"))/365.25)]
#  masterData[is.na(DoR),DoR:=DoD+1]
#  masterData[DoR>="2015-09-03",DoR:=DoR+1]
#  
#  masterData[,ageCat:=cut(age,c(0,4,14,64,200),include.lowest = TRUE)]
#  masterData[,deathWeek:=RAWmisc::WeekN(masterData$DoD)]
#  masterData[,deathYear:=RAWmisc::YearN(masterData$DoD)]
#  MDATA <- as.data.frame(masterData[!is.na(age),c("DoD","DoR","age")])
#  
#  HDATA <- data.frame(readxl::read_excel(system.file("extdata", "bank_holidays.xlsx", package = "normomo"))[,c("date", "closed")])
#  HDATA$date <- as.Date(HDATA$date)
#  
#  SetOpts(
#    DoA=as.Date("2018-08-14"),
#    DoPR=as.Date("2012-01-01"),
#    WStart=1,
#    WEnd=52,
#    country = "Norway",
#    source = "FHI",
#    MDATA = MDATA,
#    HDATA = HDATA,
#    INPUTDIR = tempdir(),
#    WDIR = tempdir(),
#    back = 3,
#    WWW = 290,
#    Ysum = 2018,
#    Wsum = 40,
#    USEglm2 = TRUE,
#    useAUTOMN = TRUE,
#    datesISO = FALSE,
#    plotGraphs = FALSE,
#    delayVersion = "original",
#    MOMOgroups = list("Total"="age >= 0 | is.na(age)"),
#    MOMOmodels = list("Total"="LINE_SIN"))
#  
#  RunMoMo()
#  
#  dataExport$aggr

## ----eval=FALSE, include=FALSE-------------------------------------------
#  aggr <- copy(dataExport$aggr)
#  setDT(aggr)
#  aggr <- aggr[order(aggr$wk),]
#  
#  #* Drop obs in week of aggregation
#  aggr <- aggr[-nrow(aggr),]
#  
#  modellingWeeks <- momoAttr$PRWEEK:momoAttr$WEEK2
#  modellingWeeks <- modellingWeeks[modellingWeeks > min(aggr$wk)+5]
#  
#  aggr$sin52 <- sin(aggr$WoDi*2*pi/52)
#  aggr$cos52 <- cos(aggr$WoDi*2*pi/52)
#  
#  aggr$sin26 <- sin(aggr$WoDi*2*pi/26)
#  aggr$cos26 <- cos(aggr$WoDi*2*pi/26)
#  
#  aggr[,nb_lag1:=shift(nb)]
#  aggr[,nb_lag2:=shift(nb,n=2L)]
#  aggr[,nb_lag3:=shift(nb,n=3L)]
#  
#  fit <- glm(nb~splines::ns(WR1,4)+closedA+YoDi+sin(WoDi*2*pi/52)+cos(WoDi*2*pi/52),data=aggr[aggr$wk %in% modellingWeeks,],family="poisson")
#  
#  x <- as.matrix(aggr[aggr$wk %in% modellingWeeks,c("sin52","cos52","sin26","cos26","WR0","WR1","WR2","WR3","nb_lag1","nb_lag2","nb_lag3")])
#  y <- aggr[aggr$wk %in% modellingWeeks,]$nb
#  
#  s <- RAWmisc::ScaleCreate(x)
#  
#  fit <- glmnet::cv.glmnet(x=RAWmisc::ScaleApply(x,s),y=y)
#  coef(fit)
#  p <- predict(fit,RAWmisc::ScaleApply(x,s))
#  
#  aggr[aggr$wk %in% modellingWeeks,pred:=p]
#  
#  plot(nb~pred,data=aggr[aggr$wk %in% modellingWeeks])
#  fit <- lm(nb~WR3,data=aggr[aggr$wk %in% modellingWeeks])
#  summary(fit)
#  
#  p <- predict(fit,aggr[aggr$wk %in% modellingWeeks])
#  aggr[aggr$wk %in% modellingWeeks,pred2:=p]
#  
#  plot(nb~pred2,data=aggr[aggr$wk %in% modellingWeeks])
#  
#  mean(sqrt((aggr$pred-aggr$nb)^2),na.rm=T)
#  mean(sqrt((aggr$pred2-aggr$nb)^2),na.rm=T)
#  
#  
#  res <- data.table(x,y,p)
#  res[,r:=p-y]
#  plot(r~p,data=res)
#  mean(res$r^2)
#  
#  
#  AIC(fit)
#  fit <- glm(nb~WR0+WR1+closed+sin(WoDi*2*pi/52)+cos(WoDi*2*pi/52),data=aggr[aggr$wk %in% modellingWeeks,],family=gaussian)
#  AIC(fit)
#  
#  summary(fit)
#  plot(fit)
#  runData <- copy(runData)
#  for(r in 0:momoAttr$delayCorr){
#    form <- "nb~closed+YoDi"
#    for(k in 0:r){
#      form <- sprintf("%s+%ssplines::ns(WR%s,3)",k)
#    }
#    fit <- glm(as.formula(form),data=runData[runData$wk %in% modellingWeeks],family="poisson")
#    summary(fit)
#    od <- max(1,sum(fit$weights * fit$residuals^2)/fit$df.r)
#  
#    p <- predict(fit,newdata=runData,type="response")
#    stdp <- predict(fit,newdata=runData,se.fit=T)$se.fit
#  
#    predvar <- sprintf("pred%s",r)
#  
#    UPIvar <- sprintf("UPI%s",r)
#    LPIvar <- sprintf("LPI%s",r)
#  
#    UCIvar <- sprintf("UCI%s",r)
#    LCIvar <- sprintf("LCI%s",r)
#  
#    runData[,(predvar):=p]
#  
#    runData[,(UPIvar):=(p^(2/3)+ zvalue*((4/9)*(p^(1/3))*(od+(stdp^2)*(p)))^(1/2))^(3/2)]
#    runData[,(LPIvar):=(p^(2/3)- zvalue*((4/9)*(p^(1/3))*(od+(stdp^2)*(p)))^(1/2))^(3/2)]
#  
#    runData[,(UCIvar):=p + zvalue*stdp]
#    runData[,(LCIvar):=p - zvalue*stdp]
#  
#    var <- sprintf("p%s",r)
#    runData[,(var):=NULL]
#  }

## ---- fig.show='hold'----------------------------------------------------
plot(1:10)
plot(10:1)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(mtcars, 10))

