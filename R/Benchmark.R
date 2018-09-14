#' Benchmark different delay corrections.
#' @import data.table
#' @export BenchmarkDelay
BenchmarkDelay <- function(){
  # remove some of the more pointless functions for a benchmark
  wdir <- opts$WDIR
  opts$WDIR <- tempdir()
  opts$verbose <- F

  stack <- expand.grid(opts$delayVersionAvailable,stringsAsFactors = F)
  names(stack) <- c("delayVersion")

  res <- vector("list",length=nrow(stack))
  for(i in 1:nrow(stack)){
    opts$delayVersion <- stack$delayVersion[i]

    print(stack[i,])

    RunMoMo()

    trueRes <- data.table::rbindlist(dataExport$toSave)
    predNames <- names(trueRes)[stringr::str_detect(names(trueRes),"^predzscore")]

    trueRes <- trueRes[,c("GROUP","YoDi","WoDi","excess","zscore",predNames),with=F]

    res[[i]] <- dataExport$aggr_fullDelay
    nrow(res[[i]])
    res[[i]] <- merge(res[[i]],trueRes,by=c("GROUP","YoDi","WoDi"))
    nrow(res[[i]])
    res[[i]]$delayVersion <- opts$delayVersion
  }

  opts$WDIR <- wdir

  res <- rbindlist(res,fill=T)
  modellingWeeks <- momo::momoAttr$PRWEEK:momo::momoAttr$WEEK2

  return(res[wk %in% modellingWeeks])
}
