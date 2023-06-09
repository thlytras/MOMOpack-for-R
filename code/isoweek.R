#' Calculate the ISO week & year for a Date
#'
#' This function takes a vector of Date objects and calculates the week and year
#' according to ISO 8601. It is flexible in its output.
#'
#' @param x A vector of class \code{\link{Date}} (of length >=1)
#' @param type A string (one of "week", "year", "both_text", "both_num" or "matrix")
#'   that determines the kind of output the function returns. See "Return value".
#' @param sep Seperator between year and week, applicable if \code{type="both_text"}
#' @param inv If \code{type="both_text"}, and \code{inv=FALSE}, then year comes before
#'   week. If \code{inv=TRUE}, week comes before year.
#' @param colnames Names for the matrix columns if \code{type="matrix"}
#'
#' @details This function calculates the week number according to ISO 8601. Note that
#'    dates near the start or end of a given year may belong to the previous or next
#'    year respectively, thus the year needs to be calculated too.
#'
#' @return Different according to the function's \code{type} argument. If
#'    \code{"both_num"} (the default), a vector of 6-digit integers is returned, in a
#'    YYYYWW format. If \code{"week"} or \code{"year"}, only the week number or year is
#'    returned, respectively. If \code{"both_text"}, then a character vector of the same
#'    length as \code{x} is returned, containing both the year and week number, seperated
#'    by \code{sep}, and inverted if \code{inv=TRUE}. Finally, if \code{type="matrix"}, both
#'    year and week numbers are returned in a two-column matrix, with the columns named as
#'    in \code{colnames}.
#'
#' @examples
#' isoweek(Sys.Date())
#' isoweek("1980-8-19", "both_text", sep="/", inv=TRUE)
#' isoweek(c("2004-5-31", "2006-6-10", "2007-8-20"), "matrix")
#'
#' @export
isoweek <- function(x, type="both_num", sep="-", inv=FALSE, colnames=c("isoyear","isoweek")) {
  alts=c("week","year","both_text","both_num","matrix")
  if(!(type %in% alts)) stop("Unknown isoweek type requested!")
  x.date<-as.Date(x)
  x.weekday<-as.integer(format(x.date,"%w"))
  x.weekday[x.weekday==0]=7
  x.nearest.thu<-x.date-x.weekday+4
  x.isoyear<-as.integer(substring(x.nearest.thu,1,4)) # Μπορεί οι πρώτες μέρες του χρόνου να ανήκουν (κατά ISO) στην προηγούμενη χρονιά!
  x.isoweek<-(as.integer(x.nearest.thu-as.Date(paste(x.isoyear,"-1-1",sep="")))%/%7)+1
  switch(type,
    week = x.isoweek,
    year = x.isoyear,
    both_text = if (inv) {
      ifelse((is.na(x.isoyear) | is.na(x.isoweek)),NA,paste(x.isoweek,x.isoyear,sep=sep))
    } else {
      ifelse((is.na(x.isoyear) | is.na(x.isoweek)),NA,paste(x.isoyear,x.isoweek,sep=sep))
    },
    both_num = ifelse((is.na(x.isoyear) | is.na(x.isoweek)),NA,x.isoyear*100+x.isoweek),
    matrix = if (inv) {
      `colnames<-`(cbind(x.isoweek, x.isoyear), rev(colnames))
    } else {
      `colnames<-`(cbind(x.isoyear, x.isoweek), colnames)
    }
  )
}
