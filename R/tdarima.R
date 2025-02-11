#' @include utils.R
NULL

STATEBLOCK<-'JD3_SsfStateBlock'


#' Title
#'
#' @param data
#' @param period
#' @param th
#' @param bth
#' @param se
#'
#' @returns
#' @export
#'
#' @examples
tdairline_decomposition<-function(data, th, bth, se=FALSE){
  if (! is.ts(data)) stop("data should be a time series (ts)")
  jmatrix<-.jcall('jdplus/toolkitx/base/r/TimeVaryingArimaModels', 'Ljdplus/toolkit/base/api/math/matrices/Matrix;', 'airlineDecomposition',
            as.numeric(data), as.integer(frequency(data)), as.numeric(th), as.numeric(bth), as.logical(se))
  return (rjd3toolkit::.jd2r_matrix(jmatrix))
}


#' Title
#'
#' @param s
#' @param td
#' @param vartd
#' @param precision
#'
#' @returns
#' @export
#'
#' @examples
tdairline_estimation<-function(s, td=NULL, vartd=FALSE, precision=1e-9){
  if (! is.ts(s)) stop("s should be a time series (ts)")
  arima<-rjd3sts::sarima("arima", frequency(s), c(0,1,1), c(0,1,1), var = 1, fixedvariance = TRUE)
  ltdarima<-rjd3toolkitx::ltd_airline("ltd", length(s), frequency(s), variance = 1, fixedvariance = TRUE)
  if (! is.null(td)){
    if (vartd) tdvar<-1 else tdvar<-0
    vtd<-rjd3sts::reg_td("td", frequency(s), start(s), length(s), groups=td, variance=tdvar, !vartd)
  }else
    vtd<-NULL
  model0<-rjd3sts::model()
  rjd3sts::add(model0, arima)
  if (! is.null(vtd)){
    rjd3sts::add(model0, vtd)
  }
  rslt0<-rjd3sts::estimate(model0, s, marginal = TRUE)
  model1<-rjd3sts::model()
  rjd3sts::add(model1, ltdarima)
  if (! is.null(vtd)){
    rjd3sts::add(model1, vtd)
  }
  rslt1<-rjd3sts::estimate(model1, s, marginal = TRUE)

  parameters<-rjd3toolkit::result(rslt0, "parameters")
  likelihood<-rjd3toolkit::result(rslt0, "likelihood.ll")
  if (! is.null(td)){
    ss<-rjd3sts::smoothed_states(rslt0)
    spos<-rjd3toolkit::result(rslt0, "ssf.cmppos")
    d<-ss[,(1+spos[2]):dim(ss)[2]]
    #    d<--cbind(d,rowSums(d))
    ctd<-ts(d, frequency=frequency(s), start=start(s))
  }else
    ctd<-NULL
  arima_model<-list(
    parameters=parameters,
    likelihood=likelihood,
    td=ctd
  )

  parameters<-rjd3toolkit::result(rslt1, "parameters")
  likelihood<-rjd3toolkit::result(rslt1, "likelihood.ll")
  if (! is.null(td)){
    ss<-rjd3sts::smoothed_states(rslt1)
    spos<-rjd3toolkit::result(rslt1, "ssf.cmppos")
    d<-ss[,(1+spos[2]):dim(ss)[2]]
    #    d<--cbind(d,rowSums(d))
    ctd<-ts(d, frequency=frequency(s), start=start(s))
  }else
    ctd<-NULL
  ltd_arima_model<-list(
    parameters=parameters,
    likelihood=likelihood, td=ctd,
    th=linear(length(s), parameters[1], parameters[2]),
    bth=linear(length(s), parameters[3], parameters[4])

  )
  return (list(sarima=arima_model, ltd_sarima=ltd_arima_model))
}

linear<-function(n,a,b){
  a+(0:(n-1))*((b-a)/(n-1))
}



#' Title
#'
#' @param name
#' @param length
#' @param period
#' @param th0
#' @param th1
#' @param bth0
#' @param bth1
#' @param fixedth
#' @param variance
#' @param fixedvariance
#'
#' @returns
#' @export
#'
#' @examples
ltd_airline<-function(name, length, period, th0=-.6, th1=-.6, bth0=-.6, bth1=-.6, fixedth=FALSE, variance=.01, fixedvariance=FALSE){
  jrslt<-.jcall("jdplus/toolkitx/base/core/tarima/TimeVaryingAirlineEstimation", "Ljdplus/sts/base/core/msts/StateItem;", "ltdAirline", name,
                as.integer(length), as.integer(period), th0, th1, bth0, bth1, fixedth, variance, fixedvariance)
  return(rjd3toolkit::.jd3_object(jrslt, STATEBLOCK))
}



