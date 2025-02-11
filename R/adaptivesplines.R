#' @include utils.R
NULL

APSPLINE<-'JD3_ADAPTIVEPERIODICSPLINE'
APSPLINES<-'JD3_ADAPTIVEPERIODICSPLINES'


#' Title
#'
#' @param x
#' @param y
#' @param period
#' @param nknots
#' @param knots
#' @param order
#' @param lambda
#' @param maxiter
#' @param precision
#' @param threshold
#'
#' @return
#' @export
#'
#' @examples
periodic_adaptive_spline<-function(x, y, period, nknots, knots=NULL, order=4, lambda=0.1, maxiter=20, precision=0.001, threshold=0.98){
  if (is.null(knots)){
    if (nknots == 0)
      stop('Invalid parameters. nknots should be greater than 0 or knots should be defined')
    p<-period/nknots
    knots<-seq(0,period-p/2, p)
  }
  jrslt<-.jcall('jdplus/toolkitx/base/r/AdaptivePeriodicSplines', 'Ljdplus/toolkitx/base/r/AdaptivePeriodicSplines$AdaptivePSpline;', 'aspline',
        as.numeric(x), as.numeric(y), as.numeric(knots), as.numeric(period), as.integer(order),
        as.numeric(lambda), as.integer(maxiter), as.numeric(precision), as.numeric(threshold))
  return(rjd3toolkit::.jd3_object(jrslt, APSPLINE, TRUE))

}

#' Title
#'
#' @param x
#' @param y
#' @param period
#' @param nknots
#' @param knots
#' @param order
#' @param maxiter
#' @param precision
#' @param threshold
#' @param lambda0
#' @param lambda1
#' @param dlambda
#' @param minKnots
#' @param criterion
#'
#' @return
#' @export
#'
#' @examples
periodic_adaptive_splines<-function(x, y, period, nknots, knots=NULL, order=4, maxiter=20, precision=0.001, threshold=0.98,
                                    lambda0=0, lambda1=10, dlambda=0.025, minKnots=0, criterion="AIC"){
  if (is.null(knots)){
    if (nknots == 0)
      stop('Invalid parameters. nknots should be greater than 0 or knots should be defined')
    p<-period/nknots
    knots<-seq(0,period-p/2, p)
  }
  jrslt<-.jcall('jdplus/toolkitx/base/r/AdaptivePeriodicSplines', 'Ljdplus/toolkitx/base/r/AdaptivePeriodicSplines$AdaptivePSplines;', 'asplines',
                as.numeric(x), as.numeric(y), as.numeric(knots), as.numeric(period), as.integer(order), as.integer(maxiter),
                as.numeric(precision), as.numeric(threshold), as.numeric(lambda0), as.numeric(lambda1), as.numeric(dlambda), as.integer(minKnots), as.character(criterion) )
  return(rjd3toolkit::.jd3_object(jrslt, APSPLINES, TRUE))

}

