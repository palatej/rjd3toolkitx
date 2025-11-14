#' @include utils.R
NULL

#' Title
#'
#' @param y
#' @param arima
#' @param X
#' @param mean
#' @param outliers
#' @param filter
#' @param mad
#'
#' @returns
#' @export
#'
#' @examples
#'
regarima_outlier<-function(y, arima, X=NULL, mean=FALSE, outliers=c("ao", "ls", "tc:0.7"), filter=c("fast", "fastc", "ansley", "kalman", "x12", "ljungbox"), mad = TRUE){
  filter<-match.arg(filter)
  jarima<-.jd3_arima(arima)
  jrslt<-.jcall('jdplus/toolkitx/base/r/Outliers', 'Ljdplus/toolkit/base/api/math/matrices/Matrix;', 'regarimaOutlier',
                 as.numeric(y), .jcast(jarima,'jdplus/toolkit/base/core/arima/IArimaModel'), mean, rjd3toolkit::.r2jd_matrix(X),
                .jarray(outliers), filter, as.logical(mad))

  return (rjd3toolkit::.jd2r_matrix(jrslt))
}

#' Title
#'
#' @param y
#' @param arima
#' @param X
#' @param mean
#' @param critical.value
#' @param outliers
#' @param filter
#' @param mad
#'
#' @returns
#' @export
#'
#' @examples
regarima_outliers<-function(y, arima, X=NULL, mean=FALSE, critical.value = 0, outliers=c("ao", "ls", "tc:0.7"), filter=c("fast", "fastc", "ansley", "kalman", "x12", "ljungbox"), mad = TRUE){
  filter<-match.arg(filter)
  jarima<-rjd3toolkit::.r2jd_sarima(arima)
  jrslt<-.jcall('jdplus/toolkitx/base/r/Outliers', 'Ljdplus/toolkitx/base/r/Outliers$Results;', 'regarimaOutliers',
                as.numeric(y), jarima, mean, rjd3toolkit::.r2jd_matrix(X),
                as.numeric(critical.value), .jarray(outliers), filter, as.logical(mad))
  joutliers<-.jcall(jrslt, 'Ljdplus/toolkit/base/api/math/matrices/Matrix;', 'getOutliers')
  jparameters<-.jcall(jrslt, 'Ljdplus/toolkit/base/api/math/matrices/Matrix;', 'getParameters')
  jtau0<-.jcall(jrslt, 'Ljdplus/toolkit/base/api/math/matrices/Matrix;', 'getInitialTau')
  jtau1<-.jcall(jrslt, 'Ljdplus/toolkit/base/api/math/matrices/Matrix;', 'getFinalTau')
  jvars<-.jcall(jrslt, 'Ljdplus/toolkit/base/api/math/matrices/Matrix;', 'getVariables')

  outliers<-rjd3toolkit::.jd2r_matrix(joutliers)
  if (nrow(outliers)>0) outliers[,c(1,2)]<-1+outliers[,c(1,2)]
  return (list(outliers=outliers,
               models=rjd3toolkit::.jd2r_matrix(jparameters),
               initial_tau=rjd3toolkit::.jd2r_matrix(jtau0),
               final_tau=rjd3toolkit::.jd2r_matrix(jtau1),
               variables=rjd3toolkit::.jd2r_matrix(jvars)
  ))
}

#' Title
#'
#' @param y
#' @param bsm
#' @param X
#' @param mad
#'
#' @returns
#' @export
#'
#' @examples
#' bsm<-rjd3sts::bsm_model(12)
#' q<-bsm_outlier(rjd3toolkit::Retail$BookStores, bsm=bsm)
#' matplot(q[,c(4,5,6,7)], type='l')
bsm_outlier<-function(y, bsm, X=NULL, mad =FALSE){
  jbsm<-rjd3sts::.r2jd_bsm(bsm)
  jrslt<-.jcall('jdplus/toolkitx/base/r/Outliers', 'Ljdplus/toolkit/base/api/math/matrices/Matrix;', 'bsmOutlier',
                as.numeric(y), jbsm, rjd3toolkit::.r2jd_matrix(X), as.logical(mad))

  return (rjd3toolkit::.jd2r_matrix(jrslt))
}

#' Title
#'
#' @param y
#' @param arima
#' @param X
#' @param mean
#' @param critical.value
#' @param outliers
#' @param fast
#' @param ml
#' @param regc
#'
#' @returns
#' @export
#'
#' @examples
tramo_outliers<-function(y, arima, X=NULL, mean=FALSE, critical.value = 0, outliers=c("ao", "ls", "tc:0.7"), fast = TRUE, ml=FALSE, regc = FALSE){
  jarima<-rjd3toolkit::.r2jd_sarima(arima)
  jrslt<-.jcall('jdplus/toolkitx/base/r/Outliers', 'Ljdplus/toolkit/base/api/math/matrices/Matrix;', 'tramoOutliers',
                as.numeric(y), jarima, mean, rjd3toolkit::.r2jd_matrix(X),
                as.numeric(critical.value), .jarray(outliers), as.logical(fast), as.logical(ml), as.logical(regc))

  return (rjd3toolkit::.jd2r_matrix(jrslt))
}

#' Title
#'
#' @param y
#' @param arima
#' @param X
#' @param mean
#' @param critical.value
#' @param outliers
#' @param fast
#'
#' @returns
#' @export
#'
#' @examples
x12_outliers<-function(y, arima, X=NULL, mean=FALSE, critical.value = 0, outliers=c("ao", "ls", "tc:0.7"), mad = TRUE){
  jarima<-rjd3toolkit::.r2jd_sarima(arima)
  jrslt<-.jcall('jdplus/toolkitx/base/r/Outliers', 'Ljdplus/toolkit/base/api/math/matrices/Matrix;', 'x12Outliers',
                as.numeric(y), jarima, mean, rjd3toolkit::.r2jd_matrix(X),
                as.numeric(critical.value), .jarray(outliers), as.logical(mad))

  return (rjd3toolkit::.jd2r_matrix(jrslt))
}


#' Title
#'
#' @param y
#' @param bsm
#' @param X
#' @param critical.value
#' @param ao
#' @param ls
#' @param so
#' @param mad
#' @param forward
#' @param backward
#'
#' @returns
#' @export
#'
#' @examples
bsm_outliers<-function(y, bsm, X=NULL, critical.value = 0, ao=TRUE, ls=TRUE, so=FALSE, mad =TRUE, forward=c("Full", "Score", "Point"), backward=c("Point", "Score","Full")){
  forward=match.arg(forward)
  backward=match.arg(backward)
  jbsm<-rjd3sts::.r2jd_bsm(bsm)
  jspec<-rjd3sts::.bsm2spec(jbsm)
  jrslt<-.jcall('jdplus/toolkitx/base/r/Outliers', 'Ljdplus/toolkitx/base/r/Outliers$Results;', 'bsmOutliers',
                as.numeric(y), as.integer(bsm$period), jspec, rjd3toolkit::.r2jd_matrix(X),
                as.numeric(critical.value**2), as.logical(ao), as.logical(ls), as.logical(so),
                as.logical(mad), forward, backward)

  joutliers<-.jcall(jrslt, 'Ljdplus/toolkit/base/api/math/matrices/Matrix;', 'getOutliers')
  jparameters<-.jcall(jrslt, 'Ljdplus/toolkit/base/api/math/matrices/Matrix;', 'getParameters')
  jtau0<-.jcall(jrslt, 'Ljdplus/toolkit/base/api/math/matrices/Matrix;', 'getInitialTau')
  jtau1<-.jcall(jrslt, 'Ljdplus/toolkit/base/api/math/matrices/Matrix;', 'getFinalTau')
  jvars<-.jcall(jrslt, 'Ljdplus/toolkit/base/api/math/matrices/Matrix;', 'getVariables')

  outliers<-rjd3toolkit::.jd2r_matrix(joutliers)
  if (nrow(outliers)>0) outliers[,c(1,2)]<-1+outliers[,c(1,2)]
  return (list(outliers=outliers,
               models=rjd3toolkit::.jd2r_matrix(jparameters),
               initial_tau=rjd3toolkit::.jd2r_matrix(jtau0),
               final_tau=rjd3toolkit::.jd2r_matrix(jtau1),
               variables=rjd3toolkit::.jd2r_matrix(jvars)
  ))
}


#' Title
#'
#' @param id
#' @param period
#' @param n
#' @param pos
#'
#' @returns
#' @export
#'
#' @examples
#' outlier_regressor("wo", 6, 120, 56)
outlier_regressor<-function(id, period, n, pos){
  return (.jcall('jdplus/toolkitx/base/r/Outliers', '[D', 'outlier',
                 as.character(id), as.integer(period), as.integer(n), as.integer(pos-1)))
}

#' Title
#'
#' @param ids
#' @param period
#' @param n
#' @param pos
#'
#' @returns
#' @export
#'
#' @examples
#' outlier_regressors(c("wo", "ao"), 6, 120, c(56, 65))
outlier_regressors<-function(ids, period, n, pos){
  jrslt<-.jcall('jdplus/toolkitx/base/r/Outliers', 'Ljdplus/toolkit/base/api/math/matrices/Matrix;', 'outliers',
                .jarray(as.character(ids)), as.integer(period), as.integer(n), .jarray(as.integer(pos)))
  return (rjd3toolkit::.jd2r_matrix(jrslt))
}
