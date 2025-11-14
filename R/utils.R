#' @importFrom rJava .jpackage .jcall .jnull .jarray .jevalArray .jcast .jcastToArray .jinstanceof is.jnull .jnew .jclass
#' @importFrom rjd3toolkit result dictionary .jd2r_matrix 
#' @importFrom rjd3sts .r2jd_bsm

NULL


.jd3_arima<-function(model){
  if (inherits(model, "JD3_ARIMA")){
    return (.jcall("jdplus/toolkit/base/r/arima/ArimaModels", "Ljdplus/toolkit/base/core/arima/ArimaModel;", "of",
                   .jarray(as.numeric(model$ar)),
                   .jarray(as.numeric(model$delta)),
                   .jarray(as.numeric(model$ma)),
                   as.numeric(model$var), F))
  }else if (inherits(model, "JD3_SARIMA")){
    return (.jcall("jdplus/toolkit/base/r/arima/SarimaModels", "Ljdplus/toolkit/base/core/sarima/SarimaModel;", "of",
                   as.integer(model$period),
                   .jarray(as.numeric(model$phi)),
                   as.integer(model$d),
                   .jarray(as.numeric(model$theta)),
                   .jarray(as.numeric(model$bphi)),
                   as.integer(model$bd),
                   .jarray(as.numeric(model$btheta))))
  }else{
    stop("Invalid model")
  }
}
