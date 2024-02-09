#" @importFrom rJava .jcall .jcast
NULL

.jd3_arima<-function(model){
    if (inherits(model, "JD3_ARIMA")){
        return(.jcall("jdplus/toolkit/base/r/arima/ArimaModels", "Ljdplus/toolkit/base/core/arima/ArimaModel;", "of",
                      .jarray(as.numeric(model$ar)),
                      .jarray(as.numeric(model$delta)),
                      .jarray(as.numeric(model$ma)),
                      as.numeric(model$var), FALSE))
    }else if (inherits(model, "JD3_SARIMA")){
        return(.jcall("jdplus/toolkit/base/r/arima/SarimaModels", "Ljdplus/toolkit/base/core/sarima/SarimaModel;", "of",
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

#" Auto-covariance function
#"
#" @param model Arima model (or Sarima model)
#" @param n Number of auto-covariances
#"
#" @return Auto-covariances of the model. The first one corresponds to the variance and the last one to
#" the auto-covariance of lag n-1.
#" @export
#"
#" @examples
#" model<-rjd3toolkit::sarima_model(
#" name = "sarima",
#" period =12,
#" phi = c(-.4, .5),
#" d = 0,
#" theta = -.9,
#" bphi = NULL,
#" bd = 0,
#" btheta = -.7
#")
#"
#" ac<-arima_acf(model, 120)
arima_acf<-function(model, n){
    jarima<-.jd3_arima(model)
    return(.jcall("jdplus/toolkitx/base/r/ArimaModels", "[D", "acf",
                  .jcast(jarima, "jdplus/toolkit/base/core/arima/IArimaModel"), as.integer(n)))
}

#" Psi-weights (MA representation)
#"
#" @param model Arima model (or Sarima model)
#" @param n Number of psi-weights
#"
#" @return The n first psi-weights
#"
#" @export
#"
#" @examples
#" model<-rjd3toolkit::sarima_model(
#" name = "sarima",
#" period =12,
#" phi = c(-.4, .5),
#" d = 0,
#" theta = -.9,
#" bphi = NULL,
#" bd = 0,
#" btheta = -.7
#")
#"
#" psi<-arima_psi_weights(model, 120)
arima_psi_weights<-function(model, n){
    jarima<-.jd3_arima(model)
    return(.jcall("jdplus/toolkitx/base/r/ArimaModels", "[D", "psi",
                  .jcast(jarima, "jdplus/toolkit/base/core/arima/IArimaModel"), as.integer(n)))
}

#" Pi-weights (AR representation)
#"
#" @param model Arima model (or Sarima model)
#" @param n Number of pi-weights
#"
#" @return The n first pi-weights
#"
#" @export
#"
#" @examples
#" model<-rjd3toolkit::sarima_model(
#" name = "sarima",
#" period =12,
#" phi = c(-.4, .5),
#" d = 0,
#" theta = -.9,
#" bphi = NULL,
#" bd = 0,
#" btheta = -.7
#")
#"
#" pi<-arima_pi_weights(model, 120)
arima_pi_weights<-function(model, n){
    jarima<-.jd3_arima(model)
    return(.jcall("jdplus/toolkitx/base/r/ArimaModels", "[D", "pi",
                  .jcast(jarima, "jdplus/toolkit/base/core/arima/IArimaModel"), as.integer(n)))
}

#" Extends a series with ARIMA forecasts/backcasts. Exact intialization for the MA part, conditional initialization (=0)
#" for the AR part
#"
#"
#" @param data The data to extend
#" @param model The Arima model
#" @param mean Indicates that a mean correction will be estimated
#" @param nbcasts Number of backcasts
#" @param nfcasts Number of forecasts
#"
#" @return The extended series
#" @export
#"
#" @examples
#" model<-rjd3toolkit::sarima_model(
#" name = "sarima",
#" period =12,
#" phi = NULL,
#" d = 1,
#" theta = -.9,
#" bphi = NULL,
#" bd = 1,
#" btheta = -.7
#")
#"
#" s<-arima_extends_fast(rjd3toolkit::ABS$X0.2.09.10.M, model, nbcasts=24, nfcasts = 60)
arima_extends_fast<-function(data, model, mean=0, nbcasts=0, nfcasts=0){
    jarima<-.jd3_arima(model)
    return(.jcall("jdplus/toolkitx/base/r/ArimaModels", "[D", "extendsFast", as.numeric(data),
                  .jcast(jarima, "jdplus/toolkit/base/core/arima/IArimaModel"),
                  as.numeric(mean), as.integer(nbcasts), as.integer(nfcasts)))

}

#" Extends a series with ARIMA forecasts/backcasts compute by means of the Kalman filter
#" with exact diffuse initialization
#"
#"
#" @param data The data to extend
#" @param model The Arima model
#" @param mean Indicates that a mean correction will be estimated
#" @param nbcasts Number of backcasts
#" @param nfcasts Number of forecasts
#"
#" @return The extended series
#" @export
#"
#" @examples
#" model<-rjd3toolkit::sarima_model(
#" name = "sarima",
#" period =12,
#" phi = NULL,
#" d = 1,
#" theta = -.9,
#" bphi = NULL,
#" bd = 1,
#" btheta = -.7
#")
#"
#" s<-arima_extends_exact(rjd3toolkit::ABS$X0.2.09.10.M, model, nbcasts=24, nfcasts = 60)
#" plot(s, type="l")
arima_extends_exact<-function(data, model, mean=FALSE, nbcasts=0, nfcasts=0){
    jarima<-.jd3_arima(model)
    return(.jcall("jdplus/toolkitx/base/r/ArimaModels", "[D", "extendsExact", as.numeric(data),
                  .jcast(jarima, "jdplus/toolkit/base/core/arima/IArimaModel"),
                  as.logical(mean), as.integer(nbcasts), as.integer(nfcasts)))

}
