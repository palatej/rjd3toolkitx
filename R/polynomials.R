#" @importFrom rJava .jpackage .jcall .jnull .jarray .jevalArray .jcast .jcastToArray .jinstanceof is.jnull .jnew .jclass
NULL

#" Evaluates a polynomial for given values
#"
#" @param p Coefficients of the polynomial (for increasing powers of x, starting at the constant)
#" @param x Points where the polynomial must be evaluated
#"
#" @return The polynomial evaluated at the given values
#" @export
#"
#" @examples
#" p<-c(1,2,3,4)
#" polynomial_at(p, c(1, 2))
#" # should be 10, 49
polynomial_at<-function(p, x){
    if (length(x) == 1)
        return(.jcall("jdplus/toolkitx/base/r/Polynomials", "D", "evaluate", .jarray(as.numeric(p)), as.numeric(x)))
    else
        return(.jcall("jdplus/toolkitx/base/r/Polynomials", "[D", "evaluate", .jarray(as.numeric(p)), .jarray(as.numeric(x))))
}

#" Computes the roots of a real polynomial
#"
#" @param p Coefficients of the polynomial (for increasing powers of x, starting at the constant)
#" @param robust if FALSE, a "Muller-Newton" algorithm is used. Otherwise, the roots a the proper values of the companion matrix
#"
#" @return An array with the complex roots is returned
#" @export
#"
#" @examples
#" p<-c(1,2,3,4)
#" polynomial_roots(p, TRUE)
polynomial_roots<-function(p, robust=FALSE){
    jc<-.jcall("jdplus/toolkitx/base/r/Polynomials", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "roots",
               .jarray(as.numeric(p)), as.logical(robust))
    r<-rjd3toolkit::.jd2r_matrix(jc)
    return(complex(real=r[, 1], imaginary=r[, 2]))
}

#" Creates a polynomial from an array of complexes (roots of the result)
#"
#" @param roots Roots of the polynomial (no zero)
#" @param p0 Value of the constant
#" @param smooth Smooth the coefficients (small values are set to 0)
#" @param reorder Reorder the roots (Leja ordering) to get more robust results
#"
#" @return The polynomial corresponding to the roots
#" @export
#"
#" @examples
#" p<-c(1, rep(0, 49), -1)
#" q<-polynomial_roots(p)
#" polynomial_of_roots(q, reorder=TRUE)
polynomial_of_roots<-function(roots, p0=1, smooth=TRUE, reorder=FALSE){
    M<-cbind(sapply(roots, function(z)Re(z)), sapply(roots, function(z)Im(z)))
    jM<-rjd3toolkit::.r2jd_matrix(M)
    return(.jcall("jdplus/toolkitx/base/r/Polynomials", "[D",
                  "polynomialOfRoots", jM, as.numeric(p0), as.logical(smooth), as.logical(reorder)))
}

#" Convolution of a given polynomial (=P(x)*P(1/x))
#"
#" @param p The given polynomial
#"
#" @return Powers 0 to degree(p)
#" @export
#"
#" @examples
#" p<-c(1,2,3,4)
#" polynomial_convolve(p)
polynomial_convolve<-function(p){
    c<-.jcall("jdplus/toolkitx/base/r/Polynomials", "[D", "convolve", .jarray(as.numeric(p)))
    return(c)
}
