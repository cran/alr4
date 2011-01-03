#  Code in the alr4 package
#  Only alrWeb and alr4Web are exported
#  Eventually, make these the same function.
#  December 12, 2012
 

alrWeb <-
function (page = c("webpage", "errata", "primer"), script)
{
    script.page <- "http://www.stat.umn.edu/alr/Links/scripts/"
    page <- match.arg(page)
    urls <- c(webpage = "http://www.stat.umn.edu/alr/", 
        errata = "http://www.stat.umn.edu/alr/Links/errata.pdf", 
        primer = "http://www.stat.umn.edu/alr/Links/Rprimer.pdf")
    url <- urls[page]
    if(!missing(script)) url <- paste(script.page, script, ".R", sep="")
    browseURL(url)
}

alr4Web <-
function (page = c("webpage", "errata", "primer", "facebook"), script)
{
    script.page <- "http://www.stat.umn.edu/~sandy/alr4ed/Links4/scripts4/"
    page <- match.arg(page)
    urls <- c(webpage = "http://www.stat.umn.edu/~sandy/alr4ed", 
        errata = "http://www.stat.umn.edu/~sandy/alr4ed/Links4/errata4.pdf", 
        primer = "http://www.stat.umn.edu/~sandy/alr4ed/Links4/Rprimer4.pdf",
        facebook= "http://www.facebook.com/alr4ed")
    url <- urls[page]
# temporary
    if(!missing(script)) return("Scripts not ready yet for 4th edition")
    if(page == "errata") return("No errata available")
#    if(!missing(script)) url <- paste(script.page, script, ".R", sep="")
    browseURL(url)
}



# Written 12/26/2012 S. Weisberg
# Not currently exported, so no one knows this is here
logOffset <- function(x, offset= rep(1, dim(x)[2]), skip=NULL){
# x is a matrix or data frame
# returns an object like x with j-th column replaced by
#    x[, j] if x is other than of type numeric or integer
#    x[, j] if j %in% skip
#    log(x[, j]) if x[, j] is strictly positive
#    log(x[, j] + offset[j] = min(x[, j], na.rm=TRUE)) 
#                 if x[, j] has nonpositive elements 
  for(j in 1:dim(x)[2]){
    if( (class(x[, j]) %in% c("numeric", "integer")) & !(j %in% skip)){
       x[, j] <- if((m <- min(x[ ,j], na.rm=TRUE)) <= 0)
                     log(x[ ,j] + m + offset[j]) else
                     log(x[, j])}}
    x}
 
# Override for print.summary.lm
# adds argument 'short=FALSE'
# if short os TRUE, some of the output is skipped
# Used to get shorter output for Applied Linear Regression 4th Ed.
# Nov 24, 2012 by S. Weisberg
# Not currently exported.

print.summary.lm <-
function (x, digits = max(3, getOption("digits") - 3), symbolic.cor = x$symbolic.cor,
    signif.stars = getOption("show.signif.stars"), short=FALSE, ...)
{   if(!short){
    cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
        "\n\n", sep = "") }
    resid <- x$residuals
    df <- x$df
    rdf <- df[2L]
    if(!short){
    cat(if (!is.null(x$weights) && diff(range(x$weights)))
        "Weighted ", "Residuals:\n", sep = "")
    if (rdf > 5L) {
        nam <- c("Min", "1Q", "Median", "3Q", "Max")
        rq <- if (length(dim(resid)) == 2L)
            structure(apply(t(resid), 1L, quantile), dimnames = list(nam,
                dimnames(resid)[[2L]]))
        else {
            zz <- zapsmall(quantile(resid), digits + 1)
            structure(zz, names = nam)
        }
        print(rq, digits = digits, ...)
    }
    else if (rdf > 0L) {
        print(resid, digits = digits, ...)
    }
    else {
        cat("ALL", df[1L], "residuals are 0: no residual degrees of freedom!\n")
    }
    }
    if (length(x$aliased) == 0L) {
        cat("\nNo Coefficients\n")
    }
    else {
        if (nsingular <- df[3L] - df[1L])
            cat("\nCoefficients: (", nsingular, " not defined because of singularities)\n",
                sep = "")
        else cat("\nCoefficients:\n")
        coefs <- x$coefficients
        if (!is.null(aliased <- x$aliased) && any(aliased)) {
            cn <- names(aliased)
            coefs <- matrix(NA, length(aliased), 4, dimnames = list(cn,
                colnames(coefs)))
            coefs[!aliased, ] <- x$coefficients
        }
        printCoefmat(coefs, digits = digits, signif.stars = signif.stars,
            na.print = "NA", ...)
    }
    cat("\nResidual standard error:", format(signif(x$sigma,
        digits)), "on", rdf, "degrees of freedom\n")
    if (nzchar(mess <- naprint(x$na.action)))
        cat("  (", mess, ")\n", sep = "")
    if (!is.null(x$fstatistic)) {
        cat("Multiple R-squared:", formatC(x$r.squared, digits = digits))
        if(!short){
        cat(",\tAdjusted R-squared:", formatC(x$adj.r.squared,
            digits = digits))}
        cat("\nF-statistic:", formatC(x$fstatistic[1L],
            digits = digits), "on", x$fstatistic[2L], "and",
            x$fstatistic[3L], "DF,  p-value:", format.pval(pf(x$fstatistic[1L],
                x$fstatistic[2L], x$fstatistic[3L], lower.tail = FALSE),
                digits = digits), "\n")
    }
    correl <- x$correlation
    if (!is.null(correl)) {
        p <- NCOL(correl)
        if (p > 1L) {
            cat("\nCorrelation of Coefficients:\n")
            if (is.logical(symbolic.cor) && symbolic.cor) {
                print(symnum(correl, abbr.colnames = NULL))
            }
            else {
                correl <- format(round(correl, 2), nsmall = 2,
                  digits = digits)
                correl[!lower.tri(correl)] <- ""
                print(correl[-1, -p, drop = FALSE], quote = FALSE)
            }
        }
    }
    cat("\n")
    invisible(x)
}
