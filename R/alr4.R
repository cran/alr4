#  Code in the alr4 package
#  December 12, 2012
#  Revised July 1, 2013

alr4Web <-
function (page = c("webpage", "errata", "primer", "solutions"))
{
    page <- match.arg(page)
    site <- "http://www.stat.umn.edu/~sandy/alr4ed"
    urls <- c(
        webpage   = site,
        errata    = paste(site, "/links/errata.pdf", sep=""),
        primer    = paste(site, "/links/alrprimer.pdf", sep=""),
        solutions = paste(site, "/links/alrsolutions.pdf", sep=""))
    url <- urls[page]
    utils::browseURL(url)
}


