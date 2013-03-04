\name{Mplus2texreg-package}
\alias{Mplus2texreg-package}
\alias{Mplus2texreg}
\docType{package}
\title{
Preparing Mplus output for texreg through MplusAutomation -
Mplus2texreg 
}
\description{
More about what it does (maybe more than one line)
~~ A concise (1-5 lines) description of the package ~~
}
\details{
\tabular{ll}{
Package: \tab Mplus2texreg\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-03-03\cr
License: \tab What license is it under?\cr
}
This package uses the MplusAutomation package to read Mplus output files, create R model objects, which can be used by the texreg package to create regression tables for LaTeX, html, and Microsoft Word. Currently, the package is only for regression models. In cases where Full Information Maximum Likelihood (FIML) methods are needed to deal with missing data, Mplus can be used for regression modeling.
}
\author{
William Murrah

Maintainer: William Murrah <william.murrah@gmail.com>
}
\references{

Hallquist, Michael (2012). MplusAutomation: Automating Mplus Model
  Estimation and Interpretation. R package version 0.5-3.
  http://CRAN.R-project.org/package=MplusAutomation
  
Leifeld, Philip (2013). texreg: Conversion of R Regression Output to LaTeX Tables.
Swiss Federal Institute of Aquatic Science and Technology (Eawag), Duebendorf,
Switzerland. R package version 1.22.
  
Muthén, L.K. and Muthén, B.O. (1998-2012). Mplus User’s Guide. Seventh Edition.
Los Angeles, CA: Muthén & Muthén

}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R ~~
~~ documentation directory ~~
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
~~ simple examples of the most important functions ~~
}
