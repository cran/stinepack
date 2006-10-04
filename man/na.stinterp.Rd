\name{na.stinterp}
\alias{na.stinterp}
\alias{na.stinterp.default}
\title{Replace NAs by Stineman interpolation}
\description{
Generic functions for replacing each \code{NA} with Stineman interpolated values.
}
\usage{
na.stinterp(object, \dots) 
\method{na.stinterp}{default}(object, along = time(object), na.rm = TRUE, \dots) 
}
\arguments{
  \item{object}{object in which \code{NA}s are to be replaced.}
  \item{along}{variable to use for interpolation. Has to be numeric, is
    otherwise coerced to numeric.}
  \item{na.rm}{logical. Should leading and trailing \code{NA}s be removed?}
  \item{\dots}{further arguments passed to methods.}
}

\details{
  Missing values (\code{NA}s) are replaced by piecewice rational 
  interpolation via \code{\link{stinterp}}.

  By default the time index associated with \code{object} is used
  for interpolation. Note, that if this calls \code{time.default}
  this gives an equidistant spacing \code{1:NROW(object)}. If \code{object}
  is a matrix or data.frame, the interpolation is done separately for
  each column.
}

\value{
An object in which each \code{NA} in the input object is replaced
by interpolating the non-\code{NA} values before and after it.
Leading and trailing \code{NA}s are omitted (if \code{na.rm = TRUE}) 
or not replaced (if \code{na.rm = FALSE}). 
}

\author{Gabor Grothendieck}

\seealso{\code{\link{stinterp}}.}

\examples{
na.stinterp(c(2,NA,1,4,5,2))
na.stinterp(ts(c(2,NA,1,4,5,2)))
\dontrun{

#comparison of gap filling with na.stinterp/stinterp and splines,
#the solid lines show the continuous interpolation functions 
#implicitly assumed by the gap-filling, they show how the Stineman
#interpolation restricts the range of the interpolant to the 
#nearby range of the points and suppresses the well known oscillations 
#characteristic of splines and other methods based on polynomials
x <- 1:6
y <- c(2,NA,1,4,5,2)
plot(x,y,ylim=c(-1,5))
points(na.stinterp(c(2,NA,1,4,5,2)),pch=2,col=2)
points(spline(x,y,n=6),pch=3,col=3)
lines(stinterp(x[!is.na(y)],y[!is.na(y)],xout=seq(1,6,by=0.1)),col=2)
lines(spline(x,y,n=50),col=3)

library(zoo)
na.stinterp(zoo(c(2,NA,1,4,5,2)))

library(its)
na.stinterp(its(c(2,NA,1,4,5,2), seq(Sys.time(), length = 6, by = "day")))

}

}
\keyword{ts}
