\name{stinemanSlopes}
\alias{stinemanSlopes}

\title{Estimates the slope of an interpolating function using an arc.}

\description{Returns estimates of the slope of an interpolating function that
 runs through a set of points in the xy-plane. The slopes are calculated
 using to the algorithm of Stineman (1980), i.e. from the tangent of
 circles passing through every three consecutive points.} 

\usage{
     stinemanSlopes(x,y,scale=FALSE)
}

\arguments{
\item{x,y}{Coordinates of points defining the interpolating function.}
\item{scale}{If true (default) then the x and y values are normalized prior to the slope calculation.} 
}

\value{
Returns an estimate of the slope of the interpolant at (x,y). 
}
\author{Tomas Johannesson}
\examples{
## Interpolate a smooth curve
x <- seq(0,2*pi,by=pi/6)
y <- sin(x)
stinemanSlopes(x,y,scale=TRUE)
stinemanSlopes(x,y,scale=FALSE)
}

\note{
This function is used as part of the Stineman
interpolation function \code{\link{stinterp}}. 
It is rarely called directly by the user, 
and checking of x and y must be performed by the calling function.

Stineman's method provides a more robust interpolating function
near abrupt steps or spikes in the point sequence
than the alternative method based on a second degree interpolating polynomial, 
which is provided by the function \code{\link{parabolaSlopes}} (see the documentation 
of the function \code{\link{stinterp}} for further information), 
but it results in slightly less accuracy for smooth functions.
} 

\seealso{\code{\link{stinterp}} and \code{\link{parabolaSlopes}}.}

\references{
  Stineman, R. W. \emph{A Consistently Well Behaved Method of Interpolation.} 
  Creative Computing (1980), volume 6, number 7, p. 54-57.
}

\keyword{math}
\keyword{ts}
\keyword{smooth}
