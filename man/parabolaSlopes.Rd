\name{parabolaSlopes}
\alias{parabolaSlopes}

\title{Estimates the slope of an interpolating function using a parabola.}

\description{Returns estimates of the slope of an interpolating function that 
 runs through a set of points in the xy-plane.}

\usage{ 
     parabolaSlopes(x,y)
}

\arguments{
\item{x,y}{Coordinates of points defining the interpolating function.}
}

\value{
Returns an estimate of the slope of the interpolant at (x,y). 
}
\author{Norbert Nemec, Institute of Theoretical Physics, University of Regensburg. Translation from Python code by  Halldor Bjornsson}
\examples{
x <- seq(0,2*pi,by=pi/6)
y <- sin(x)
\dontrun{parabolaSlopes(x,y)}
}
\note{
This function is used as part of the Stineman
interpolation function \code{\link{stinterp}}. 
It is rarely called directly by the user, 
and checking of x and y must be performed by the calling function.

The parabola method provides a better approximation of the slope for smooth functions
than the original method suggested by Stineman (1980), which is provided by
the function \code{\link{stinemanSlopes}} (see the documentation 
of the function \code{\link{stinterp}} for further information), 
but it results in higher slopes near abrupt steps or spikes 
and can lead to some overshooting where Stineman's method does not.
}
\seealso{\code{\link{stinterp}} and \code{\link{stinemanSlopes}}.}

\keyword{math}
\keyword{ts}
\keyword{smooth}
