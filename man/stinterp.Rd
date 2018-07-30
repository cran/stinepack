\name{stinterp}
\alias{stinterp}

\title{A consistently well behaved method of interpolation}
\description{Returns the values of an interpolating function that
 runs through a set of points in the xy-plane according
 to the algorithm of Stineman (1980).
}

\usage{ 
stinterp(x,y,xout,yp,method=c("scaledstineman","stineman","parabola"))
}

\arguments{
\item{x,y}{coordinates of points defining the interpolating function.
NAs are not allowed, but see \code{\link{na.stinterp}} 
for gap-filling based on \code{stinterp}.}
\item{xout}{the x-coordinate where the interpolant is to be found.}
\item{yp}{slopes of the interpolating function at x. 
Optional: only given if they are known, else the argument is not used.}
\item{method}{method for computing the slope at the given points 
if the slope is not known. With \code{method}="stineman",  Stineman's
original method based on an interpolating circle is used. 
Use \code{method}="scaledstineman" if scaling of x and y is to 
be carried out before Stineman's method is applied, and use 
\code{method}="parabola" to calculate the slopes from a 
parabola through every three points. Partial argument 
matching is supported (for example \code{m}="pa" instead 
of \code{method}="parabola").}
}

\value{\code{stinterp} returns a list with components 'x' and 'y'
with the coordinates of the interpolant at the points specified by \code{xout}.
}
\author{Tomas Johannesson, Halldor Bjornsson}

\examples{
## Interpolation with rational functions
#This example shows how the rational interpolating functions used in
#`stinterp' have a monotonic slope over an interval with widely varying
#slopes specified at the end points, consistent with Stineman's
#requirements above. A third degree interpolating function (commonly
#used in spline interpolation), on the other hand, leads to
#spurious oscillations, which are a well known problem in
#interpolation with a single polynomial function and also
#in piecewise polynomial interpolation.
\dontrun{
xo <- seq(0,1,by=1/50)
plot(c(0,1),c(0,1),xlim=c(0,1),ylim=c(-1,1),xlab="",ylab="")
for (s in 2:10) {
lines(stinterp(c(0,1),c(0,1),xo,yp=c(0,s)))
lines(xo,xo^2*((s-2)*xo-s+3),col=2) }
}
#Note that the two interpolation functions almost coincide for the
#lowest value (s=2) of the slope at the right end point.
#The user may verify that the rational interpolating functions continue
#to provide "reasonable" results for much higher values of the slope at the
#right end point (for example s=15, s=25 or s=100), for which the third degree
#polynomial leads to absurd results (for most practical purposes).

## Interpolate a smooth curve
#This example illustrates that the interpolation procedure
#reproduces a smooth function with known slopes at the specified
#points very well. If the slopes are not known, both methods for
#estimating the slopes at the specified points (the default method
#and method="parabola") lead to good interpolating functions, but
#the "parabola" method is slightly more accurate. The traditional 
#spline interpolation method leads to a similar result as Stineman's 
#method with slopes computed with method="parabola".
x <- seq(0,2*pi,by=pi/6)
y <- sin(x)
yp <- cos(x)
xo <- seq(0,2*pi,by=pi/150)
y1 <- stinterp(x,y,xo,yp)$y
y2 <- stinterp(x,y,xo)$y
y3 <- stinterp(x,y,xo,m="pa")$y
\dontrun{
plot(x,y)
lines(xo,sin(xo))
points(stinterp(x,y,xo,yp),cex=1/5,col=2)
points(stinterp(x,y,xo),cex=1/5,col=3)
points(stinterp(x,y,xo,method="parabola"),cex=1/5,col=4)
points(spline(x,y,n=length(xo)),cex=1/5,col=5)
}

## Interpolate through a sharp oscillation
#This example shows that Stineman's interpolation, with the default
#method for estimating slopes at the given points, results in no oscillations
#in the neighbourhood of a spike. If the slopes at the given points are
#computed with method="parabola", some overshooting can be seen and
#spline interpolation leads to repeated oscillations near the spike.
\dontrun{
yy <- y
yy[3] <- -1.5
plot(x,yy,ylim=c(-1.5,1.5))
points(stinterp(x,yy,xo),cex=1/5,col=3)
points(stinterp(x,yy,xo,method="parabola"),cex=1/5,col=4)
points(spline(x,yy,n=length(xo)),cex=1/5,col=5)
}
}

\note{
The interpolation method is described in an article by Russell W. Stineman
in the July 1980 issue of Creative Computing with a note from the editor
stating that while they were "not an academic journal but once in a while
something serious and original comes in" adding that this was "apparently
a real solution" to a well known problem.

According to Stineman, the interpolation procedure has "the following properties:
\enumerate{
\item
If values of the ordinates of the specified points change monotonically,
and the slopes of the line segments joining the points change monotonically,
then the interpolating curve and its slope will change monotonically.
\item
If the slopes of the line segments joining the specified points change
monotonically, then the slopes of the interpolating curve will change
monotonically.
\item
Suppose that the conditions in (1) or (2) are satisfied by a set of points,
but a small change in the ordinate or slope at one of the points will result
conditions (1) or (2) being not longer satisfied. Then making this small change
in the ordinate or slope at a point will cause no more than a small change in the
interpolating curve."
}
The method is based on rational interpolation with specially chosen
rational functions to satisfy the above three conditions.

Slopes computed at the given points with the methods provided by the `stinterp'
function satisfy Stineman's requirements. The original method suggested by
Stineman (method="scaledstineman", the default, and "stineman") result in lower slopes 
near abrupt steps or spikes in the point sequence, and therefore a smaller tendency
for overshooting. The method based on a second degree polynomial (method="parabola")
provides better approximation to smooth functions, but it results in in higher slopes
near abrupt steps or spikes and can lead to some overshooting where Stineman's
method does not. Both methods lead to much less tendency for `spurious' oscillations
than traditional interplation methods based on polynomials, such as splines (see the
\code{examples} section). 

Stineman states that "The complete assurance that the procedure will never 
generate `wild' points makes it attractive as a general purpose procedure".

This interpolation method has been implemented in Matlab and Python in addition to R.
}

\seealso{\code{\link{na.stinterp}}, 
\code{\link{stinemanSlopes}} and \code{\link{parabolaSlopes}}.}

\references{
  Stineman, R. W. \emph{A Consistently Well Behaved Method of Interpolation.}   
  Creative Computing (1980), volume 6, number 7, p. 54-57.
}

\keyword{math}
\keyword{ts}
\keyword{smooth}
