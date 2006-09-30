"parabolaSlopes" <-
function (x, y) 
{
    m <- length(x)
    m1 <- m - 1
    dx <- diff(x)
    dy <- diff(y)
    dydx = dy/dx
    if (m == 2) {
        yp <- rep(dydx, 2)
    }
    else {
        yp[2:m1] = (dydx[-m1] * dx[2:m1] + dydx[2:m1] * dx[-m1])/(dx[2:m1] + 
            dx[-m1])
        yp[1] = 2 * dydx[1] - yp[2]
        yp[m] = 2 * dydx[m1] - yp[m1]
    }
    yp
}
