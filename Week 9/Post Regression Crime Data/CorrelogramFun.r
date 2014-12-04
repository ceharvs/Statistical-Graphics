panel.corrgam.2 <-
function(x,y,z,subscripts, at=pretty(),scale=.8,...)
{ require("grid",quietly=TRUE)
  x <- as.numeric(x)[subscripts]
  y <- as.numeric(y)[subscripts]
  z <- as.numeric(z)[subscripts]
  zcol <- level.colors(z,at = at,...)
  for (i in seq(along = z))
  { lims <- range(0,z[i])
    tval <- 2*base::pi * 
      seq(from = lims[1], to = lims[2], by=.01) 
    grid.polygon(x=x[i] + .5*scale*c(0,sin(tvalue)),
                 y=y[i] + .5*scale*c(0,cos(tvalue)),
                 default.unit="native",
                 gp = gpar(fill = zcol[[i]]))
    grid.circles(x=x[i],y=y[i],r = .5*scale,
       default.unit="native")          
  }
}

levelplot(