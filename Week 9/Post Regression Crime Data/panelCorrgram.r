panel.corrgram <- 
  function(x, y, z, subscripts, at,
    level = .9, label=FALSE, ...)
{ require("ellipse", quietly = TRUE)
  x <- as.numeric(x)[subscripts]
  y <- as.numeric(y)[subscripts]
  z <- as.numeric(z)[subscripts]
  zcol <- level.colors(z, at = at, ...)
  for(i in seq( along = z )){ 
    ell <- ellipse(z[i], level = level, npoints = 50,
      scale=c(.2,.2),centre=c(x[i],y[i]))
    panel.polygon(ell, col=zcol[i], border = zcol[i], ...)
  } 
  if(label)
    panel.text(x=x,y=y,lab = 100*round(z,2),cex=.8,
      col = ifelse(z<0, "white","black"))
}

panel.corrgram.2 <-
function(x,y,z,subscripts, at=pretty(z),scale=0.8,...)
{ require("grid", quietly=TRUE)
  x <- as.numeric(x)[subscripts]
  y <- as.numeric(y)[subscripts]
  z <- as.numeric(z)[subscripts]
  zcol <- level.colors(z, at = at,...)
  for(i in seq( along = z ))
  { lims <- range(0,z[i])
    tval <- 2* base::pi * 
      seq(from = lims[1], to = lims[2], by=.01) 
    grid.polygon(x=x[i] + .5*scale*c(0,sin(tval)),
                 y=y[i] + .5*scale*c(0,cos(tval)),
                 default.unit="native",
                 gp = gpar(fill = zcol[[i]]))
    grid.circle(x=x[i],y=y[i],r = .5*scale,
       default.unit="native")          
  }
}
