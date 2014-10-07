hbin = function(x,y,xbins=20,aspectRatio = 1,xlim=range(x),ylim=range(y))
{

# In this version xbins is expected to be an integer.
# Some checks are made to 
 
# Here aspectRatio means width to height of the data rectangle
# My Splus routine used the term shape to mean height to width

# The function scales x into sx:  [0, xbins].
#                     y into sy:  [0, xbins/(aspectRatio*sqrt(3)]
# In this version xbins is forced to be an integer
                     
# The hexagons are oriented with vertical sides and width 1
# A hexagon is centered at (0,0) and its left edge is at -.5 
# A hexagon is centered at (xbins,0) and its right edge is at xbins+.5
# There are xbins+1 hexagons covering y=0
# The next row up of hexagons has the left edge at x =0,
#     and a "wasted" hexagon on the right

#  Calculation for the shift rows may have problem at the edges 
#     Observations x==xmin or x==xmax may go into the wrong cells.
#     This version takes a little extra time and 
#     converts i2 < 0 to 0 and i2 = xbins to xbins-1 
#     Other approaches may be better, include those not requiring
#     xbins to be an integer

# The hexagons, if drawn, go beyond the limits of the data.  
# Adjusting the x and y scales disportional to accomodate the full
# display of hexagons will lead to departures from the target
# aspectRatio. This may not be noticable or immportant

xbins=ceiling(xbins)
ncol=xbins+1
sx = xbins*(x-xlim[1])/diff(xlim)
sy = xbins*(y-ylim[1])/(diff(ylim)*sqrt(3)*aspectRatio)

# Discussion of y scaling
# This shrinks the hexagon rectangular centered lattice
# to square centered lattice
# Lower left for one square integer lattice is (0,0)
# Lower left for second is at (.5,.5)
# x bin centers will be 0,1,2, ..., xbins
# In hexagon lattice the hexagon center at x=.5
#     has y = 1/(2*sqrt(3)) + 2/(2*sqrt(3)) = sqrt(3)/2 = .5*sqrt(3)
# Dividing by sqrt(3)shrinks this to the target (.5,.5) 

# After scaling the data, the algorithm finds two closed lattice
# points, one from each lattice
# It then pick the closest of these two.   
#     A more careful treatment of ties could be considered   

# Speed
#     The version here use R function saves a lot of intermediate values
#          which is undesirable for large data sets.  More could
#          like be done for speed.  This is a get done
#          implementation          
#     Give extrema for x and y, data can be binned in one pass
#          The Splus version use Fortran code (that I wrote) for this
#          In roughly 89% of cases only one distance calculation is required.
#          Points close to or far from one lattice can be classifed
#          immediately.
#          Over the years, I played with versions to shave off a few 
#          operations or trade off multiplications and additions.     

i1=floor(sx+.5)
j1=floor(sy+.5)
i2=floor(sx)
j2=floor(sy)

# check for possible boundary problems
i2 = ifelse(i2<0,0,i2)
i2 = ifelse(i2==ncol,xbins,i2)

# Discussion
# Points  in (-.5,.5) x (-.5, .5) centered at (0, 0 ) map to i1=0, j1=0 
# Points in   (0, 1) x   (0 1)) centered at (.5 ,.5) map to i2=0, j2=0
# The bottom row of hexagons is indexed by i1,j1
# The next row up is shift to the right by .5 and indexed by i2,j2
# The rows are handle in pairs

# Here i indexes positions along the x axis
#      j indexes positions along the y axis
# Later the view switches to a 
#      matrix of hexagon cells with row and columns
#      Row are staggered 
#      imax indicating the number of rows
#      jmax indicating the number of columns 
 
k = ifelse((sx - i1)^2 + 3*(sy-j1)^2 < (sx-i2-.5)^2 + 3*(sy-j2 -.5)^2,
           2*j1*ncol+i1+1,(2*j2+1)*ncol+i2+1) 

cnt = table(k)
xMean =tapply(x,k,mean)
yMean =tapply(y,k,mean) 
mat= data.frame(cell=as.numeric(names(cnt)),count=as.numeric(cnt),
                xMean=as.numeric(xMean),yMean=as.numeric(yMean))
attr(mat,"xbins")=xbins
attr(mat,"xlim")=xlim
attr(mat,"ylim")=ylim
attr(mat,"aspectRatio")=aspectRatio
oldClass(mat)=c("hbin","data.frame")

return(mat)
} 

#__________________________________________________________________________

plot.hbin = 
function(bin, style = "grayscale",
       minarea = 0.04, maxarea = 0.8,
       mincount = 1, maxcount = max(bin$count),
       nclass = min(16, maxcount), 
       bnds=hbin.bnds(c(mincount,maxcount),nclass),
       basecolors = c("#FFFFD4","#FED98E","#FE9929","#D95F0E","#993404"),
       cRamp =  colorRampPalette(basecolors, space = "Lab"),
       col.regions = cRamp(length(bnds)-1),
       col.nest=c("#D0D0D0","#C0C0FF","#C0F0C0","#FFC0C0","#A0FFFF",
                  "#FFA0FF","#FFFFA0","#000000","#000000","#000000",
                  "#606060","#0000C0","#00A000","#E00000","#007070",
                  "#700070","#707000"),
       border = "#A0A0A0", density = -1,
	 col.one = "#0060CC",
       col.omit="red",
	 legend = T, legend.width = 1, legend.lab = "Counts", legend.cex = 1,
       xlab = "", ylab = "", ...)
{
	if(dev.cur() == 1) windows(width=10,height=9)
	if(minarea < 0)
		stop("Minimum area must be non-negative")
	if(maxarea > 1)
		stop("Maximum area must not be larger than 1")
	if(minarea > maxarea)
		stop("minarea must be smaller than maxarea")
	style.ok <- if(!missing(style)){ char.expand(style, c("grayscale", "centroids", "lattice", "nested.lattice", 
			"nested.centroids"), stop(paste("style ", style, "is not a plotting option at this time. ")))
		 } else style
#	if(style.ok == "grayscale" )col.regions <- 1:cuts
		
	bin.attr <- attributes(bin)
	shape <- 1/bin.attr$aspectRatio
	oldpar <- par()
	pin <- oldpar$pin
	mai <- oldpar$mai
	xsize <- pin[1]
	ysize <- pin[2]
	xsize <- xsize - legend.width
	if(xsize < 1)
		stop("plot width too small")
	if(shape * xsize <= ysize) {
		center <- (ysize - shape * xsize)/2
		mai[1] <- mai[1] + center
		mai[3] <- mai[3] + center
		mai[4] <- mai[4] + legend.width
		ysize <- shape * xsize
	} else {
		center <- (xsize - ysize/shape)/2
		mai[2] <- mai[2] + center
		mai[4] <- mai[4] + center + legend.width
		xsize <- ysize/shape
	}
	fin <- oldpar$fin
	# fin = c(width, height), in inches
	# plt = c(xmin,xmax,ymin,ymax), in proportions of plot figure area
	# mai = c(bottom, left, top, right), in inches
	# Setting plt instead of mai has fewer side effects
	fin.mai.to.plt <- function(fin, mai)
	c(mai[2]/fin[1], 1. - mai[4]/fin[1], mai[1]/fin[2], 1. - mai[3]/fin[2])
	old.pty <- list(pty = par("pty"))
	old.plt <- par(plt = fin.mai.to.plt(fin, mai))
	on.exit(par(c(old.plt, old.pty)))
	to.nonzero.range <- function(lim)
	{
		if(diff(lim) == 0)
			lim <- lim * (1 + c(-1, 1) * 0.04)
		if(diff(lim) == 0)
			lim <- c(-1, 1)
		lim
	}
	# the [xy]lim attributes from hexbin() never have diff 0, but
	# they can when made by db.internal.hexbin().  Adjust here and
	# in hexagons function.  We cannot change the attributes themselves
	# since that would mess up cell2xy().
	xlim <- to.nonzero.range(bin.attr$xlim)
	ylim <- to.nonzero.range(bin.attr$ylim)
	sx <- bin.attr$xbins/diff(xlim)
	sy <- (bin.attr$xbins * shape)/diff(ylim)
	dx <- 0.5/sx
	dy <- 0.5/(sqrt(3) * sy)
	plot(xlim + c( - dx, dx), ylim + c( - dy, dy), type = "n", xlab = xlab, ylab = ylab, axes = F, ...)
	box()
#	log.axis <- function(side, lim)
#	{
#		at <- signif(pretty.log(10^lim), 4)
#		axis(side = side, at = log10(at), lab = as.character(at), srt = if(side %% 2 == 1) 0 else 90)
#	}
#	if(log.x)
#		log.axis(side = 1, lim = xlim)
#	else axis(side = 1)
#	if(log.y)
#		log.axis(side = 2, lim = ylim)
#	else axis(side = 2)
	axis(side=1)
	axis(side=2)
	density <- density * max(xsize, ysize)
	# need to scale 
	key <- hbin.draw(bin, style = style.ok,
                       minarea = minarea, maxarea = maxarea,
                       mincount = mincount, maxcount = maxcount,
                       nbnds = nbnds,
                       bnds=bnds,
                       basecolors=basecolors,
                       col.regions = col.regions,
                       col.nest=col.nest,
			     col.one=col.one,
                       density = density, border = border)
	retpar <- par("plt", "usr")
	if(legend) {
		mai[2] <- mai[2] + xsize
		mai[4] <- mai[4] - legend.width
		par(plt = fin.mai.to.plt(fin, mai), new = T)
		plot(c(0, legend.width), c(0, ysize), type = "n", axes = F, xlab = "", ylab = "", main = "")
		inner <- xsize/bin.attr$xbins
		hbin.legend(col = key$col, bnds = key$bnds,
                 legend.lab = legend.lab, height = ysize,
                 lcex = legend.cex, width = legend.width,
                 inner = inner, style = style.ok,
                 minarea = minarea, maxarea = maxarea, maxcount = maxcount, 
                 density = density,border=border,col.nest=col.nest)
		mai[2] <- mai[2] - xsize
		mai[4] <- mai[4] + legend.width
	}
	invisible(retpar)
}

#_______________________________________________________________________________________________ 

hbin.draw = 
function(bin, style = "grayscale",
       minarea = 0.04, maxarea = 0.8,
       mincount = 1, maxcount = max(bin$count),
       nbnds = min(16, maxcount), 
       bnds=hbin.bnds(c(mincount,maxcount),nbnds),
       basecolors = c("#FFFFD4","#FED98E","#FE9929","#D95F0E","#993404"),
       cRamp =  colorRampPalette(basecolors, space = "Lab"),
       col.regions = cRamp(length(bnds)-1),
       col.nest=c("#D0D0D0","#C0C0FF","#C0F0C0","#FFC0C0","#A0FFFF",
                  "#FFA0FF","#FFFFA0","#000000","#000000","#000000",
                  "#606060","#0000C0","#00A000","#E00000","#007070",
                  "#700070","#707000"),
	 col.one = "#0060CC",
       col.omit = "red",
       density = -1, border = "#A0A0A0")
{
	# Warning:  Adds to a plot.  Presumes the canvas has the right shape
	# and scales.  See hbin.plot() for setup.
	#
	#       style = type of plotting
	#          'centroids' =  symbol area is a function of the count, 
	#                  approximate location near cell center of
	#                  mass without overplotting
	#          'lattice' = symbol area is a function of the count, 
	#                  plot at lattice points
	#          'grayscale'  =  gray scale plot, 
	#                  color number determined by
	#                  color cuts, arguments at, and cuts, 
	#                  area = full hexagons.
	#          'nested.lattice'= plots two hexagons
	#		   background hexagon
	#			 area = full size
	#			 color number by count in 
	#                        powers of 10 starting at col 2
	#                  foreground hexagon 
	#		   	 area by log10(count)-floor(log10(count))
	#                        color number by count in 
	#                         powers of 10 starting at col 12
	#          'nested.centroids' = like nested but counts <10 are plotted
	#                        hexagons plotted at centroids
	#
	# 	density=     0 for hexagon graph paper
	# 	border=      plot the border of the hexagon,
      #                  use T for hexagon graph paper

	# Symbol size encoding: 
	#   Area= minarea + scaled.count*(maxarea-minarea)
	#   When maxarea==1 and scaled.count==1, 
      #   the hexagon cell is completely filled.
	#
	#   If small hexagons are hard to see increase minarea.
	#
	# For gray scale encoding
	#         motif coding: black 15 white puts the first of the
	#                       color class above the background black
      # See colorbrewer   sequentail scale like best but double ended may work 

	# ------------------- Initial Checks ---------------------
	
	if(!inherits(bin, "hbin")) warning("Not a \"hexbin\" object")
	style.ok = if(!missing(style)){
            char.expand(style, c("grayscale", "centroids", "lattice", "nested.lattice", 
		"nested.centroids"), 
             stop(paste("style ", style, "is not a current plotting option")))
		 } else style
	if(minarea <= 0)
		stop("hexagons cannot have a zero area, change \"minarea\"")
	if(maxarea > 1)
		warning("\"maxarea\" is greater than 1, hexagons may overplot")
	span = maxcount - mincount
	if(span < 0)
		stop("\"mincount\" must be greater than \"maxcount\"")
	
	# ------------------- Collect computing constants -------------------
	#
	count = bin$count
	bin.attrs = attributes(bin)
	xbins = bin.attrs$xbins
	tmp = hbin.xy(bin)
	good = mincount <= count & count <= maxcount
	xnew = tmp$x[good]
	ynew = tmp$y[good]
	count = count[good]
	rcount = range(count)
	to.nonzero.range = function(lim)
	{
		if(diff(lim) == 0)
			lim = lim * (1 + c(-1, 1) * 0.04)
		if(diff(lim) == 0)
			lim = c(-1, 1)
		lim
	}
	xlim = to.nonzero.range(bin.attrs$xlim)
	ylim = to.nonzero.range(bin.attrs$ylim)
	sx = xbins/diff(xlim)
	sy = xbins/(bin.attrs$aspectRatio*diff(ylim))
	#
	# ---------------- Transform Counts to Radius -------------------
	#
	switch(style.ok,
		centroids = ,
		lattice = ,
		grayscale = {
			if(span > 0)
				z = (count - mincount)/span
			else z = rep(1, length(count))
			area = minarea + z * (maxarea - minarea)
			area = pmin(area, maxarea)
			radius = sqrt(area)
		},
		nested.lattice = ,
		nested.centroids = {
			diffarea = maxarea - minarea
			step = 10^floor(log10(count))
			f = (count/step - 1)/9
			area = minarea + f * diffarea
			area = pmax(area, minarea)
			area = pmin(area, maxarea)
			radius = sqrt(area)
		}
		)
	#
	#
	# -------------------- Set Colors --------'nes--------------
	#
	switch(style.ok,
		nested.lattice = ,
		nested.centroids = {
                 colnum = floor(log10(count)) + 1
                 col=col.nest[pmin(colnum,length(col.nest))]},
		grayscale = {
			index = cut(count,bnds,labels=F,include.lowest=T)
			# set min and max counts
			# use omit color for out of bounds counts: zoomed legend
			col = ifelse(is.na(index),"red",col.regions[index])
		}
		)
	# 
	# ------------------ Construct a hexagon ------------------
	#
	# The inner and outer radius for hexagon in the scaled plot
	inner = 0.5
	outer = (2 * inner)/sqrt(3)
	# Now construct a point up hexagon symbol in data units
	dx = inner/sx
	dy = outer/(2 * sy)
	polyx = c(dx, dx, 0,  - dx,  - dx, 0, NA)
	polyy = c(dy,  - dy, -2 * dy,  - dy, dy, 2 * dy, NA)
	#
	#
	# ------------------ Full Cell  Plotting ------------------
	#
	switch(style.ok,
		grayscale = ,
		nested.lattice = ,
		nested.centroids = {
			n = length(xnew)
			seven = rep(7, n)
			pltx = rep(polyx, n) + rep(xnew, seven)
			plty = rep(polyy, n) + rep(ynew, seven)
			polygon(pltx, plty, density = density, border = border, col = col)
		}
		)
	if(style.ok == "grayscale")
		return(list(col = col.regions, bnds = bnds))
	#
	#
	# ---------------- Symbol Center Adjustments ----------------
	#
	if(style.ok == "centroids" | style.ok == "nested.centroids") {
		xcm = bin$xMean[good]
		ycm = bin$yMean[good]
		#
		# Store 12 angles around a circle and then replicate the first
		# The actual length for these vectors is determined by using
		# factor 'use' below
		k = sqrt(3)/2
		cosx = c(1, k, 0.5, 0, -0.5,  - k, -1,  - k, -0.5, 0, 0.5, k, 1)/sx
		siny = c(0, 0.5, k, 1, k, 0.5, 0, -0.5,  - k, -1,  - k, -0.5, 0)/sy
		#
		# Compute distances for differences after scaling into [0,size]x[0,aspect*size]
		# Then there are size hexagons on the x axis 
		dx = sx * (xcm - xnew)
		dy = sy * (ycm - ynew)
		dlen = sqrt(dx^2 + dy^2)
		#
		# Find the closest approximating direction of the 12 vectors above
		cost = ifelse(dlen > 0, dx/dlen, 0)
		tk = (6 * acos(cost))/pi
		tk = round(ifelse(dy < 0, 12 - tk, tk)) + 1
		#
		# Select the available length for the approximating vector
		hrad = ifelse(tk %% 2 == 1, inner, outer)
		#
		# Rad is either an inner or outer approximating radius.  
		# If dlen + hrad*radius <= hrad, move the center dlen units.
		# Else move as much of dlen as possible without overplotting.
		fr = pmin(hrad * (1 - radius), dlen)
		#
		# Compute the symbol centers
		# fr is the distance for the plot [0,xbins]x[0,aspect*xbins]
		# cosx and siny give the x and y components of this distance in data units
		xnew = xnew + fr * cosx[tk]
		ynew = ynew + fr * siny[tk]
	}
	#
	# ------------------ Size Hexagon Plotting -----------------
	# Scale the symbol by radius and add to the new center
	n = length(radius)
	switch(style.ok,
		centroids = ,
		lattice = {col = col.one},
		nested.lattice = ,
		nested.centroids = {
                colnum = colnum + 10
                col=col.nest[pmin(colnum,length(col.nest))]}
            )
	seven = rep(7, n)
	pltx = rep(polyx, n) * rep(radius, seven) + rep(xnew, seven)
	plty = rep(polyy, n) * rep(radius, seven) + rep(ynew, seven)
	expr = polygon(pltx, plty, density = density, border = border, col = col)
	cat(substitute(expr))
	eval(expr)
	return(list(col = col, bnds = bnds))
}

#_______________________________________________________________________________

hbin.legend = 
function(col, bnds, labels = format(round(bnds)), legend.lab = "Counts",
      height = 3, lcex = 1, width = 1, inner, style = 
	"grayscale", minarea = 0.04, maxarea = 0.8, maxcount,
      density = -1, border=border,
      col.nest=c("#D0D0D0","#C0C0FF","#C0F0C0","#FFC0C0","#A0FFFF",
                  "#FFA0FF","#FFFFA0","#000000","#000000","#000000",
                  "#606060","#0000C0","#00A000","#E00000","#007070",
                  "#700070","#707000"))
{
	#
	# -------------- Plotting -------------------------
	#
	switch(style,
		grayscale = {
			n <- max(2, length(bnds))
			spacing <- height/(n + 3)
			inner <- min(width/3.5, (sqrt(3) * spacing)/2)
			dx <- inner/2
			dy <- dx/sqrt(3)
			midx <- width/3
			textx <- (2 * width)/3
			tx <- c(dx, dx, 0,  - dx,  - dx, 0, NA) + midx
			ty <- c(dy,  - dy, -2 * dy,  - dy, dy, 2 * dy, NA)
			for(i in 1:(n - 1)) {
				polygon(tx, ty + i * spacing, col = col[i],
                        density = density, border = "#A0A0A0")
				text(textx, (i - 0.5) * spacing, labels[i])
			}
			text(textx, (n - 0.5) * spacing, labels[n])
			text(width/2, (n + 1.5) * spacing, "Counts", cex = 1.5 * lcex)
		}
		,
		centroids = ,
		lattice = {
			dx <- inner/2
			dy <- dx/sqrt(3)
			#
			# Need to scale 'bnds' to be between 0 and 1
			bnds.z <- (bnds - min(bnds))/diff(range(bnds))
			radius <- sqrt(minarea + (maxarea - minarea) * bnds.z)
			n <- length(radius)
			xmid <- width/3
			tx <- rep(c(dx, dx, 0,  - dx,  - dx, 0, NA), n)
			ty <- rep(c(dy,  - dy, -2 * dy,  - dy, dy, 2 * dy, NA), n)
			seven <- rep(7, n)
			inc <- height/(n + 3)
			y <- inc * 1:n
			textx <- rep((2 * width)/3, n)
			pltx <- tx * rep(radius, seven) + rep(xmid, 7 * n)
			plty <- ty * rep(radius, seven) + rep(y, seven)
			polygon(pltx, plty, density = density, border=border,col = col)
			text(textx, y, labels, cex = lcex)
			text(width/2, (n + 2) * inc, legend.lab, cex = 1.5 * lcex)
		}
		,
		nested.lattice = ,
		nested.centroids = {
			#
			#  x scaling
			numb <- cut(floor(width/inner), breaks = c(-1, 0, 2, 4))
			#Note: In old code top breaks=c(-1,0,2,4,8), numb<- 5 and size=1:9 
			if(is.na(numb)) numb <- 4
			switch(numb,
				return("not enough space for legend"),
				size <- 5,
				size <- c(1, 5, 9),
				size <- c(1, 3, 5, 7, 9))
			xmax <- length(size)
			diffarea <- maxarea - minarea
			radius <- sqrt(minarea + (diffarea * (size - 1))/9)
			txt <- as.character(size)
			#
			# y scaling
			lab <- c("Ones", "Tens", "Hundreds", "Thousands", "10 Thousands", "100 Thousands", "Millions",
				"10 Millions", "100 Millions", "Billions")
			power <- floor(log10(maxcount)) + 1
			dx <- inner/2
			dy <- dx/sqrt(3)
			if(height/power < 16 * dy)
				return("Not enough height for legend")
			xmid <- width/2
			x <- inner * (1:xmax - (1 + xmax)/2) + xmid
			n <- length(x)
			tx <- rep(c(dx, dx, 0,  - dx,  - dx, 0, NA), n)
			ty <- rep(c(dy,  - dy, -2 * dy,  - dy, dy, 2 * dy, NA), n)
			seven <- rep(7, n)
			yinc <- 16 * dy
			y <- rep(3 * dy - yinc, xmax)
			delta1 <- 4.5 * dy
			delta2 <- 4.5 * dy
			#  ____________________plotting______________________
			for(i in 1:power) {
				y <- y + yinc
				pltx <- tx + rep(x, seven)
				plty <- ty + rep(y, seven)
				polygon(pltx, plty, col = col.nest[i], border = F)
				pltx <- tx * rep(radius, seven) + rep(x, seven)
				plty <- ty * rep(radius, seven) + rep(y, seven)
				polygon(pltx, plty, col = col.nest[i + 10], border = F)
				text(x, y - delta1, txt, adj = 0.5)
				text(xmid, y[1] + delta2, lab[i], adj = 0.5)
			}
		}
		,
		stop(cat(paste("No definition for style", style, "\n"))))
}

#___________________________________________________________________________

hbin.xy = 
function(bin)
{
	bin.attrs = attributes(bin)
	xbins = bin.attrs$xbins
	xlim = bin.attrs$xlim
	ylim = bin.attrs$ylim
      shape = 1/bin.attrs$aspectRatio
	c3 = diff(xlim)/xbins
	c4 = (diff(ylim) * sqrt(3))/(2 * shape * xbins)
	jmax = xbins+1
	cell = bin$cell - 1
	i = cell %/% jmax
	j = cell %% jmax
	y = c4 * i + ylim[1]
	x = c3 * ifelse(i %% 2 == 0, j, j + 0.5) + xlim[1]
	data.frame(x = x, y = y)
}

#_________________________________________________________________

hbin.cell = function(x,y,xbins=20,aspectRatio = 1,xlim=range(x),ylim=range(y))
{

# In this version xbins is expected to be an integer.
# Some checks are made to 
 
# Here aspectRatio means width to height of the data rectangle
# My Splus routine used the term shape to mean height to width

# The function scales x into sx:  [0, xbins].
#                     y into sy:  [0, xbins/(aspectRatio*sqrt(3)]
# In this version xbins is forced to be an integer
                     
# The hexagons are oriented with vertical sides and width 1
# A hexagon is centered at (0,0) and its left edge is at -.5 
# A hexagon is centered at (xbins,0) and its right edge is at xbins+.5
# There are xbins+1 hexagons covering y=0
# The next row up of hexagons has the left edge at x =0,
#     and a "wasted" hexagon on the right

#  Calculation for the shift rows may have problem at the edges 
#     Observations x==xmin or x==xmax may go into the wrong cells.
#     This version takes a little extra time and 
#     converts i2 < 0 to 0 and i2 = xbins to xbins-1 
#     Other approaches may be better, include those not requiring
#     xbins to be an integer

# The hexagons, if drawn, go beyond the limits of the data.  
# Adjusting the x and y scales disportional to accomodate the full
# display of hexagons will lead to departures from the target
# aspectRatio. This may not be noticable or immportant

xbins=ceiling(xbins)
ncol=xbins+1
sx = xbins*(x-xlim[1])/diff(xlim)
sy = xbins*(y-ylim[1])/(diff(ylim)*sqrt(3)*aspectRatio)

# Discussion of y scaling
# This shrinks the hexagon rectangular centered lattice
# to square centered lattice
# Lower left for one square integer lattice is (0,0)
# Lower left for second is at (.5,.5)
# x bin centers will be 0,1,2, ..., xbins
# In hexagon lattice the hexagon center at x=.5
#     has y = 1/(2*sqrt(3)) + 2/(2*sqrt(3)) = sqrt(3)/2 = .5*sqrt(3)
# Dividing by sqrt(3)shrinks this to the target (.5,.5) 

# After scaling the data, the algorithm finds two closed lattice
# points, one from each lattice
# It then pick the closest of these two.   
#     A more careful treatment of ties could be considered   

# Speed
#     The version here use R function saves a lot of intermediate values
#          which is undesirable for large data sets.  More could
#          like be done for speed.  This is a get done
#          implementation          
#     Give extrema for x and y, data can be binned in one pass
#          The Splus version use Fortran code (that I wrote) for this
#          In roughly 89% of cases only one distance calculation is required.
#          Points close to or far from one lattice can be classifed
#          immediately.
#          Over the years, I played with versions to shave off a few 
#          operations or trade off multiplications and additions.     

i1=floor(sx+.5)
j1=floor(sy+.5)
i2=floor(sx)
j2=floor(sy)

# check for possible boundary problems
i2 = ifelse(i2<0,0,i2)
i2 = ifelse(i2==ncol,xbins,i2)

# Discussion
# Points  in (-.5,.5) x (-.5, .5) centered at (0, 0 ) map to i1=0, j1=0 
# Points in   (0, 1) x   (0 1)) centered at (.5 ,.5) map to i2=0, j2=0
# The bottom row of hexagons is indexed by i1,j1
# The next row up is shift to the right by .5 and indexed by i2,j2
# The rows are handle in pairs

# Here i indexes positions along the x axis
#      j indexes positions along the y axis
# Later the view switches to a 
#      matrix of hexagon cells with row and columns
#      Row are staggered 
#      imax indicating the number of rows
#      jmax indicating the number of columns 
 
k = ifelse((sx - i1)^2 + 3*(sy-j1)^2 < (sx-i2-.5)^2 + 3*(sy-j2 -.5)^2,
           2*j1*ncol+i1+1,(2*j2+1)*ncol+i2+1) 
return(k)
} 

#_________________________________

hbin.bnds =
function(rang,n){
tmp = pretty(rang,n)
tmp = tmp[tmp >= rang[1] & tmp <= rang[2]] 
return(sort(unique(c(tmp,rang))))
}
