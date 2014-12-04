panelFill = function(col="#D0D0D0",border=NA,...)
{
	xy <- par("usr")
	polygon(xy[c(1, 2, 2, 1)], xy[c(3, 3, 4, 4)],col=col,border=border,xpd=T,...)
}

panelGrid = function(x = NULL, y = NULL, col = 2, lwd = 1, lty = 1)
{
	if(!is.null(x))
		abline(v = x, lwd = lwd, lty = lty, col = col)
	if(!is.null(y))
		abline(h = y, lwd = lwd, lty = lty, col = col)
}

panelInbounds = function(bnds)
{
	grid = pretty(bnds)
	return(grid[bnds[1] < grid & grid < bnds[2]])
}


panelLayout = function(nrow = 1, ncol = 1, leftMargin = 0, rightMargin = 0,
      topMargin = 1, bottomMargin = 0,
      borders = rep(0.5, 4), colSize = 1, rowSize = 1, 
	# The figure borders are left, right, top, bottom
	colSep = 0, rowSep = 0, pad = NULL)
{
# Note fig matrices rounded to 6 places in an effort of avoid a R problem with fig when
#  values appear in exponential notation.

	oldpar = par()
	din = oldpar$din
	din.x = din[1]
	din.y = din[2]

	plotX = din.x - borders[1] - borders[2] - leftMargin - rightMargin
	plotY = din.y - borders[3] - borders[4] - bottomMargin - topMargin
	xbnds = c(0, leftMargin, leftMargin + plotX, leftMargin + plotX + 
		rightMargin) + borders[1]  #shift all by the left border
	ybnds = c(0, bottomMargin, bottomMargin + plotY, bottomMargin + 
		plotY   + topMargin) + borders[4] # shift all by bottom border
	fig.scale = c(din.x, din.x, din.y, din.y)	

	leftfig = c(xbnds[1] - borders[1], xbnds[2] + borders[2], ybnds[1] - 
		borders[4], ybnds[4] + borders[3])
	rightfig = c(xbnds[3] - borders[1], xbnds[4] + borders[2], ybnds[1] - 
		borders[4], ybnds[4] + borders[3])
	topfig = c(xbnds[1] - borders[1], xbnds[4] + borders[2], ybnds[3] - 
		borders[4], ybnds[4] + borders[3])
	botfig = c(xbnds[1] - borders[1], xbnds[4] + borders[2], ybnds[1] - 
		borders[4], ybnds[2] + borders[3])
	colSep = panelLengthen(colSep, ncol + 1)
	rowSep = panelLengthen(rowSep, nrow + 1)
	if(is.null(pad)) pad = c(borders[1] + colSep[1] + leftMargin, 
			borders[2] + colSep[ncol + 1] + rightMargin, borders[
			3] + rowSep[1] + topMargin, borders[4] + rowSep[
			nrow + 1] + bottomMargin)	
	# The borders should align around the edge.
	colSep = cumsum(colSep)
	rowSep = cumsum(rowSep)
	plotX = plotX - colSep[ncol + 1]
	plotY = plotY - rowSep[nrow + 1]
	relx = panelLengthen(colSize, ncol)
	rely = panelLengthen(rowSize, nrow)
	relx = relx/sum(relx)
	rely = rely/sum(rely)
	xinc = plotX * cumsum(c(0, relx))
	yinc = plotY * cumsum(c(0, rely))
	fig = matrix(0, nrow = nrow * ncol, ncol = 4)
	k = 0
	for(i in 1:nrow) {
		for(j in 1:ncol) {
			k = k + 1
			fig[k, 1] = xbnds[2] + xinc[j] + colSep[j] - pad[1]
			fig[k, 2] = xbnds[2] + xinc[j + 1] + colSep[j] + pad[2]
			fig[k, 4] = ybnds[3] - yinc[i] - rowSep[i] + pad[3]
			fig[k, 3] = ybnds[3] - yinc[i + 1] - rowSep[i] - pad[4]
		}
	}
	labfig = rbind(leftfig, rightfig, topfig, botfig)
	fig = abs(t(t(fig)/fig.scale))
	labfig = t(t(labfig)/fig.scale)	
	# coltabs are in inches and start inside the left border
	coltabs = cbind(c(0, colSep + xinc + leftMargin), leftMargin + c(0,
		colSep) + c(xinc, xinc[length(xinc)] + rightMargin))	
	# rowtabs are in inches and start below the upper border
      # yinc is padded with a leading 0.  
	rowtabs = cbind(c(ybnds[3], ybnds[3] - rowSep - c(yinc[-1], yinc[
		nrow + 1] + bottomMargin)), c(ybnds[4], ybnds[3] - rowSep - 
		yinc)) - borders[4]
		
	list(dim = c(nrow, ncol), datfig = round(fig,6), labfig = round(labfig,6), brd = borders,
		pad = pad, coltabs = coltabs, rowtabs = rowtabs, 
		figsize = c(din.x, din.y))
}

panelLengthen = function(x, n=1)
{
   if(n<1)stop("panelLengthen: invalid required length")       
   if(length(x)==0)return(rep(0,n))
   newx = rep(x,ceiling(n/length(x)))
   length(newx) = n
   return(newx)
}

panelOutline =  function(col = "black", lwd = 1, lty = 1)
{
	xy <- par("usr")
	polygon(xy[c(1, 2, 2, 1)], xy[c(3, 3, 4, 4)],density=0,col=col, xpd=T)
}

panelScale = function(rx = c(0, 1), ry = c(0, 1),firstp=FALSE, inches = FALSE)
{
	if(inches) {
		pin = par("pin")
		rx = c(0, pin[1])
		ry = c(0, pin[2])
	}
	warn = unlist(options('warn'))
      options(warn=-1)
      par(new=TRUE)
      options(warn=warn)
	plot(rx, ry, type = "n", axes = F, xaxs = "i", yaxs = "i", 
				xlab = "", ylab = "", main = "")
	return(list(rx = rx, ry = ry))
}


panelSelect = function(layout, i = 1, j = 1, margin = NULL)
{
	dim = layout$dim
	if(i > dim[1] || j > dim[2])
		fatal("Dimension error")
	if(is.null(margin)) {
		k = dim[2] * (i - 1) + j
		par(fig = layout$datfig[k,  ], 
		    mai = layout$pad[c(4, 1, 3, 2)] )
	}
	else {
		vec = 1:4
		nam = c("left", "right", "top", "bottom", "bot")
		ind = match(margin, nam)
		if(is.na(ind))
			fatal("Bad label region name")
		if(ind == 5)   ind = 4
		par(fig = layout$labfig[ind,  ],
				mai = layout$brd[c(4, 1, 3, 2)] )
	}
#	"done"
}

