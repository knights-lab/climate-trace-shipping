"plot.fitted.vs.observed.colored" <- function(x,y,yhat,color.by='year.built'){
  col.ix <- cut(x[,color.by],br=11)
  library('RColorBrewer')
  cols <- brewer.pal(11,'RdBu')
  cols <- sprintf('%s77',cols)
  print(cols)
  plot(y,yhat,
         pch=21,col=NA,
         bg=cols[col.ix],
         xlab='Observed',
         ylab='Predicted',
         cex.main=1.1)
}


"plot.co2.vs.size.by.ship.type" <- function(color.by='year.built'){
	col.ix <- cut(x[,color.by],br=11)
	library('RColorBrewer')
	cols <- brewer.pal(11,'RdBu')

	par(mfrow=c(4,4),mar=c(4,4,1.2,.4))
	for(shiptype in levels(x$Ship.type)){
		ix <- x$Ship.type == shiptype
		plot(x[ix,'deadweight.tons'],
			log10(x[ix,'kg.CO2.nm.2020']),
			pch=21,col=NA,
			#bg='#00000022',
			bg=cols[col.ix[ix]],
			xlab='Deadweight Tons',
			ylab='log10(KG CO2/NM)',
			main=shiptype,
			cex.main=1.1)
	}
}


"is.outlier" <- function(x,iqr.width=1.5){
	stats <- summary(x)
	q1 <- stats[2]
	q3 <- stats[5]
	iqr <- q3 - q1
	return((x < q1 - iqr.width*iqr) | (x > q3 + iqr.width*iqr))
}


