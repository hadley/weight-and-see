weighted_hist <- function(x, w=rep(1, length=length(x)), breaks="Sturges") {
	breaks <- hist_breaks(x, breaks)
	nbreaks <- length(breaks) - 1

	counts <- rep(NA,nbreaks)
	for(bin in 1:nbreaks) {
		indices <- which(x >= breaks[bin] & x < breaks[bin+1])
		if(length(indices)) counts[bin] <- sum(w[indices])
	}

	data.frame(left=breaks[-length(breaks)], width=diff(breaks), values=counts)
}

hist_breaks <- function(x, breaks="Sturges") {
	# if a break computing function name is passed
	if(is.character(breaks))
		breaks <- do.call(paste("nclass",breaks,sep=".",collapse=""),list(x))
	
	# if breaks is numeric
	if(is.numeric(breaks)) {
		# if just the number of breaks is passed
		if(length(breaks) == 1) {
			nbreaks<-breaks
			breakinc<-diff(range(x))/nbreaks
			breaks<-c(min(x),rep(breakinc,nbreaks))
			breaks<-cumsum(breaks)
		} else {
			# otherwise assume that breaks specifies the breakpoints
			nbreaks<-length(breaks)-1
		}
		if(diff(range(breaks)) < diff(range(x)))
			warning("Not all values will be included in the histogram")
		breaks[nbreaks+1]<-breaks[nbreaks+1]+1
	}
	# make sure that the last break is greater than the maximum value
	breaks
}