#Daniel Shriner
#3/23/2020
#sojourn time to specified allele frequency under random genetic drift

N <- 19602 #effective population size
reps <- 100000
gens <- vector("numeric",reps)
for (i in 1:reps) {
	p <- 1/(2*N)
	g <- 0
	while (p>0 & p<2.1e-3) { #while the allele has not been lost and not yet reached the target frequency
		p <- mean(rbinom(2*N,2,p))/2
		g <- g + 1
	}
	if (p==0) {
		time[i] <- NA
	} else {
		time[i] <- g
	}
}
quantile(g,prob=c(0.025,0.5,0.975),na.rm=TRUE)
