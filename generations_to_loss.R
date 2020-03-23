#Daniel Shriner
#3/23/2020
#generations to loss under random genetic drift

N <- 19602 #effective population size
reps <- 10000
loss <- vector("numeric",reps)
for (i in 1:reps) {
	p <- 1/(2*N)
	g <- 0
	while (p!=0) {
		p <- mean(rbinom(2*N,2,p))/2
		g <- g + 1
	}
	loss[i] <- g
}
quantile(loss,probs=c(0.025,0.5,0.975),na.rm=TRUE)
