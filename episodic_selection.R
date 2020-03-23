#Daniel Shriner
#3/23/202
#episodic positive selection

h <- 0.5 #degree of dominance, 0.5 for additive model
s <- 0.214 #selection coefficient
loss <- vector("numeric",length(s)) #used to estimate probability that the advantageous allele is lost
freq <- vector("numeric",length(s)) #used to estimate final allele frequency, conditional on allele not being lost
N <- 19602 #effective population size
reps <- 10000
for (k in 1:length(s)) {
	res <- vector("numeric",reps)
	for (i in 1:reps) {
		p <- 2.1e-3 #initial allele frequency
		for (g in 1:8) { #generations for epoch 2 - the first plague pandemic
			if (p>0) {
				p <- mean(rbinom(2*N,2,(p^2*(1+s[k])+p*(1-p)*(1+h*s[k]))/(p^2*(1+s[k])+2*p*(1-p)*(1+h*s[k])+(1-p)^2)))/2
			} else {
				p <- 0
			}
		}
		for (g in 1:21) { #generations for epoch 3 - the interplague period
			if (p>0) {
				p <- mean(rbinom(2*N,2,p))/2
			} else {
				p <- 0
			}
		}
		for (g in 1:19) { #generations for epoch 4 - the second plague pandemic
			if (p>0) {
				p <- mean(rbinom(2*N,2,(p^2*(1+s[k])+p*(1-p)*(1+h*s[k]))/(p^2*(1+s[k])+2*p*(1-p)*(1+h*s[k])+(1-p)^2)))/2
			} else {
				p <- 0
			}
		}
		for (g in 1:5) { #generations for epoch 5 - modern times
			if (p>0) {
				p <- mean(rbinom(2*N,2,p))/2
			} else {
				p <- 0
			}
		}
		res[i] <- p
	}
	loss[k] <- length(which(res==0))/reps
	freq[k] <- mean(res[res!=0])
}
