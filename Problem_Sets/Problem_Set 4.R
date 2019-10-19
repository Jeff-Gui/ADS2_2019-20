## generate linear synthetic data
sn <- 1000
# xp <- runif(sn)
xp <- rnorm(sn,0,5)
noise_rate <- 0.5
yp <- 10.8 * xp + 0.6
spl <- sample(1:sn, noise_rate * sn)
noise <- rnorm(noise_rate * sn, 0, 10) # (if default, sd = 1)
for (i in 1:(noise_rate * sn)){
  yp[spl[i]] <- yp[spl[i]] + noise[i]
}
plot(xp,yp,pch=20,col="thistle",cex=0.4) #cex: change the size of spots

## generate synthetic DNA seq
lth <- 100
seq <- ""
for (j in 1:lth){
 seq <- paste(seq,sample(c('A','T','C','G'),1, p=c(0.2,0.2,0.3,0.3)), sep="")
}
library(RBioinf)
randDNA(100)
DNA <- paste0(sample(c('A','T','C','G'),1000,rep=TRUE),collapse='')
