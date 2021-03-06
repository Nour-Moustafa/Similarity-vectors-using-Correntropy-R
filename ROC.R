
### DNS and HTTP techniques
par(mfrow=c(1,2))
##### DNS data source UNSW

dat1 <- data.frame(TPR = c(85, 89, 94.15),FPR = c( 5.22,22,60))
op <- par(xaxs = "i", yaxs = "i")
plot(TPR ~ FPR, data = dat1, xlim = c(0,100), ylim = c(0,100), type = "n",main="UNSW-NB15 - DNS data source",xlab="False Positive Rate %",ylab="Detection Rate %")
with(dat1, lines(c(0, FPR, 100), c(0, TPR, 100), type = "o", lty=1,lwd=2, col= 4,pch=NA_integer_))
#abline(0, 1)
par(op)

# add points
dat2 <- data.frame(TPR = c(73, 87, 90.78),FPR = c( 8.25,40,55))
points(TPR ~ FPR, dat2,lwd=2,col="blue",lty=2,pch=NA_integer_)
with(dat2, lines(c(0, FPR, 100), c(0, TPR, 100), type = "o", lty=2,lwd=2, col=2,pch=NA_integer_))

# add points
dat3 <- data.frame(TPR = c(78, 88, 92.61),FPR = c( 7.78,36,59))
points(TPR ~ FPR, dat3,lwd=2,col="blue",lty=2,pch=NA_integer_)
with(dat3, lines(c(0, FPR, 100), c(0, TPR, 100), type = "o", lty=3,lwd=2, col=3,pch=NA_integer_))

dat4 <- data.frame(TPR = c(95, 98, 99),FPR = c( 1.38,50,78))
points(TPR ~ FPR, dat4,lwd=2,col="blue",lty=2,pch=NA_integer_)
with(dat4, lines(c(0, FPR, 100), c(0, TPR, 100), type = "o", lty=4,lwd=2, col=6,pch=NA_integer_))

legend("bottomright", legend = c("DT","NB","ANN", "Ensemble method"),lwd=2, col=c(4,2,3,6), lty=c(1,2,3,4))


##### DNS data source NIMS


dat1 <- data.frame(TPR = c(87, 92, 95.02),FPR = c(4.19,20,55))
op <- par(xaxs = "i", yaxs = "i")
plot(TPR ~ FPR, data = dat1, xlim = c(0,100), ylim = c(0,100), type = "n",main="NIMS - DNS data source",xlab="False Positive Rate %",ylab="Detection Rate %")
with(dat1, lines(c(0, FPR, 100), c(0, TPR, 100), type = "o", lty=1,lwd=2, col= 4,pch=NA_integer_))
#abline(0, 1)
par(op)

# add points
dat2 <- data.frame(TPR = c(69, 84, 87.15),FPR = c(11.15,50,63))
points(TPR ~ FPR, dat2,lwd=2,col="blue",lty=2,pch=NA_integer_)
with(dat2, lines(c(0, FPR, 100), c(0, TPR, 100), type = "o", lty=2,lwd=2, col=2,pch=NA_integer_))

# add points
dat3 <- data.frame(TPR = c(80, 89, 93.47),FPR = c( 6.76,35,57))
points(TPR ~ FPR, dat3,lwd=2,col="blue",lty=2,pch=NA_integer_)
with(dat3, lines(c(0, FPR, 100), c(0, TPR, 100), type = "o", lty=3,lwd=2, col=3,pch=NA_integer_))

dat4 <- data.frame(TPR = c(93, 95.5, 97.38),FPR = c( 2.01,52,75))
points(TPR ~ FPR, dat4,lwd=2,col="blue",lty=2,pch=NA_integer_)
with(dat4, lines(c(0, FPR, 100), c(0, TPR, 100), type = "o", lty=4,lwd=2, col=6,pch=NA_integer_))

legend("bottomright", legend = c("DT","NB","ANN", "Ensemble method"),lwd=2, col=c(4,2,3,6), lty=c(1,2,3,4))

##### HTTP data source- UNSW
par(mfrow=c(1,2))

dat1 <- data.frame(TPR = c(86, 90, 96.34),FPR = c( 3.43,25,57))
op <- par(xaxs = "i", yaxs = "i")
plot(TPR ~ FPR, data = dat1, xlim = c(0,100), ylim = c(0,100), type = "n",main="UNSW-NB15 - HTTP data source",xlab="False Positive Rate %",ylab="Detection Rate %")
with(dat1, lines(c(0, FPR, 100), c(0, TPR, 100), type = "o", lty=1,lwd=2, col= 4,pch=NA_integer_))
#abline(0, 1)
par(op)

# add points
dat2 <- data.frame(TPR = c(76, 88, 95.25),FPR = c( 4.18,30,60))
points(TPR ~ FPR, dat2,lwd=2,col="blue",lty=2,pch=NA_integer_)
with(dat2, lines(c(0, FPR, 100), c(0, TPR, 100), type = "o", lty=2,lwd=2, col=2,pch=NA_integer_))

# add points
dat3 <- data.frame(TPR = c(79, 90, 95.53),FPR = c( 4.26,32,58))
points(TPR ~ FPR, dat3,lwd=2,col="blue",lty=2,pch=NA_integer_)
with(dat3, lines(c(0, FPR, 100), c(0, TPR, 100), type = "o", lty=3,lwd=2, col=3,pch=NA_integer_))

dat4 <- data.frame(TPR = c(90, 93, 97.02),FPR = c( 2.58,20,40))
points(TPR ~ FPR, dat4,lwd=2,col="blue",lty=2,pch=NA_integer_)
with(dat4, lines(c(0, FPR, 100), c(0, TPR, 100), type = "o", lty=4,lwd=2, col=6,pch=NA_integer_))

legend("bottomright", legend = c("DT","NB","ANN", "Ensemble method"),lwd=2, col=c(4,2,3,6), lty=c(1,2,3,4))


##### HTTP data source- NISM


dat1 <- data.frame(TPR = c(84, 88, 95.92),FPR = c( 4.65,22,50))
op <- par(xaxs = "i", yaxs = "i")
plot(TPR ~ FPR, data = dat1, xlim = c(0,100), ylim = c(0,100), type = "n",main="NIMS - HTTP data source",xlab="False Positive Rate %",ylab="Detection Rate %")
with(dat1, lines(c(0, FPR, 100), c(0, TPR, 100), type = "o", lty=1,lwd=2, col= 4,pch=NA_integer_))
#abline(0, 1)
par(op)

# add points
dat2 <- data.frame(TPR = c(77, 85, 92.19),FPR = c( 6.87,31,61))
points(TPR ~ FPR, dat2,lwd=2,col="blue",lty=2,pch=NA_integer_)
with(dat2, lines(c(0, FPR, 100), c(0, TPR, 100), type = "o", lty=2,lwd=2, col=2,pch=NA_integer_))

# add points
dat3 <- data.frame(TPR = c(78, 89, 94.34),FPR = c( 5.13,33,59))
points(TPR ~ FPR, dat3,lwd=2,col="blue",lty=2,pch=NA_integer_)
with(dat3, lines(c(0, FPR, 100), c(0, TPR, 100), type = "o", lty=3,lwd=2, col=3,pch=NA_integer_))

dat4 <- data.frame(TPR = c(90.5, 94, 97.95),FPR = c( 2.15,19,39))
points(TPR ~ FPR, dat4,lwd=2,col="blue",lty=2,pch=NA_integer_)
with(dat4, lines(c(0, FPR, 100), c(0, TPR, 100), type = "o", lty=4,lwd=2, col=6,pch=NA_integer_))

legend("bottomright", legend = c("DT","NB","ANN", "Ensemble method"),lwd=2, col=c(4,2,3,6), lty=c(1,2,3,4))




