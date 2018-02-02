library(gamlr)

load("dominicks-beer.rda")

# how many upcs?
length( upctab <- table(wber$UPC) )

# check data types
sapply(wber, class)

# create priceperoz
wber$lp <- log(12*wber$PRICE/upc[wber$UPC,"OZ"])

# numeric matrices for week, store, item
wber$s <- factor(wber$STORE)
wber$u <- factor(wber$UPC)
wber$w <- factor(wber$WEEK)

xs <- sparse.model.matrix( ~ s-1, data=wber)
xu <- sparse.model.matrix( ~ u-1, data=wber)
xw <- sparse.model.matrix( ~ w-1, data=wber)
controls <- cBind(xs, xu, xw, descr[wber$UPC,]) 

# smallbeer
set.seed(5807)
ss <- sample.int(nrow(wber),1e4)

# all together
coef( margfit <- lm(log(MOVE) ~ lp, data=wber[ss,]) )

# mle
mlefit <- gamlr(x=cBind(lp=wber$lp,controls)[ss,], y=log(wber$MOVE)[ss], lambda.start=0)
coef(mlefit)[1:2,]

# naive lasso
naivefit <- gamlr(x=cBind(lp=wber$lp,controls)[ss,], y=log(wber$MOVE)[ss], free=1, standardize=FALSE)
coef(naivefit)[1:2,]

# double ML
source("orthoML.R")
dreg <- function(x,d){ 
	gamlr(x, d, standardize=FALSE, lmr=1e-5) }

yreg <- function(x,y){ 
	gamlr(x, y, standardize=FALSE, lmr=1e-5) }

resids <- orthoPLTE( x=controls[ss,], d=wber$lp[ss], y=log(wber$MOVE)[ss], dreg=dreg, yreg=yreg, nfold=5)

# full data mle fit
fullfit <- gamlr(x=cBind(lp=wber$lp,controls), y=log(wber$MOVE), lambda.start=0)
coef(fullfit)[1:2,]

##############  heterogeneity

lpxu <- xu*wber$lp
colnames(lpxu) <- paste("lp",colnames(lpxu),sep="")

# mle with all upcs
mlehte <- gamlr(x=cBind(controls,lpxu)[ss,], y=log(wber$MOVE)[ss], lambda.start=0)
gammle <- coef(mlehte)[-(1:(ncol(controls)+1)),]
pdf("smallbeer-mle.pdf", width=4, height=4)
par(mai=c(.9,.9,.1,.1))
hist(gammle, main="", xlab="elasticity", col="pink", freq=FALSE)
dev.off()
sort(gammle)[1:4]

xhte <- cBind(1,xu,descr[wber$UPC,])
colnames(xhte)[1] <- "(baseline)"
d <- xhte*wber$lp
colnames(d) <- paste("lp",colnames(d),sep=":")
oxhte <- xhte[match(rownames(upc),wber$UPC),]

# naive fit
naivehte <- gamlr(x=cBind(d,controls)[ss,], y=log(wber$MOVE)[ss], free=1, standardize=FALSE)
gamnaive <- drop(oxhte%*%coef(naivehte)[2:(ncol(d)+1),])

pdf("smallbeer-naive.pdf", width=4, height=4)
par(mai=c(.9,.9,.1,.1))
hist(gamnaive, main="", xlab="elasticity", col="lightyellow", freq=FALSE)
dev.off()

# double ML
dmlhte <- gamlr(x=xhte[ss,]*resids$dtil, y=resids$ytil, free=1, standardize=FALSE)
coef(dmlhte)[1:2]
gamdml <- drop(oxhte%*%coef(dmlhte)[-1,])
pdf("smallbeer-dml.pdf", width=4, height=4)
par(mai=c(.9,.9,.1,.1))
hist(gamdml, main="", xlab="elasticity", col="lightblue", freq=FALSE)
dev.off()

# fullhte 
fullhte <- gamlr(x=cBind(controls,lpxu), y=log(wber$MOVE), lambda.start=0)
gamfull <- coef(fullhte)[-(1:(ncol(controls)+1)),]

pdf("smallbeer-full.pdf", width=4, height=4)
par(mai=c(.9,.9,.1,.1))
hist(gamfull, main="", xlab="elasticity", ,
			 col="darkgrey", freq=FALSE)
dev.off()

pdf("smallbeer-compare.pdf", width=8, height=4)
par(mai=c(.9,.9,.1,.1), mfrow=c(1,3))
plot(gamfull, gammle, pch=21, bg=8, xlab="fulldata MLE", ylab="subsample MLE")
abline(a=0, b=1, lty=2)
plot(gamfull, gamnaive, pch=21, bg=8, xlab="fulldata MLE", ylab="subsample Naive ML")
abline(a=0, b=1, lty=2)
plot(gamfull, gamdml, pch=21, bg=8, xlab="fulldata MLE", ylab="subsample DML")
abline(a=0, b=1, lty=2)
dev.off()