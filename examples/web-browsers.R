
xx <- seq(1,60,length=1000)
pdf("../book/graphics/bootpopdist.pdf", width=10, height=4)
par(mai=c(.0,.0,.1,.0))
plot(xx, dgamma(xx,5,.2),type="l", lwd=3, xaxt="n",yaxt="n",bty="n",xlab="",ylab="")
dev.off()

browser = read.csv("web-browsers.csv")

pdf("../book/graphics/web-spendhist.pdf", width=4.7, height=4)
par(mai=c(.8,.8,.1,.1))
hist(log(browser$spend), freq=FALSE,
	xaxt="n", main="", xlab="total online spend", col=8, border="grey90")
lgrid = c(1,10,100,1000,10000,100000)
axis(1, at=log(lgrid), labels=sprintf("%.0e",lgrid))
dev.off()

nrow(browser)
mean(browser$spend)
var(browser$spend)/nrow(browser)

xbar <- mean(browser$spend)
xbse <-  sd(browser$spend)/sqrt(nrow(browser))

xx <- seq(1650,2250,length=1000)

pdf("../book/graphics/web-spendnormal.pdf", width=4, height=4)
par(mai=c(.9,.8,.2,.2))
plot(xx, dnorm(xx, xbar, xbse), type="l", col="royalblue", lwd=1.5,
	xlab="average total online spend", ylab="density")
dev.off()

B <- 1000
mub <- c()
for (b in 1:B){
	samp_b = sample.int(nrow(browser), replace=TRUE)
	mub <- c(mub, mean(browser$spend[samp_b]))
}

pdf("../book/graphics/web-spendboot.pdf", width=4, height=4)
par(mai=c(.8,.8,.2,.2))
hist(mub, main="", xlab="average total online spend", 
	col=8, border="grey90", freq=FALSE)
lines(xx, dnorm(xx, xbar, xbse), col="royalblue", lwd=1.5)
dev.off()

summary( glm( log(spend) ~ broadband + anychildren, data=browser) )


B <- 1000
betas <- c()
for (b in 1:B){
	samp_b = sample.int(nrow(browser), replace=TRUE)
	reg_b <- glm(log(spend) ~ broadband + anychildren, data=browser[samp_b,])
	betas <- rbind(betas, coef(reg_b))
}
head(betas)

cor(betas[,"broadband"], betas[,"anychildren"])

xx <- seq(min(betas[,2]),max(betas[,2]),length=100)
pdf("../book/graphics/web-regboot.pdf", width=4, height=4)
par(mai=c(.8,.8,.2,.2))
hist(betas[,2], main="", xlab="broadband coefficient", 
	col=8, border="grey90", freq=FALSE)
lines(xx, dnorm(xx, 0.55285, 0.04357), col="royalblue", lwd=1.5)
dev.off()

pdf("../book/graphics/web-regmultboot.pdf", width=4, height=4)
par(mai=c(.8,.8,.2,.2))
hist(exp(betas[,2]), main="", xlab="broadband multiplier", 
	col=8, border="grey90", freq=FALSE)
dev.off()


spendy <- glm( log(spend) ~ .-id, data=browser) 
round(summary(spendy)$coef,2)
pval <- summary(spendy)$coef[-1,"Pr(>|t|)"]

pdf("../book/graphics/web-fdr.pdf", width=4, height=4)
par(mai=c(.8,.8,.2,.2))
plot(sort(pval), bty="n", xlab="rank", ylab="p-values")
abline(a=0, b=.25/9)
points(sort(pval)[1:8], col=2, pch=20)
dev.off()


pdf("../book/graphics/uniformpdf.pdf", width=4, height=4)
par(mai=c(.8,.8,.2,.2))
plot(c(-1,0,0,1,1,2), c(0,0,1,1,0,0), ylim=c(0,1.5), xlim=c(-0.1,1.1),
	type="l", bty="n", xlab="U", ylab="probability density", main = "uniform pdf")
dev.off()


pdf("../book/graphics/uniformranks.pdf", width=4, height=4)
par(mai=c(.8,.8,.2,.2))
plot(1:9, (1:9)/10, ylim=c(0,1),
	pch=16, col="black", bty="n", ylab="p-value", 
	xlab="order", main = "p-value order statistics")
points(1:9, sort(pval), pch=17, col=rgb(0,0,1,.5))
legend("topleft", bty="n",
	legend=c("expectation under null","observed"), pch=c(16,17),
	col=c("black",rgb(0,0,1,.5)))
dev.off()

