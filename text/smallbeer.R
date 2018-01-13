## small beer dataset
beer <- read.csv("smallbeer.csv", 
	colClasses=c(rep("factor",3),rep("numeric",2)))
print(nrow(beer))

## dumbfit
coef( allforone <- lm(log(units) ~ log(price), data=beer) )
alldiff <- lm(log(units) ~ log(price)*item - log(price), data=beer)
#pdf("../book/graphics/smallbeer-alldiff.pdf", width=4, height=4)
#par(mai=c(.9,.9,.1,.1))
hist(coef(alldiff)[345:688], main="", xlab="elasticity", col=8)
#dev.off()

## build some regression designs
library(gamlr)
xitem <- sparse.model.matrix(~item-1, data=beer)
xweek <- sparse.model.matrix(~week-1, data=beer)

# parse the item description text 
library(tm)
descr <- Corpus(VectorSource(as.character(beer$description)))
xtext <- DocumentTermMatrix(descr)
xtext <- sparseMatrix(i=xtext$i,j=xtext$j,x=as.numeric(xtext$v>0), # convert from stm to Matrix format
              dims=dim(xtext),dimnames=dimnames(xtext))
xtext[1:5,1:6]
xtext[1,xtext[1,]!=0]

# wrap them together
xx <- cBind(xweek, xitem, xtext)
xtreat <- cBind(1,xtext,xweek)
colnames(xtreat)[1] <- "(baseline)"

# single elasticity
dx <- as.matrix(cbind(log(beer$price),xx))
naiveglm <- glm(log(beer$units) ~ dx)
summary(naiveglm)$coef[1:3,]

# naive ml
xx <- cBind(xweek, xitem, xtext)
d <- log(beer$price)
naiveml <- gamlr(x=cBind(d, xx), y=log(beer$units))
coef(naiveml)[2,]

# double ML
source("orthoML.R")
dreg <- function(x,d){ 
	gamlr(x, d, standardize=FALSE, lmr=1e-5) }

yreg <- function(x,y){ 
	gamlr(x, y, standardize=FALSE, lmr=1e-5) }

resids <- orthoPLTE( x=xx, d=d, y=log(beer$units), 
				dreg=dreg, yreg=yreg, nfold=5)


# orthogonal ML
hte <- gamlr( resids$dtil*cBind(baseline=1, xtext), resids$ytil, 
				free=1, standardize=FALSE )
B <- round(coef(hte)[-1,],2)
B["baseline"]
B["baseline"]+B["draught"]
B["baseline"]+B["bud"]

pricelasso <- gamlr(xx, log(beer$price),  lmr=1e-4)
plot(pricelasso)
lphat <- predict(pricelasso,xx)
causeml <- gamlr(cBind(logprice=log(beer$price), lphat, xx), 
				log(beer$units), lmr=1e-4,
				varweight=c(sdlp*2, rep(1, ncol(xx)+1)),
				standardize=FALSE, free=2, gamma=10)
coef(causeml)[2,]

# double ML
train <- sample.int(nrow(xx), nrow(xx)/2)
salesml <- gamlr(xx[train,], log(beer$units)[train], standardize=FALSE, lmr=1e-4)
priceml <- gamlr(xx[train,], log(beer$price)[train], standardize=FALSE, lmr=1e-4)
ytilde <- drop(log(beer$units)[-train] - predict(salesml, xx[-train,]))
dtilde <- drop(log(beer$price)[-train] - predict(priceml, xx[-train,]))
summary( glm(ytilde ~ dtilde) )


# double ML
n <- nrow(xx)
I <- split(1:n,rbinom(n,1,0.5))
d <- log(beer$price)
y <- log(beer$units)
dtil <- matrix(nrow=n, ncol=1)
ytil <- rep(NA, n)
for(b in 1:length(I)){
	print(b)
	dreg <- gamlr(xx[I[[b]],], d[I[[b]]],  standardize=FALSE, lmr=1e-4)
	yreg <- gamlr(xx[I[[b]],], y[I[[b]]], standardize=FALSE, lmr=1e-4)
	dtil[-I[[b]],] <- drop(d[-I[[b]]] -  predict(dreg,xx[-I[[b]],],type="response"))
	ytil[-I[[b]]] <- drop(y[-I[[b]]] - predict(yreg,xx[-I[[b]],],type="response"))
}
summary( lm(ytil ~ dtil) )$coef

# fit the naive ML
naiveml <- gamlr(x=cBind(xtreat*log(beer$price), xx), 
				y=log(beer$units),
				free=1, standardize=FALSE)
naiveb <- coef(naiveml)
zebra <- match(levels(beer$item),beer$item)
xtest <- xtext[zebra,]
rownames(xtest) <- beer$description[zebra]
naiveel <- drop(naiveb[2,1] + xtest%*%naiveb[(1:ncol(xtext))+2,] )
hist(naiveel)

# Orthogonal ML insteal

# OML steps 1-2
pfit <- gamlr(x=xx, y=log(beer$price), lmr=1e-5, standardize=FALSE)
qfit <- gamlr(x=xx, y=log(beer$units), lmr=1e-5, standardize=FALSE)
# Calculate residuals
lpr <- drop(log(beer$price) - predict(pfit, xx))
lqr <- drop(log(beer$units) - predict(qfit, xx))
# Run 3rd ML step to get gammas
ofit <- gamlr(x=(lpr*xtreat), y=lqr, standardize=FALSE, free=1)
gams <- coef(ofit)[-1,]

# translate into elasticities and plot
el <- drop(gams[1] + xtest%*%gams[(1:ncol(xtext))+1])
hist(el, xlab="OML elasticities", xlim=c(-6,1), col="lightblue", main="",breaks=7)

# high and low sensitivity brands
names(sort(el)[1:5])
names(sort(-el)[1:5])
