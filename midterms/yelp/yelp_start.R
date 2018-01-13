### MIDTERM: yelp business reviews

## read the data
biz <- read.csv("BizType.csv")  
txt <- read.csv("WordFreq.csv")
rev <- read.csv("ReviewStats.csv") # nwrd=review length, nrev= # reviews by this user
words <- scan("words.txt", what="character", sep="\n") #\n is `end of line'
categories <- scan("categories.txt", what="character", sep="\n") # these are the yelp categories

## create some sparse matrices
library(gamlr)
Biz <- sparseMatrix(i=biz$i,j=biz$j,x=biz$x,
	dimnames=list(rev=1:nrow(rev),cat=categories))

C <- sparseMatrix(i=txt$i,j=txt$j,
		x=txt$x,
		dimnames=list(rev=1:nrow(rev),cat=words))

## what do these look like?
Biz[1:2,c(12,103,96,124,129,130)] # reviews in rows, business types concerned in columns
C[1:5,1:5] # reviews in rows, word indexes in columns, values are word counts
# just matrices, but we haven't stored the zeros

## [1] Marginal Regression Screening 
##
## Another example of an algorithm that can be Distributed! 
## We'll do 5000 univariate regressions of 
## star rating on word presence, one for each word.
## Each regression will return a p-value, and we can
## use this as an initial screen for useful words.

# create a dense matrix of word presence
P <- as.data.frame(as.matrix(C>0))
library(parallel)
margreg <- function(p){
	fit <- lm(stars~p)
	sf <- summary(fit)
	return(sf$coef[2,4]) 
}

cl <- makeCluster(detectCores())
# pull out stars and export to cores
stars <- rev$stars
clusterExport(cl,"stars") 
# run the regressions in parallel
mrgpvals <- unlist(parLapply(cl,P,margreg))
## If parallel stuff is not working, 
## you can also just do (in serial):
# mrgpvals <- c()
# for(j in 1:5000){
# 	print(j)
# 	mrgpvals <- c(mrgpvals,margreg(P[,j]))
# }
## make sure we have names
names(mrgpvals) <- colnames(P)

#####  Set data for rest of exam #####

# Restrict text data to top 250 by above pvalues
Ccut <- C[,names(sort(mrgpvals)[1:250])]
# create matrix of interactions between length and category
BizNWRD <- rev$nwrd*Biz
colnames(BizNWRD) <- paste("NWRD:",colnames(Biz),sep="") 
# create our x matrix, used throughout.
x <- cBind(Biz,BizNWRD,Ccut) # make sure you understand what's in rows, what's in columns,
                             # and what do values mean

## [2] linear regression
# note: you'll want to use lambda.min.ratio=1e-3 in your lassos.
# linear regression for 1-5 stars on x. 
linfit <- gamlr(x, y=stars, lambda.min.ratio=1e-3)
# the coefficients (drop turns it into simple vector)
Blin <- drop(coef(linfit)) # AICc default selection
# predicted values
yhat <- as.vector(predict(linfit,x))
# note linfit$deviance contains the in sample deviance for each lambda

## bootstrap code for 2.3
# the sampling distribution of AICc optimal df.
# function takes an index of data and will return fitted AICc optimal df
bootdf <- function(ib){
	require(gamlr)
	fit <- gamlr(x[ib,],y=stars[ib], lambda.min.ratio=1e-3)
	sum(coef(fit)!=0)-1
}
# export the data to the clusters 
clusterExport(cl,"x")
# run 100 bootstrap resample fits
boots <- 100
n <- nrow(x)
resamp <- as.data.frame(matrix(sample(1:n,boots*n,replace=TRUE),ncol=boots))
dfsamp <- unlist(parLapply(cl,resamp,bootdf))

## again, this is all pretty fast so it could have happened in serial:
# dfsamp <- c()
# for(b in 1:100){
# 	dfsamp <- c(dfsamp, bootdf(resamp[,b]))
# 	print(b)
# }

## [3] logistic regression and model selection
y <- rev$stars<4 # our `bad review' binary response
# Say binfit is the estimated cv.gamlr object.
# Then note binfit$cvm contains  average OOS deviance for each lambda, and
# binfit$seg.min and binfit$seg.1se identify selected segments under each rule

## [4] Treatment effects estimation for `reviewer experience'

# consider treatment acting on log scale
d <- log(rev$nrev)
# fit a model for d on x
treatfit <- gamlr(x,d)

## [5] multinomial regression
# very straight forward, you just need to read the coefficients
library(distrom)
ymn <- factor(stars)
mnfit <- dmr(cl,x,ymn)
Bmn <- coef(mnfit)
Bmn[c("crap","fantastic"),]
