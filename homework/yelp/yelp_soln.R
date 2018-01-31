### MIDTERM Solutions: yelp business reviews

source("yelp_start.R")

## [1.1] look at a histogram of pvalues
hist(mrgpvals)
# there is a clear spike down near zero, which indicates more very small
# p-values than you would get if the p-values were all from the null (since
# the null implies a uniform distribution)

## [1.2] 
source("fdr.R")
print(cutoff <- fdr_cut(mrgpvals,0.01))
# cutoff is 0.00161, or 0.161%
sum(mrgpvals<=cutoff)
# 813

## [1.3]
# the 250th smallest pvalue is 1.790372e-08 
mrgpvals[order(mrgpvals)[250]] 
# We can reverse the FDR equation to figure out `q':
# pval = q*250/5000 => q = pval*20
## around 3.6 (dep on machine precision) per 10million FDR
mrgpvals[order(mrgpvals)[250]]*20 
# A good answer is `basically zero'.
# If you said "2 or 3", as many students did, please review your notes.

## [2] least squares and bootstrapping

## Some illustration (not part of question, but always nice)
par(mfrow=c(1,2))
plot(linfit)
abline(v=log(linfit$lambda)[which.min(BIC(linfit))],
	col="green",lty=2) # BIC selection    
abline(v=log(linfit$lambda)[which.min(AIC(linfit))],
	col="red",lty=2) # AIC selection (same as AICc here, since n is big)
plot(yhat~factor(rev$stars),xlab="y", col=8)
#  Residual error tails are skewed low for 1-2 stars.
#  More troublesome, we're predicting stars <1 and >5 in many cases
#  but it does look like a nearly 
#  linear-gaussian relationship between fitted and true.

## [2.1]  
# in sample SSE is just deviance
(SSE <- linfit$deviance[which.min(AICc(linfit))]) # 6632
# Then in sample R^2:
cor(yhat,rev$stars)^2 # right around 1/2
#  or, based on deviance
1-SSE/linfit$deviance[1]

## [2.2] coefficients (don't forget interactions!)
print(Blin[c("Bowling","Airports")])
print(Blin[c("NWRD:Bowling","NWRD:Airports")])
Blin[c("Bowling")] + 100*Blin["NWRD:Bowling"]
Blin[c("Airports")] + 100*Blin["NWRD:Airports"]
# The intercept for an empty review (no words) for Bowling is zero, for
# Airports is negative (surprise!).  Bowling has a positive interaction with
# review length (the more you say, the better it is on average) while Airports
# have a negative interaction with length (again, surprise!).  For a review
# with 100 words, Airports and Bowling have intercepts (before looking at
# individual word counts) of roughly + and - a quarter (.27-.28) star (respectively).

## [2.3] bootstrapping
# dsamp is the bootstrap estimate of sampling distribution for degrees of freedom
par(mfrow=c(1,1))
hist(dfsamp) # nice histogram
mean(dfsamp) # around 400
(dfse <- sd(dfsamp)) # 30ish (variable with different runs)
(mean(dfsamp) + c(-1,1)*2*dfse) # 95% CI

## [3] logistic regression and model selection

# [3.1]
# fit the cross-validated regression model
binfit <- cv.gamlr(x, y=y, family="binomial")
# plot in sample fit pbad ~ y
pbad <- predict(binfit,x,type="response")
plot(as.vector(pbad)~factor(y))
# We gave partial credit to students who discussed OOS deviance.
# Another solid answer would be to plot IS deviance.

## [3.2]
# select=1se chooses the lambda whose mean OOS deviance is no more than 1
# standard error away from the lowest mean OOS deviance.
# select=min chooses lambda with mimimum mean OOS deviance
(1- binfit$cvm[binfit$seg.1se]/binfit$cvm[1]) # 1se rule ~ 35%
(1- binfit$cvm[binfit$seg.min]/binfit$cvm[1]) # min rule ~ 35% also
# We did not require you to show us the exact values of lambda.

## [3.3]
# easiest to use plots for comparison
par(mfrow=c(1,2))
plot(binfit)
plot(binfit$gamlr)
abline(v=log(binfit$gamlr$lambda)[which.min(BIC(binfit$gamlr))],
	col="green",lty=2) # BIC selection    
abline(v=log(binfit$gamlr$lambda)[which.min(AIC(binfit$gamlr))],
	col="red",lty=2) # AIC selection: close to AICc, since n>>df   
# Relative to both CV rules, 
# AICc and AIC are overfit (AIC moreso) while BIC is underfit.
# I'll use AICc here, but you could have chosen another.
# select words with AICc
Bwrd <- coef(binfit$gamlr)[colnames(Ccut),]
print(Bwrd[order(-Bwrd)[1:10]]) ## ten worst words
print(Bwrd[order(Bwrd)[1:10]]) ## ten best words
# they make sense to me!
# Notice that we are looking for REVIEW words, so we have excluded
# businesses from the print-out

## [3.4] classification of a good vs bad review
## ROC plot is nice; but it was not required
source("roc.R")
roc(p=pbad,y=y)
## what proportion of true bad reviews do we catch at p=1/20 cutoff?
mean(pbad[y==1]>=.05) # sensitivity
## what proportion of our alerts will be false alarms?
mean((y==0)[pbad>.05]) # false positive rate

# Suppose you had used AICc: you would still get full creidt
binfitaicc <- gamlr(x, y=y, family="binomial",lambda.min.ratio=1e-3)
pbadaicc <- predict(binfitaicc,x,type="response")
mean(pbadaicc[y==1]>=.05) # sensitivity
mean((y==0)[pbadaicc>.05]) # false positive rate

## [4] Treatment effects estimation for `reviewer experience'
#
## [4.1] 
# First look at distribution of nrev
par(mfrow=c(1,2))
hist(rev$nrev)
hist(log(rev$nrev))
# Why: nrev is all positive, and sprawled out to the right.
# Those far right values will have very high leverage on our estimates.
# Moreover, intuitively there is a bigger difference in experience 
# between 1-2 reviews than there is between 800 and 801 reviews.
# Also, `d' is our response for the treatment regression...  
# if you fit nrev ~ x without transformation you will get nonconstant
# variance in your residuals.
# All of this indicates we should be working on log scale
# since we will be regressing log(odds bad) on to log(nrev),
# this is a log-log regression and the coefficient on d can be interpreted
# as % change in odds of a bad review per 1% increase in user review count.

## [4.2] treatment propensity
# the fit is not very tight (R2<.2), which means we have a good amount of
# variation in d _independent_ of x.  So if x contains all the confounders we'd need 
# (admittedly, probably not true here) this would leave us a lot of independent
# treatment variation and mean that we're mimicking an actual experiment.
dhat <- drop(predict(treatfit,x))
plot(d,dhat,col="grey40")
cor(d,dhat)^2 #R2 is around 0.2
# What really matters here is our estiamted In Sample fit, (why?)
# but if you discussed out-of-sample fit here that is fine too.

## [4.3] Causal estimation
# just add dhat to the naive lasso, and make sure it is unpenalized.
# you could also have done BCH here.
causal <- gamlr(cBind(d,dhat,x), y, free=2, 
	family="binomial", lambda.min.ratio=1e-3)
coef(causal)["d",] # so we get a significant nonzero causal effect
# in words: odds of a bad review increase by 0.035% for every 1%
# increase in existing total reviews by that author
# independent from any correlated change in businesses visited
# or content of the written review.

# [4.4] Compare to marginal regression, 
# which gives a tiny and insignificant effect
summary(marg <- glm(y~d, family="binomial"))$coef["d",]
# So here, controlling for confounders allowed us to measure
# an effect where we could not see anything marginally.
# If you did a naive lasso instead of the marginal regression I asked for,
# that's good enough for partial marks.

## [5] multinomial regression
# very straight forward, you just need to read the coefficients
exp(Bmn["crap",1]) # Bmn["crap",2:3] are zero, so odds over both increase by ~ 11%
# We see some non-linearity in the relationship for fantastic.
# An extra count decreases odds of 2 over 1 stars by ~ 20%
exp(Bmn["fantastic",2]-Bmn["fantastic",1])
# but increases odds of 5 over 1 stars by ~ 50%
exp(Bmn["fantastic",5]-Bmn["fantastic",1])
# You were able to get full credit if you showed us
# exp(Bmn["fantastic",1]-Bmn["fantastic",2])
# exp(Bmn["fantastic",1]-Bmn["fantastic",5])
# but your verbal description needed to be correct!
