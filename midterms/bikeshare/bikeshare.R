
### if you don't yet have data.table, run install.packages("data.table")
library(data.table)
biketab <- fread("bikeshare.csv")
# tell R which are factors
biketab[, c("dteday", "mnth","season","weekday","hr","weathersit") := list(
  factor(dteday), factor(mnth), factor(season), 
  factor(weekday), factor(hr), factor(weathersit))]

####### Q1: outliers and FDR
## the next command calculates total cnt by day, 
# also keeping track of the corresponding yr and mnth id.
daytots <- biketab[, list(total=sum(cnt)), by=c("dteday","yr","mnth")]
row.names(daytots) <- daytots$dteday
# simple regression
daylm <- glm(total ~ yr*mnth, data=daytots)

### 1.1
# SSE = 726316624
summary(daylm)$deviance
# R^2 = 0.73
1 - summary(daylm)$deviance/summary(daylm)$null.deviance

### 1.2 
# the model is cnt_i = alpha_{month_i} + error_i, or E[cnt] = alpha_month.
# It is important to note that we've used both \textt{mnth} and \textt{yr}
# in the fashion of interaction,
# so that the model has a different conditional expected count for each
# specific month (i.e. different for January 2011 and January 2012).
# Finally, since we've fit Gaussian linear regression here the implied
# probability model is that cnt_i ~ N(alpha_{month_i}, sigma^2).
# You could criticize the model by noting that cnt is not independent accross days,
# nor does it appear to be normally distributed around the monthly means
# nor does the variance look constant:
plot(as.Date(daytots$dteday), daytots$total)

### 1.3
## For the standardized residuals, I expected you to calculate
stdres <- daylm$residuals/sqrt(summary(daylm)$dispersion)
## alternatively, you could use stdres <- rstandard(daylm) or related rstudent.
## daylm$residuals/sd(daylm$residuals) is OK too (because n >> df here).

## then:
pvals <- 2*pnorm(-abs(stdres))
# these are p-values under the null hypothesis that the residuals are N(0, sigma^2),
# or that the standardized residuals are N(0,1).  Thus small values indicate an
# observation that is improbably far from zero -- a potential outlier.

### 1.4
source("fdr.R")
print(alpha <- fdr_cut(pvals,0.05, plotit=TRUE)) # [1] 1.609804e-06
signif <- names(stdres)[pvals<=alpha]
stdres[signif] ## all big negatives
signif ## 2012-10-29 and 2012-10-30 are hurricane sandy.  
## There was a big nor'easter on 2012-04-22

### 1.5
hist(pvals, col="grey")
## The peak towards one indicates the opposite of our spike at zero 
## 	(recall, the latter indicates p-values smaller than at the null).  In particular,
## this shows p-values that are _larger_ than you'd expect from the null.
## if you said this then you are good for part marks!

## why is harder:
## The reason this happens here is that the residuals are not normally distributed.
## for example,
par(mfrow=c(1,2))
plot(as.Date(biketab$dteday),biketab$cnt)
hist(stdres,breaks=100,col="grey")
abline(v=0, col=2)
## you'll notice that the peak of the distribution (it's mode) is right of zero
## and that it has a thicker tail out to the left than to the right.
## P-values area only uniform if data come from the null distribution (here N(0,1)) 
## so that this non-normality for residuals leads to non-uniformity in p-values.
##
## In addition, the data are likely not independent: there is correlation accross days.
## This could also be causing some residuals to be packed more
## tightly together than you'd see if they came from an independent sample.
## For example,
acf(stdres) ## e.g., the autocorrelation function (see timespace lecture)
## check out bonus solution at end of script for a model of the time dynamics

#### Q2: lasso regression
library(gamlr)
source("naref.R")
mmbike <- sparse.model.matrix(
	cnt ~ . + yr*mnth + hr*notbizday, 
	data=naref(biketab))[,-1]
y <- log(biketab$cnt)
## note, I need lambda.min.ratio=1e-4 because otherwise we don't get a path
## out to complex enough models (i.e. cv err is still decreasing at termination)
fitlin <- cv.gamlr( mmbike, y, lmr=1e-4, verb=TRUE )

## 2.1
# The response here is log(ride count), and the design (model) matrix includes
# the three continuous weather variables (temp, humidity, wind) along with 
# dummy variables for each of our date factors, for the weathersit factor, and  
# for every specific day.  The interaction terms create separate month effects by year
# (so that Jan 2011 is different from Jan 2012) and separate hour-of-day effects
# for business and nonbusiness days (the main hrX effect is thus on a business day,
# and the hrX:notbizday terms measure how things differ on a weekend or holliday).
#
# The issue of 'outlier detection' -- which was addressed on a daily scale in Q1 --
# is incorporated into the model here by adding possible effects for every day.
# The lasso path (and selection along this path) will determine which days are usefully
# modelled as outliers: different from other days in a manner not explainable by the
# other available covariates.
#
# Also, you could have connected the outliers to weather and said that now we are accounting for weather in the model.
 
## 2.2
# first, for good measure, here are cv and path plots.
par(mfrow=c(1,2))
plot(fitlin$gamlr)
plot(fitlin)
# select=1se chooses the lambda whose mean OOS deviance is no more than 1
# standard error away from the lowest mean OOS deviance.
# select=min chooses lambda with mimimum mean OOS deviance
(1- fitlin$cvm[fitlin$seg.1se]/fitlin$cvm[1]) # 1se rule ~ 94%
(1- fitlin$cvm[fitlin$seg.min]/fitlin$cvm[1]) # min rule ~ 94% also
# You could have also just roughly read these values off of the cv plot

### 2.3
## plot your AICc and BIC along the lasso path
par(mfrow=c(1,2))
plot(log(fitlin$gamlr$lambda),AIC(fitlin$gamlr), 
	pch=20, col="gold", xlab="log(lambda)", ylab="IC")
points(log(fitlin$gamlr$lambda),AICc(fitlin$gamlr), pch=20, col=2)
points(log(fitlin$gamlr$lambda),BIC(fitlin$gamlr), pch=20, col=4)
legend("top",fill=c(2,"gold",4),legend=c("AICc","AIC","BIC"),bty="n",horiz=TRUE)
## you can barely see AIC because AICc and it overlap.  
plot(fitlin$gamlr, col=8)
abline(v=log(fitlin$gamlr$lambda[which.min(AIC(fitlin$gamlr))]),col="gold")
abline(v=log(fitlin$gamlr$lambda[which.min(AICc(fitlin$gamlr))]),col="red")
abline(v=log(fitlin$gamlr$lambda[which.min(BIC(fitlin$gamlr))]),col="blue")
abline(v=log(fitlin$lambda.min),lty=2)
abline(v=log(fitlin$lambda.1se),lty=2)
# Both AIC and AICc are trying to approximate the OOS deviance (MSE here).
# Thus the lambdas at minimum AIC and AICc values are estimates of the 
# lambda which minimizes OOS error -- the same thing targeted with the cv.min rule.
# Also, in this case, the degrees of freedom are low enough relative to 'n' 
# that AIC works fine, and gives an answer close to AICc. 
#
# For these reasons, AIC, AICc, and cv.min all line up close to each other.
# 
# The BIC is trying to find lambda with highest probability of having the minimum OOS error,
# which is different than finding the lambda corresponding to lowest expected OOS error.
# For example, if there is more uncertainty about OOS error at the lambda with min expectation, 
# then it could be that another value with higher expected error but lower uncertainty around this
# value will have a higher probability of being best. 
# 
# To put it simply, the BIC incorporates uncertainty about OOS error that the AICc does not.
# In this way, it is similar to the 1SE rule, which also accounts for uncertainty about OOS error.
# But BIC is more conservative and tends to choose simpler models than the 1se rule (it does here).

### 2.4
## there are a few way ways to pull out coefficients by name efficiently.
## here's my approach
B1se <- coef(fitlin) # grab one set of coef
dayvar <- grep("dteday",rownames(B1se)) # 'grep' which contain dteday in name
B1se <- B1se[dayvar,] # use these to subset here and for other slices below
Bmin <- coef(fitlin, select="min")[dayvar,]
Baic <- coef(fitlin$gamlr, select=which.min(AIC(fitlin$gamlr)))[dayvar,]
Bbic <- coef(fitlin$gamlr, select=which.min(BIC(fitlin$gamlr)))[dayvar,]
Baicc <- coef(fitlin$gamlr)[dayvar,]
## they all find hurricane sandy and christmas each year (boxing day in 2012)
exp(B1se[order(-abs(B1se))[1:3]])
exp(Bmin[order(-abs(Bmin))[1:3]])
exp(Bbic[order(-abs(Bbic))[1:3]])
exp(Baic[order(-abs(Baic))[1:3]])
exp(Baicc[order(-abs(Baicc))[1:3]])
## and each implies a 60-70% drop in cnt.

### 2.5
## bootstrap the lambdas using a for loop.
## note we don't need cv.gamlr here, just gamlr
B <- 50 # more samples is better, but we're not picky
n <- nrow(mmbike)
aiclams <- biclams <- rep(0,B)
for(b in 1:B){
	index <- sample(1:n,n,replace=TRUE)
	fitb <- gamlr(mmbike[index,], y[index], lmr=1e-4)
	aiclams[b] <- fitb$lambda[which.min(AICc(fitb))]
	biclams[b] <- fitb$lambda[which.min(BIC(fitb))]
	print(b)
}
hist(log(aiclams),col=rgb(1,0,0,.5), freq=FALSE, xlab="log lambda",
	xlim=range(log(fitlin$gamlr$lambda)),main="IC selected lambdas", ylim=c(0,6))
hist(log(biclams),col=rgb(0,0,1,.5),add=TRUE, freq=FALSE)
legend("topleft",bty="n",fill=c("red","blue"),legend=c("AIC","BIC"))
# two things to notice.  First is the familiar 'AIC chooses smaller lambda than BIC'.
# but more interestingly, we also see that the sampling distribution for BIC lambda
# has lower variance (is less uncertain) than for AIC lambda.
# This is what we mean when we say that BIC is more stable than the AIC.

# NOTE: If you bootstrapped lambda rather than log(lambda), 
# you would have seen higher variance for BIC.  This happens 
# because the lambdas are bigger for BIC, 
# and variance is correlated with the size of lambda.
# This answer is OK and worth full marks, because I didn't specify.  
# However, the scale upon which variablity in lambda affects 
# model choice is on log scale: the models are indexed 
# evenly spaced on log(lambda).  

##### Q3: logistic regression 
overload <- biketab$cnt > 500

### 3.1
fitlogit <- gamlr(mmbike, overload, family="binomial", lmr=1e-4)
plot(fitlogit)

### 3.2
# since the main effect of 'hr' is for business days (interaction term is with notbizday)
# we can just pull out those coefficients with label 'hrX' and interpret them.
Bhr <- coef(fitlogit)[paste("hr",0:23,sep=""),] # you only need one selection rule (I use AICc)
plot(1:24, Bhr,type="h")
## looks like rush hour! we see high overload probabilities in morning and evening rush hour
exp(Bhr["hr17"]) # odds increases x10million!  
# from looking at the other hour effects, this is relative to a baseline of the odds of overload
#  between 9pm and 8 am and between 10am and 3pm (these are when the effects are zero)
exp(coef(fitlin)["hr17",]) # count increases x2.7 at 5pm

# NOTE: the question actually asked for change in probability 
# rather than odds.  Bonus marks if you saw this and answered that way.  
# It makes it a much tougher question, because the changes in 
# probablities are a function of the baseline x.  
#
# Also, answering in % change, etc, is fine.

### 3.3
## costs you $100/hr to add an extra driver, 
## but $200/hr are the estimated damages when 
## the system is overloaded and your other drivers work overtime.
## thus costs are
## 0 if < 500 with no extra driver
## 100 if < 500 with extra driver
## 200 if > 500 with no extra driver
## 100 if > 500 with extra driver
##
## thus you'll add a driver if 
## 200*p > 100*p + 100*(1-p)
## => 200p > 100
## => p > 1/2 ... so it's just max.prob rule! 

### 3.4
source("roc.R")
p <- predict(fitlogit, mmbike, type="response")
roc(p,overload)
# this picture tells me that, in-sample, I can have a very low 
# number of false positives while still having a low false negative rate.
#
# what proportion of true overloads do I classify as such?
mean(p[overload==1]>=.5) # sensitivity of 90-91%
## what proportion of non-overloads are correctly labelled?
mean(p[overload==0]<.5) # specificity of > 99%

### 3.5
# if you run set.seed before sampling, 
# you'll always get the same `random' sample
set.seed(5807) 
test <- sample(1:n, 3000)
#
# just fit the same model as above, but using -test
fittrain <- gamlr(mmbike[-test,], overload[-test], family="binomial", lmr=1e-4)
ptest <- predict(fittrain, mmbike[test,], type="response")
roc(ptest,overload[test])
# It appears that our AICc selected predictions (p) are good, 
# i.e. not overfit,
# since the ROC curve drops very little when moving from in to out-of sample.

##### Q4: treatment effects

### 4.1
# the naive value of beta on humidity
coef(fitlin)["hum",]
exp(coef(fitlin)["hum",]) 
# implies around a 5% drop in rides.
# BIC agrees
exp(coef(fitlin, k=log(n))["hum",]) 

### 4.2
#
# a new model matrix excluding humidity
x <- mmbike[, -grep("hum",colnames(mmbike))]
hum <- mmbike[, "hum"] # pull humidity out as a separate vector
#
# fit the treatment regression for humidity on x
treat <- gamlr(x,hum, lmr=1e-4)
# get predicted dhat values (humhat here)
humhat <- predict(treat,x)
# compare them
plot(hum,humhat)
# there does appear to be a lot of variation around the
# pattern of humidity that is predictable from x.
# this means that (so long as the relevant controls are in x)
# we have seen some independent variation in humidity and can 
# hope to measure a treatment effect 
# (i.e., it looks somewhat like an experiment).

### 4.3
#
# Using the double-lasso algorithm from class, we just put humhat into
# the model without any penalty (using the free argument)
fitte <- gamlr(cBind(humhat,hum,x), y, free=1, lmr=1e-4)
## AICc gives roughly same answer as 4.1: 5% drop
exp(coef(fitte)["hum",]) 
## But BIC now disagrees, and roughly halves this value.
exp(coef(fitte,k=log(nrow(x)))["hum",])

### 4.4
humtemp <- hum*x[,"temp"]
fitinteract <- gamlr(cBind(hum,humtemp,x), y, free=1, lmr=1e-4)
coef(fitinteract)[c("hum","humtemp"),] 
# the main effect of humidity on log count is -0.05 per 1SD increase,
# but this increases by 0.05 with every 1SD increase in temperature.
# Seems counterintuitive ... who wants hot sticky bike ride?
# alternatively, you could have exponentiated
exp(coef(fitinteract)[c("hum","humtemp"),] )
# In which case we get a 5% drop per 1SD increase in hum, but every 
# 1SD increase in temp leads to an extra 5% increase in rides 
# per 1SD increase in humidity.
# Thus if the  temp is +1SD, then humidity doesn't matter.  
# If temp is +2SD, a 1SD increase in hum actually increases rides by 5%.

### 4.5
#
# This question gets to the root of what it means to control for stuff.
# Now that we're adding hum:temp to the model, we need to also control for
# anything that predicts that beyond the information in humhat.
#
# NOTE: points for this if you attempted at all to control for humidity and temp.
# For example, partial marks if you controlled for temp*humhat but not humhat.
#
# you could have repeated the double-lasso algo for humhat,
# but even better would be to just realize that the only thing
# you need beyond humhat to predict hum:temp is temp!
# Thus we just need to add humhat interacted with temp into 
# the model to control for the predictable bit of humtemp.
humtemphat <- humhat*x[,"temp"]
mmtei <- cBind(humhat,humtemphat,hum,humtemp,x)
fittei <- gamlr(mmtei, y, free=1:2, lmr=1e-4) # both humhat and humtemphat need to be free
exp(coef(fittei)[c("hum","humtemp"),])
# now the interaction term no longer matters, and we see the same rough drop in rides per hum +1SD
# indeed, the effect size actually increase in magnitude to -6%.  
# this happens because we've now controlled for interaction between temp and the
#  bit of humidity that is predictable... it's like we've added a nonlinear temperature term.
#
# if you predicted hum*tem from controls and added that as humtemphat, that is fine too


##### BONUS
# Huge variety in what you could have done here, as there are many weaknesses in above analysis.
# The question I asked ealier: what about lags?  This accounts for dependence between temporal observations.
# To dwell deeper into it, I look back 8 hours for dependence.  See the timespace lecture for some context.
resid <- y-predict(fitlin, mmbike, select="min")
acf(drop(resid))
ylags <- cbind( lag1=head(y[-(1:7)],-1), 
				lag2=head(y[-(1:6)],-2),
				lag3=head(y[-(1:5)],-3),
				lag4=head(y[-(1:4)],-4),
				lag5=head(y[-(1:3)],-5),
				lag6=head(y[-(1:2)],-6),
				lag7=head(y[-1],-7),
				lag8=head(y,-8) )
mmbikear <- cBind(mmbike[-(1:8),], ylags) # drop first row since it has no lag available
fitar <- cv.gamlr(mmbikear, y[-(1:8)], lmr=1e-4)
residar <- y[-(1:8)]-predict(fitar, mmbikear, select="min")
par(mfrow=c(1,3))
plot(fitar)
plot(fitar$g)
acf(drop(residar))
## we are able to drop the deviance by around 1/4.
fitar$cvm[fitar$seg.min]
fitlin$cvm[fitlin$seg.min]
## it gives a much weaker effect of humidity.
coef(fitar)["hum",]














