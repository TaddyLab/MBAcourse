### light beer solutions
library(distrom)
source("naref.R")
source("fdr.R")
source("roc.R")
source("deviance.R")

lb <- read.csv("LightBeer.csv")

# break out the variables of main interest
n <- nrow(lb)
logspend <- log(lb$beer_spend)
logprice <- log(lb$price_floz)
logvol <- log(lb$beer_floz)
brand <- lb$beer_brand
container <- lb$container_descr
promo <- lb$promo
demog <- lb[,-(1:9)]
purch <- lb[,1:3]

# relevel some things
demog$market <- relevel(demog$market, "CHICAGO")
demog$income <- factor(demog$income, levels=rev(levels(demog$income)))

########
## Q1 ##
########

### 1.1 

# MLE linear model fit
mle <- glm( logprice ~ logvol*promo*brand + container + ., data=demog )
# This is a model where
# E[log(price) | x] = b_0 + log(vol)*b_{brand, promo} + x*beta, 
# where x includes demographic information and containter factors.
# the model is then log(price) ~ N( E[log(price) | x], sigma^2 ),
# where  sigma^2 is 0.028
summary(mle)$dispersion # 0.02799033
# Note: this estimates the variance of the residuals
sum(mle$resid^2)/(n-length(coef(mle)))
## GRADING NOTES: Most people got these no problem.

### 1.2
## plot and color by brand
par(mfrow=c(1,2))
# we have a long [left] tail of residuals on log price, indicating purchases 
# where we have overestimated the price by a large margin.  
plot(mle$resid, logvol, col=as.numeric(brand)+1, bty="n",cex=.75, pch=20) 
legend("bottomright", fill=2:6,legend=levels(brand),bty="n")
abline(h=8) 
# There are only big left residuals above the point logvol=8; this could be
# fixed by adding the factor:
bulk <- logvol>8
mle_with_bulk <- glm( logprice ~ logvol*promo*brand + container + bulk + ., data=demog )
# Indeed, we see that this new model is able to fit a bunch of transactions with very low prices.
plot(logprice, mle_with_bulk$fitted, col=as.numeric(brand)+1, bty="n",cex=.75, pch=20) 
abline(a=0,b=1)
legend("bottomright", fill=2:6,legend=levels(brand),bty="n")
# Note that 1DF is one parameter!  

## GRADING NOTES: Lots of people plotted things besides the residuals vs. log
#  volume. Another common mistake was that people added UPC as their degree of
#  freedom, overlooking the fact that it adds 100+ variables to the
#  regression. Many people also took away a variable instead of adding one.

### 1.3
# Rural ND vs CHICAGO?  Notice that we made chicago the reference level...
exp(coef(mle)["marketRURAL NORTH DAKOTA"])
# marketRURAL NORTH DAKOTA 
#                 1.209057 
# Switch from Natural to Busch?
exp(coef(mle)["brandBUSCH LIGHT"] - coef(mle)["brandNATURAL LIGHT"])
# brandBUSCH LIGHT 
#         1.415033 
# price drop with promotion at average log volume for COORS
exp(coef(mle)["promoTRUE"] 
    + coef(mle)["promoTRUE:brandCOORS LIGHT"]
    + mean(logvol)*coef(mle)["logvol:promoTRUE"] 
    + mean(logvol)*coef(mle)["logvol:promoTRUE:brandCOORS LIGHT"])
# promoTRUE 
# 0.9323223 
# And on a 6 pack of 12oz cans
exp(coef(mle)["promoTRUE"] 
    + coef(mle)["promoTRUE:brandCOORS LIGHT"]
    + log(12*6)*coef(mle)["logvol:promoTRUE"] 
    + log(12*6)*coef(mle)["logvol:promoTRUE:brandCOORS LIGHT"])
# promoTRUE 
# 0.9093099 

# GRADING NOTES: this one was done rather poorly.  Many people did not
# exponentiate. We did not give credit if people interpreted their
# correct calculation incorrectly. A common mistake was to say beer was $1.23
# more expensive per ounce in North Dakota, which would make for a very
# expensive beer!

### 1.4
## FDR; plot the p-values
par(mfrow=c(1,2))
pv <- coef(summary(mle))[,4]
hist(pv,col=8)
# We see a big spike at zero, indicating more p-values near zero than you
#  would expect if they all came from the null.  i.e., there is a log of signal here.
#
# Getting the cutoff at 0.01 is easy:
alpha <- fdr_cut(pv,q=.01,plotit=TRUE)
alpha # 0.00777462, or about 0.008 if you wish
length(pv[pv<=alpha])
# The last question is a bit harder...
# The slope of the FDR cutoff line is k*q/N;
# if we cutoff at 0.05, that implies 
# cutting off below an FDR line where
# 0.05 = q*k/N => q = N*0.05/k
N <- length(pv)
k <- sum(pv<=0.05)
N*0.05/k # 0.05643939
# So this implies an FDR of around 5.6%.
# Alternatively, you could also have asked what it the biggest p-value 
# below 0.05 in our sample, then found the FDR cutoff implied at that point.
samplecut = max(pv[pv<=0.05]) # 0.039
N*samplecut/k # 0.04434687
# This implies an FDR of around 4.4%.  So either way, the answer is ~5%

# GRADING NOTES: Most people understood FDR, and most people even managed to
# figure out the last part of the question (FDR at alpha=0.05), so nice work!

### 1.5
# bootstrapping
# this code takes a minute to run...
B <- 20
volbeta <- rep(NA,B)
set.seed(41201)
for(b in 1:B){
    print(b)
    mleb <- glm( formula(mle), data=demog, subset=sample.int(n,replace=TRUE))
    volbeta[b] <- coef(mleb)["logvol"]
}

## Use the results of summary(mle) to get the confidence interval.
betastats <- coef(summary(mle))["logvol",]
## upper 90% and lower 10% bounds 
ub <- betastats["Estimate"] + qnorm(0.9)*betastats["Std. Error"]
lb <- betastats["Estimate"] + qnorm(0.1)*betastats["Std. Error"]
## recall that qnorm returns the normal quantile (z-value) corresponding to
#  specific probabilities P(Z<z); here we've grabbed the 90th and 10th quantiles.

# plot the comparison
hist(volbeta, col=8, border="grey80")
abline(v=betastats["Estimate"])
abline(v=ub, lty=2)
abline(v=lb, lty=2)

## 80% CI from bootstrap:
quantile(volbeta, c(.1,.9))

mean(volbeta < lb) # 0.1, or 2/20.  right on!
mean(volbeta > ub) # 0.2, or 4/20.  
# So the mle summary under-estimates the right tail of uncertainty.
#
# These differences are most likely due to the fact that volbeta (bootstrap) 
# accounts for uncertainty in x, while the theoretical mle standard errors assume a fixed x.
# This is also a small bootstrap sample, so that might explain  any discrepancy.
# 
# Since we have so much data here (n>70k), I DO NOT have any reason to doubt the 
# normality assumptions of the theoretical mle confidence interval. (i.e., such an answer is wrong)

## GRADING NOTES: All over the place. Lots of problems with getting the code
#  to run. A large number of students must be making the same mistake because
#  they have identical looking distributions that are far to the right of the
#  mle. Many people do not understand what CI is, and thus how to calculate it
#  for a bootstrap. Many people are also not showing their work, they just
#  tell me what their CI is. This was especially common for the weaker
#  midterms and it was pretty challenging to figure out whether they
#  calculated it correctly but their distribution was slightly different, or
#  if it was just wrong.


########
## Q2 ##
########

# build a bigger demographics design by interacting with market
xdemog <- sparse.model.matrix( 
    ~ market*(buyertype+income+childrenUnder6+children6to17+
        employment+degree+occupation+ethnic+microwave+
        dishwasher+tvcable+singlefamilyhome+npeople
        ), data=naref(demog))[,-1]
xdemog <- xdemog[,colSums(xdemog)>0] # drop n'er occurs

xbeer <- sparse.model.matrix( ~ ., data=data.frame(brand=naref(brand), promo) )[,-1]

### 2.1
## one-D regression
oned <- glm(logspend ~ logprice)
coef(summary(oned))["logprice",]
#       Estimate     Std. Error        t value       Pr(>|t|) 
#  -3.026858e-01   9.179719e-03  -3.297332e+01  1.080635e-236 
# so sales drop by .3% for every 1% price increase

## Naive lasso regression
naivereg <- gamlr(cBind(logprice,xbeer,xdemog), logspend, lmr=1e-4)
## you need to drop lmr from its default to avoid overly simple models
plot(naivereg)
coef(naivereg)["logprice",]
# -0.479777 
# so sales drop by .5% for every 1% price increase.
#
# The consumers look MORE price sensitive after we include controls
# (even if the lasso approach is naive here).  Thus there were controls that
# led to higher sales while being simultaneously associated with higher prices.

## GRADING NOTES: Most of the students found correct numbers. However, there
#  were few people who regressed price onto expenditure, which screws up not
#  only this part but also the subsequent questions. Aside from the number
#  that you get from the software, it is very important to have the right
#  interpretation. Some people made a mistake in terms of converting the
#  estimates to percentage points. Some people even didn't notice that because
#  of logs they should interpret the result in terms of relative changes
#  instead of absolute changes. Some people interpreted it as the percentage
#  change in log price, which is wrong. Unfortunately, one rather common
#  mistake was that some people exponentiate the estimates to interpret them.
#  Probably they mixed up Logit models with this linear regression model.

### 2.2 
#
#   In the first case, we have controlled for nothing.  Thus any outside
#   variables that are correlated with BOTH log-price and log-spend could
#   influence the estimated treatment effect.  For example, if the stores in
#   richer areas raise prices, and people still spend more, then it might seem
#   as though higher prices lead to more spending.
#
#   In the second case, we have included a bunch of controls (covariates).
#   However the lasso will choose the combination of covariates that is useful
#   for predicting future spending if demographics and prices follow the same
#   distribution as in our data.  That means that, for example, it might
#   choose to place a single coefficient on log-price that confounds our
#   treatment effect estimate with the effects of other variables (e.g,
#   market, income) that are correlated with price.  We have not done our
#   model selection (or building of candidate models) to ensure that we avoid
#   this scenario.
#
#   In a randomized controlled trial, a supermarket could have COMPLETELY AT
#   RANDOM moved prices up and down at their stores.  The treatment (price)
#   thus varies independently from the response of interest.  Thus nothing is
#   correlated with price (so you don't need to control for it at all) and, in
#   the lasso case, the model will be optimized for prediction in a future
#   where price remains independent from all other controls: i.e., it will be
#   optimized for estimation of the treatment effect.


## GRADING NOTES: In order to get the full mark in this question you should be
#  very clear. Very few people got this one fully right. Many students threw
#  some buzzwords such as confoundedness, multicollinearity, and so forth, but
#  they were using the words incorrectly or in the wrong context. You should
#  have had four clear components to get the full credit. 1. The uni-variate
#  regression is not valid because there might be some other missing variables
#  such as promotion, advertisement, and so forth that are correlated both
#  with price and expenditure. 2. Naive Lasso also is not competent for causal
#  inference because it is designed for prediction. In particular, when there
#  are many highly correlated variables, Lasso could randomly choose only one
#  of them. In fact, theoretically, it is possible that Lasso assigns zero to
#  the price in favor of another highly correlated covariate. 3. The gold
#  standard for causal inference is RTC. 4. Describe carefully your RTC
#  design.

### 2.3
#   Build your best possible price regression
#   My solution here is straightforward, but you could have also added additional interactions.
#   I use AICc to select the optimal model, but you could have used CV. If you do use CV, 
#   it is preferable to use CV.min here because our goal is prediction [of log price]
pricereg <- gamlr(cBind(xbeer,xdemog), logprice,  lmr=1e-4)  # again, dropping lmr is key
# predict
estlogprice <- drop( predict(pricereg,cBind(xbeer,xdemog)) )
# plot them
plot(pricereg)
plot(estlogprice,logprice)
# the in-sample deviances and R2:
(D <- deviance(estlogprice,logprice))
# 2333.724
(D0 <- deviance(mean(logprice), logprice))
# 4712.443
1-D/D0
# 0.5047741
# there is lots of left-over independent variation in log-price around its expectation given x.
# this bodes well for our treatment effect estimation

# GRADING NOTES:  This one was relatively easy. You should have had a graph
# and calculated R squared. In addition, you should explain in one line about
# the magnitude of R-squared and what does it mean for your double-Lasso
# procedure in the following question. In fact, since the R-squared is not
# that high, it leaves plenty of independent variation for the price.


### 2.4 this question is quite easy if you did the other bits correct. 
#   two key things we check for: you set lmr lower than default to get a model
#   selection that is not at the edge, and you let estlogprice enter without penalty. 
treatreg <- gamlr(cBind(estlogprice,logprice,xbeer,xdemog), 
                logspend, lmr=1e-4, free=1)
plot(treatreg)
coef(treatreg)["logprice",]
# -0.475 essentially the same as before.
# This means that there was 
# *enough independent variation in logprice relative to the controls*
# so that a naive lasso does a decent job of recovering the treatment effect.

# GRADING NOTES: This one was also rather easy. This is just the second stage
# of the double-Lasso procedure. The double-Lasso and naive lasso results
# should be very close regardless of using CV or IC criteria. Some people
# tried to justify the tiny difference with obviously wrong arguments. 

### 2.5
# now subset to a small group, repeating both your naive and treatment effects lasso
sub = which(demog$market=="LOS ANGELES" & demog$income=="200k+")
subnaive <- gamlr(cBind(logprice,xbeer,xdemog)[sub,], logspend[sub],  lmr=1e-4)

# I use the former estlogprice model here; this is probably better than
# re-running it, since there are only 39 purchases in sub and we are able to
# learn a better price predictor by using the full 70k transactions.  Also,
# the full model already allows for different price prediction by region and
# income (although it would have been better if I interacted these).

# and finally, but it all into a treatment regression with subestlogprice unpenalized.
subtreat <- gamlr(cBind(estlogprice,logprice,xbeer,xdemog)[sub,], 
                logspend[sub], lmr=1e-4, free=1)
par(mfrow=c(1,2))
plot(subnaive)
plot(subtreat)
# compare the results
coef(subnaive)["logprice",] # -0.54
coef(subtreat)["logprice",] # -0.19
#  In the naive lasso, it looked like the rich Angelenos were more price
#  sensitive than full sample average (0.54 against 0.47)!  But once we did the treatment
#  effect lasso properly, we see that their price sensitivity is about 60% lower
#  than in the full sample (.19/.47 = .4).
# 
#  Note that this also implies we should be allowing for heterogeneity 
#  in price sensitivity across markets, income, etc... see the bonus

## GRADING NOTES: This question turned out ex-post hard. Very few people got
#  the right number for this specific subsample of data (200k+ * LA). Most of
#  the people didn't apply subsetting properly. In fact, many students found
#  higher price sensitivity for this rich population, which should have been a
#  red flag. Many students didn't point out to the morale of the story that it
#  is important to consider the heterogeneity among different segments of the
#  population. In fact, you should have done two comparisons. First,
#  double-Lasso for the whole population didn't improve the estimate whereas
#  for this specific segment made a huge difference. Second, this segment of
#  the population is significantly less price sensitive.

########
## Q3 ##
########

## build a much bigger matrix of price interactions
xprice <- sparse.model.matrix( ~ logprice*(market*income+brand+promo)-market*income-brand-promo, 
    data=naref(cbind(demog,brand,promo)))[,-1]
xprice <- xprice[,colSums(xprice!=0)>0] # remove the never occurs
x <- cBind(xprice,xbeer,xdemog) # bind with out other matrices

## 3.1
# run the cv regression 
cvreg <- cv.gamlr(x, logspend, lmr=1e-4, verb=TRUE)
# for good measure, here are cv and path plots.
par(mfrow=c(1,2))
plot(cvreg$gamlr)
plot(cvreg)
# select=1se chooses the lambda whose mean OOS deviance is no more than 1
# standard error away from the lowest mean OOS deviance.
# select=min chooses lambda with mimimum mean OOS deviance
(1- cvreg$cvm[cvreg$seg.1se]/cvreg$cvm[1]) # 1se rule ~ 39%
(1- cvreg$cvm[cvreg$seg.min]/cvreg$cvm[1]) # min rule ~ 40% also
# You could have also just roughly read these values off of the cv plot
# note that there is some randomness here due to the CV experiment

## GRADING NOTES: This was straightforward and generally well done.  The
#  biggest issue was that some people didn't use a small enough lambda.min.ratio

## 3.2 plot your information criteria against each other and the cv results
#  Note that multiple plots would also have been fine.  You need to exponentiate 
#  and divide by n to get the IC results on the same page as the CV results
par(mfrow=c(1,1))
plot(cvreg, bty="n", ylab="deviance approximation")
ll <- log(cvreg$g$lambda)
points(ll, exp(AICc(cvreg$g)/n), pch=20, col="red", xlab="log lamba", ylab="IC approx deviance", bty="n")
points(ll, exp(AIC(cvreg$g)/n), pch=20, col="gold")
points(ll, exp(BIC(cvreg$g)/n), pch=20, col="green")
legend("top",fill=c("red","gold","green","blue"),legend=c("AICc","AIC","BIC","CV"),bty="n",horiz=TRUE)
# Both AIC and AICc are trying to approximate a re-scaling of the OOS deviance 
# Thus the lambdas at minimum AIC and AICc values are estimates of the 
# lambda which minimizes OOS error -- the same thing targeted with the cv.min rule.
# Also, in this case, the degrees of freedom are low enough relative to 'n' (i.e., n/df is big) 
# so that AIC works fine, and gives an answer close to AICc. 
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

## GRADING NOTES: As above, mostly well done.

### 3.3 bud light price sensitivity in LA for 200k+ earners
Bspend <- coef(cvreg)[,1]
Bi <- Bspend[c("logprice",
    "logprice:marketLOS ANGELES",
    "logprice:income200k+",
    "logprice:brandBUD LIGHT",
    "logprice:marketLOS ANGELES:income200k+")]
sum(Bi) # (note that there is some randomness due to CV selection)
#   Sensitivity for our wealthy Angeleno is higher than we'd estimated in
#   question 2.4. This could be due to extra flexibility (we're now allowing
#   the sensitivity to vary by brand and by promotion), or it could be that we
#   have other variables that are correlated with LA and price that are making
#   things complicated. Regardless, this should be a good model for prediction
#   of future price sensitivity so long as the covariate distribution doesn't
#   change too much (i.e., so long as demographics and pricing strategies
#   remain the same).

## GRADING NOTES: many people didn't add up all the correct coefficients,
#  while some ran another unnecessary lasso on a subset of data (probably
#  confused from Q2)

### 3.4 
# This question is actually pretty easy, 
# but you need to have a good understanding of what is going on.
# the average of X'Beta is the (average of X) times beta.  
# So you just use xbar%*%beta  [ or, same thing, sum(xbar * beta) ]
xpbar <- colMeans(xprice/logprice)
xpbar%*%Bspend[colnames(xprice)]
# -0.5223889 
# the result is very close to the average elasticity in question 2
# The result is less  sensitive than for the median consumer.
xbar_median <- apply(xprice/logprice, 2, median)
xbar_median%*%Bspend[colnames(xprice)]
# -0.6453533

## GRADING NOTES: A common wrong answer was to report the raw coefficient on
#  logprice for both the average and median question parts. Another common
#  mistake was misunderstanding what the median is in practice: many students
#  assumed that in a categorical 0-1 data the median is 0; but remember that in e.g. a
#  sample of three observations 0, 1, 1 the median will be 1, not 0.  
#  Note that the median is a somewhat strange thing to look at here; the
#  'median' customer is, as you mention, just mostly zeros (they don't belong
#  in any state).    In other contexts it is can be more meaningful; e.g., if
#  the covariates measure activity or some other ordinal variables.  But here,
#  my main goal was to hint to you how you could easily calculate average
#  sensitivity by calculating an average customer (i.e., finding fitted values at E[x]).
#  The average sensitivity is generally a useful concept.

### 3.5
## bootstrap using a for loop.
## note we don't need cv.gamlr here, just gamlr
## in any case this takes a bit of time.
set.seed(41201)
B <- 10 # more samples would have been better, but it takes a while
DF <- rep(0,B)
for(b in 1:B){
    index <- sample.int(n,replace=TRUE)
    # if you don't set lmr low enough the anser will change
    fitb <- gamlr(x[index,], logspend[index], lmr=1e-4) 
    DF[b] <- sum(coef(fitb)!=0)
    print(b)
}
hist(DF,col=8, freq=FALSE, main="", 
    xlab="AICc selected model degrees of freedom")
abline(v=sum(coef(cvreg$gamlr)!=0), col=2)
# compare full sample fit to bootstrap mean
mean(DF) # will be around 2570-2580
sum(coef(cvreg$gamlr)!=0) #2520
# The reason that the AICc tends to select slightly more coefficients in the bootstrap is that
# the bootstrap procedure makes the prediction problem seem easier than it actually is 
# (such that AICc decides to select a more complicated model).
# In particular, the same observations are repeated multiple times (with no added noise)
# in every bootstrap replication, and these are easier to predict than true new observations.
#
# to put it another way, since more or less the same signal is observed in
# each bootstrap iteration it means that the signal is not so noisy, which
# means more dfs can be picked. Imagine if you did CV on bootstrap data; the
# same info would often be in the leave-out and training samples, making you
# overly optimistic about your ability to predict the future.
#
# 
#
# This is why we use CV, and not the bootstrap, to estimate OOS performance.
#
# In week 8, when we discuss  trees and bagging, we'll see how to use
# bootstraps instead of CV to improve OOS prediction 
# (the trick is averaging, instead of choosing)

## GRADING NOTES: very few students were able to verbalize something close to
#  the intuition above about why the bootstrap AICc chooses a higher DF, but
#  this is fine: the bootstrap is brand new to you guys and takes a while to
#  figure out.  The rest of the question was well done.


########
## Q4 ##
########

### 4.1 
ybud <- brand=="BUD LIGHT"
budfit <- gamlr(xdemog, ybud, 
    family="binomial", lmr=1e-4, verb=TRUE)
phat <- predict(budfit, xdemog, type="response")
## I do both a lasso path and fit plot here, but fit is enough for full marks
par(mfrow=c(1,2))
plot(budfit)
plot(phat ~ brand,col=c(3,2,2,2,2))
# note that a perfect fit would have every brand other than bud down near zero.

## GRADING NOTES: generally well done, but many students chose to talk about
#  lambda selection rather than about the prediction fit for ybud.

#### 4.2
# AICc selection chooses the model that maximizes
#     deviance + 2*df * n/(n-df-1). 
# The model at the selected lambda is fit via lasso penalized deviance 
# minimization,  so we need to know the  selected lambda under our AICc rule.
(lam <- budfit$lambda[which.min(AICc(budfit))] )
#        seg69 
# 7.156001e-05 
#
# thus the function that we are minimizing is
#
# -(2/n)*Deviance + 7.156001e-05*sum{ |b_j| }
# Where the deviance is 
# sum{ log(1 + exp(x_i'b)) - y_i*x_i'b }
#
# The fitted and null deviances are then:
(D <- deviance(ybud, phat, "binomial"))
# 54145.29
(D0 <- deviance(ybud, mean(ybud), "binomial"))
# 88738.62
1 - D/D0
# 0.389834
# So we're explaining around 40% of the in-sample deviance with out fitted regression.

## GRADING NOTES: Almost nobody answered with the precise formula above, which
#  wrote out the full deviance. However, since the overwhelming majority of
#  the class figured that's what I was asking for we gave you the full marks.

#### 4.3
# these are pretty straight-forward applications of lecture 5
# ROC plot
roc(p=phat, y=ybud)
## how many true bud light buyers do we get?
(sens <- mean(phat[ybud]>.1)) # sensitivity of 95%
## and how many non-bud buyers are classified as potential?
(spec <- mean(phat[!ybud]<.1)) # specificity of 47%
# and add this point to the roc plot
points(1-spec,sens, col=2,lwd=2)

## GRADING NOTES: this was generally well done.

#### 4.4
# coefficients
Bbud <- coef(budfit)[,1]
# The main effect is when there is no interaction
# market main effects (there are 92 market levels)
mktmains <- Bbud[2:93]
mktmains[which(mktmains!=0)]
# Since every other market is subsumed into the intercept, these effects 
# are relative to the baseline Bud market share expectation in 85 markets.  
# Thus, for example, the odds of choosing Bud over any other brand drop by
exp(Bbud["marketRURAL CALIFORNIA"]) # 1.512625
# around 50% in rural california relative to this baseline

# the second part of the question is about interactions...
Bbud[grep("marketRURAL CALIFORNIA:income",names(Bbud))] 
# marketRURAL CALIFORNIA:incomeunder20k  marketRURAL CALIFORNIA:income60-100k 
#                             2.4780317                             0.0000000 
#   marketRURAL CALIFORNIA:income20-60k    marketRURAL CALIFORNIA:income200k+ 
#                             0.4172631                            -4.5909389 
# marketRURAL CALIFORNIA:income100-200k 
#                             1.3447338 
# all of the interactions are nonzero except for income60-100K, 
# so that the main effect above should actually be interpreted 
# as the odds change for those making between 60 and 100 thousand.
# 
# For those who make >200k, the odds of bud actually decrease by a big margin
# for rural california relative to the baseline:
 exp(Bbud["marketRURAL CALIFORNIA"] + Bbud["marketRURAL CALIFORNIA:income200k+"]) 
# 0.01534306, so the odds decrease by 98.5%
#
# Bonus mark if you mentioned that the baseline actually changes!  Since we're not talking
# about a specific income group, the intercept is any market who has a zero main effect
# AND a zero interaction between market and income200k+
mkt200k <- Bbud[grep(":income200k+",names(Bbud))]
mkt200k[mkt200k==0] 
#   marketPHILADELPHIA:income200k+ marketRURAL MICHIGAN:income200k+ 
#                                0                                0 
# marketRURAL MISSOURI:income200k+       marketURBAN NY:income200k+ 
# now the set of states IN the intercept is quite small! 
# before, the set of states NOT IN the intercept was small.

## GRADING NOTES: This question has many parts, so rarely somebody got a full
#  grade. 1. Rural Californian are 50% more likely to buy Bud Light. 2. The
#  reference group in the intercept are the average of markets with zero
#  coefficients, including 85 markets out of 92. Very few people got this part
#  right. Many just skipped this part. An overwhelming wrong answer was
#  Chicago! 3. You should have had all the five interaction terms of the
#  income groups with Rural California in a table and interpret them. I'd say
#  about a third of the class had a clean analysis for this part.

#### 4.5 
library(parallel)
cl <- makeCluster(
    min(detectCores(),5),
    type=ifelse(.Platform$OS.type=="unix","FORK","PSOCK"))
multifit <- dmr(cl, xdemog, brand, verb=TRUE, lmr=1e-3)

# the DMR command with cl is distributing computation across your machine's cores,
# and it does so in such a way that each cores sees a copy of all the data 
# (i.e., it is parallel computing).
# Here you're using this many cores:
min(detectCores(),5)

# plot it
par(mfrow=c(3,2))
for(i in 1:5) plot(multifit[[i]])

# grab the coefficients (AICc)
Bmulti <- coef(multifit)

# recall, we found that consumption of Bud drops in rural california if you make lots.
exp(Bbud["marketRURAL CALIFORNIA"] + Bbud["marketRURAL CALIFORNIA:income200k+"]) 
# 0.01534306
# we can look at the corresponding multinomial coefficients to see who gained
exp(Bmulti["marketRURAL CALIFORNIA",]) # nothing here
# BUD LIGHT   BUSCH LIGHT   COORS LIGHT   MILLER LITE NATURAL LIGHT 
#          1             1             1             1             1 
exp(Bmulti["marketRURAL CALIFORNIA:income200k+",]) # lots here
#  BUD LIGHT   BUSCH LIGHT   COORS LIGHT   MILLER LITE NATURAL LIGHT 
# 0.09966718    1.00000000    7.15059343   33.45975235  104.62414499 
# Natural light gets all the rich rural Californians!
# the odds of NL are 105x higher than they are of the intercept customer making > 200k
# so the bud selection drops and is replaced by the others (obviously), 
# with Miller and Natural getting the most new customers

## GRADING NOTES:  Complete answer to this question was not that common. The
#  point of this question was to study the interaction of income brackets and
#  different brands in Rural California. Many people apparently had run some
#  parallel regressions but didn't mention the extra insight that considering
#  other brands in addition to the Bud Light can bring to the story.

#############################################
### BONUS!  
## you could have done many things.  

# Here, I take a look at income-dependent elasticity.

# This gets to the root of what it means to control for stuff. Now
# that we're adding logprice*income to the model, we need to also control for
# anything that predicts that the levels of this interaction beyond the
# information in estlogprice.

# The answer that I give recognizes that the only thing you need beyond estlogprice
# to predict logprice*income is income. 

# Thus you just need to add estlogprice interacted with income into the model, UNPENALIZED.

# Following your notation in the class, nu is the independent variation part in price.
#     logprice = estlogprice + nu
# We need estlogprice to isolate nu. Now with interaction the problem is as follows:
#     (estlogprice + nu)*income + (estlogprice + nu)
# In order to identify the coefficient of nu*income we need to control for estlogprice*income
# 
# This allows there to be a completely different price intensity within each income level,
# as well as for other factors that influence price sensitivity to act freely within each 
# income level.
#
# Create the design matrix for an income dependent logprice effect
xinc <- sparse.model.matrix( ~ income - 1, data=demog)
lp_xinc <- xinc*logprice
colnames(lp_xinc) <- paste(colnames(xinc),"logprice",sep=":")
# note that xinc is already inside xdemog too
# 
# Do the same thing for estlogprice
estlp_xinc <- xinc*estlogprice
colnames(estlp_xinc) <- paste(colnames(xinc),"estlogprice",sep=":")
# and run it
inctreat <- gamlr( 
    cBind(estlp_xinc, xinc,  logprice, lp_xinc, xbeer,xdemog), 
    logspend, free=1:5, # 5 income levels; columns of estlp_xinc
    lmr=1e-4, verb=TRUE)
plot(inctreat)
B <- coef(inctreat)[,1]
B[grep("logprice",names(B))]
# we can plot price sensitivity as a function of the income level
par(mfrow=c(1,1))
plot(B["logprice"] + B[colnames(lp_xinc)], type="l", lwd=2, xaxt="n", ylab="price sensitivity", xlab="income")
axis(1, at=1:5, labels=levels(demog$income))
# It is strange that this is not strictly increasing
# I suspect that we really don't have good data on the under 20k people 
# (e.g., incl students with outside support)

