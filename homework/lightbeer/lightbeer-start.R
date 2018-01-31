### starter script
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


### 1.5
# bootstrapping
# this code takes a minute to run...
B <- 20
volbeta <- rep(NA,B)
set.seed(41201)
for(b in 1:B){
    print(b)
    # mleb <- ?
    # volbeta[b] <- ?
}


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

# 2.5
# now subset to a small group
sub = which(demog$market=="LOS ANGELES" & demog$income=="200k+")

########
## Q3 ##
########

## build a much bigger matrix of price interactions
xprice <- sparse.model.matrix( ~ logprice*(market*income+brand+promo)-market*income-brand-promo, 
    data=naref(cbind(demog,brand,promo)))[,-1]
xprice <- xprice[,colSums(xprice!=0)>0] # remove the never occurs
x <- cBind(xprice,xbeer,xdemog) # bind with out other matrices

# 3.4
# the median consumer
xbar_median <- apply(xprice/logprice, 2, median)

# 3.5
## bootstrap the lambdas using a for loop.
## note we don't need cv.gamlr here, just gamlr
set.seed(41201)
B <- 10 
DF <- rep(0,B)
for(b in 1:B){
    index <- sample.int(n,replace=TRUE)
    # fitb <- ?
    # DF[b] <- ? 
    print(b)
}

########
## Q4 ##
########

#### 4.5
library(parallel)
cl <- makeCluster(
    min(detectCores(),5),
    type=ifelse(.Platform$OS.type=="unix","FORK","PSOCK"))
multifit <- dmr(cl, xdemog, brand, verb=TRUE, lmr=1e-3)
