
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

##### Q3: logistic regression 
overload <- biketab$cnt > 500
set.seed(5807) 
test <- sample(1:nrow(mmbike), 3000)

##### Q4: treatment effects
coef(fitlin)["hum",]

# a new model matrix excluding humidity
x <- mmbike[, -grep("hum",colnames(mmbike))]
hum <- mmbike[, "hum"] # pull humidity out as a separate vector

##### BONUS
# Huge variety in what you could do here...













