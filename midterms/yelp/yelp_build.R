## forward regression
library(distrom)

## read and build source data
rev <- read.table("~/project/yelp/data/yelp_rev_attr.txt", sep="\t",
	colClasses=c("integer",rep("character",4),rep("integer",4)), row.names=2,
	col.names=c("rkey","review","user","business","date","stars","funny","useful","cool"))
usr <- read.table("~/project/yelp/data/yelp_usr.txt", sep="\t", row.names=1,
	colClasses=c("character","numeric",rep("integer",4)),
	col.names=c("user","usr.stars","usr.count","usr.funny","usr.useful","usr.cool"))
biz <- read.table("~/project/yelp/data/yelp_biz_attr.txt", sep="\t", row.names=2,
	colClasses=c("integer",rep("character",3),rep("numeric",2)),
	col.names=c("bkey","business","city","state","biz.stars","biz.count"))

txt <- read.table("~/project/yelp/data/yelp_rev_text.txt", comment.char="", quote="",
	colClasses=c("integer","factor","integer"), sep="\t",
	col.names=c("rkey","term","count"))
cat <- read.table("~/project/yelp/data/yelp_biz_cats.txt", comment.char="", quote="",
	colClasses=c("integer","factor"), sep="\t",
	col.names=c("bkey","category"))

library(Matrix)
X <- sparseMatrix(i=as.numeric(txt$rkey),j=as.numeric(txt$term),x=txt$count,
		dimnames=list(rownames(rev),levels(txt$term)))
B <- sparseMatrix(i=as.numeric(cat$bkey),j=as.numeric(cat$category),
		dimnames=list(rownames(biz),levels(cat$category)))

## grab only reviews with known users
ru <- rev$user%in%rownames(usr)
rev <- rev[ru,] 
X <- X[ru,]

X <- X[,!grepl('"',colnames(X))]
X <- X[,!grepl(',',colnames(X))]

rev$date <- as.Date(rev$date)
rev$daysup <- as.numeric(as.Date("2013-01-19")) - as.numeric(rev$date)
rev$votetotal <- rowSums(rev[,c("funny","useful","cool")])
rev$year <- format(rev$date, "%Y")

biz$city <- factor(biz$city,levels=c(NA,unique(biz$city)),exclude=NULL)
rev$bizcity <- biz[rev$business,"city"]

rev$nrev <- usr[rev$user,"usr.count"] 

##  match to biz
B <- B[rev$business,]
rownames(B) <- rownames(rev)

totals <- colSums(X)
rev$nwrd <- rowSums(X)
X <- X[,order(-totals)[1:10000]]
all(rownames(X)==rownames(B))

xtract <- rev$bizcity=="Phoenix" & rev$year=="2012" & rev$date > as.Date("2012-09-01")
sum(xtract)

B <- as(B[xtract,],"dgCMatrix")
B <- B[,colSums(B)>10]
B <- B[rowSums(B)>0,]

R <- rev[rownames(B),c("stars","nwrd","nrev")]
X <- X[rownames(B),]
## get rid of ne'r occurs
X <- X[,colSums(X)>0][,1:5000]

Bstm <- summary(B)
Xstm <- summary(X)

write.table(Bstm,file="BizType.csv",sep=",",row.names=FALSE)
write.table(Xstm,file="WordFreq.csv",sep=",",row.names=FALSE)
write.table(R,file="ReviewStats.csv",sep=",",row.names=FALSE)
write(colnames(B),file="categories.txt")
write(colnames(X),file="words.txt")
