byhour <- read.csv("hour.csv")
bikeshare <- byhour[,c(3:11,13:14,2,17)]
w <- c("hum","temp","windspeed")
bikeshare[,w] <- scale(bikeshare[,w])
bikeshare[,"workingday"] <- 1-bikeshare[,"workingday"]
names(bikeshare)[7] <- "notbizday"
write.csv(bikeshare, "bikeshare.csv",quote=FALSE, row.names=FALSE)
