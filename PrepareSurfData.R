library(tree);library(lattice);library(boot);library(MASS);library(fmsb);library(e1071);library(ipred);library(caret)
library('rpart');library(randomForest);library(mda)
Surfboards <- read.csv("~/Data_mining/surf/.Rproj.user/SurfboardSoldAndOnHandAll.txt", header=F)
View(Surfboards)
Surfboards <- Surfboards[,-3]
Surfboards$V8 <- substring(Surfboards$V2,1,regexpr(" ",as.character(Surfboards$V2))-1)
View(Surfboards)
Surfboards <- Surfboards[which(Surfboards$V8 != ""),]
#Next time remove Catch Surf, McKevlins, and Hydroflex
#Next time convert to inches and remove all greater than 79 inches
Surfboards <- Surfboards[grep("10-",Surfboards$V8,invert=TRUE),]
Surfboards <- Surfboards[grep("9-",Surfboards$V8,invert=TRUE),]
Surfboards <- Surfboards[grep("8-",Surfboards$V8,invert=TRUE),]
Surfboards <- Surfboards[grep("7-",Surfboards$V8,invert=TRUE),]
Surfboards <- Surfboards[grep("6-10",Surfboards$V8,invert=TRUE),]
Surfboards <- Surfboards[grep("6-9",Surfboards$V8,invert=TRUE),]
Surfboards <- Surfboards[grep("00-00",Surfboards$V8,invert=TRUE),]
levels(Surfboards$V2) <- c(levels(Surfboards$V2),"6-2 Gullwing")
Surfboards[142,2] <- '6-2 Gullwing'
Surfboards <- Surfboards[grep("Mckevlins",Surfboards$V8,invert=TRUE),]
Surfboards <- Surfboards[grep("unshape",Surfboards$V2,invert=TRUE),]
Surfboards <- Surfboards[grep("Catch Surf",Surfboards$V1,invert=TRUE),]
Surfboards <- Surfboards[grep("6-8 Mini",Surfboards$V2,invert=TRUE),]
#Remove all returns (V5 = R) and the previous sale of that board#
#Populate Model, SellDate, ArriveDate, and SellDays fields
Surfboards$Model <- substring(Surfboards$V2,regexpr(" ",as.character(Surfboards$V2))+1)
Surfboards$SellDate <- strptime(substring(Surfboards$V4,1,regexpr(" ",as.character(Surfboards$V4))+1),"%m/%d/%y")
Surfboards$ArriveDate <- strptime(substring(Surfboards$V6,1,regexpr(" ",as.character(Surfboards$V6))+1),"%m/%d/%y")
Surfboards$SellDays <- as.numeric(difftime(Surfboards$SellDate,Surfboards$ArriveDate,units="days"))
#Set V12 to sold within a year logical
Surfboards[which(Surfboards$SellDays >= 365),12] <- FALSE
Surfboards[which(Surfboards$SellDays < 365),12] <- TRUE
#Build training, validation, and test sets
DFS.NoNA <- DFS[!(is.na(DFS$SellYear)),]
samplingSubset <- sample(1:1183,1183,replace=F)
trainsub <- samplingSubset[1:887]
validatesub <- samplingSubset[888:1035]
testsub <- samplingSubset[1036:1183]
DFStrain <- DFS[trainsub,]
DFSvalidate <- DFS[validatesub,]
DFStest <- DFS[testsub,]
DFStestVal <- DFS[- trainsub,]
DFStrainVal <- DFS[-testsub,]
#Define a log regression model using stepAIC and use LOOCV(its broken)
logm4 <- glm(formula = SellYear ~ LengthF + StyleCat + Brand + StyleCat * Brand+StyleCat*LengthF+Brand*LengthF, family = "binomial", data = DFS.NoNA, na.action = na.exclude)
stepAIC(object = logm4, direction="backward",steps=1000)$anova
logm5 <- glm(formula = SellYear ~ LengthF + StyleCat + Brand, family = "binomial", data = DFS.NoNA, na.action = na.exclude)

#Define a tree for predictions
tree1 <- tree(formula = as.factor(SellYear) ~ LengthF + StyleCat + Price + Brand, data = DFStrain)
plot1 <- xyplot(SellDays~LengthF|Brand,data=DF,col=c("yellow","purple","green","black","red","cyan"),groups=StyleCat,pch=20,cex=1.25,main="Sell Days by Board Length",xlab="Board Length in Inches",ylab="Days Needed to Sell",auto.key = list(group=DF$StyleCat,points = FALSE, x=.6,y=.7,corner = c(0,0),col=c("yellow","purple","green","black","red","cyan")),jitter.x=TRUE)
plot2 <- xyplot(SellYear~LengthF|Brand,data=DF,col=c("yellow","purple","green","black","red","cyan"),groups=StyleCat,pch=20,cex=1,main="Success by Board Length (Near 1 Sold by a Year, 0 did not)",xlab="Board Length in Inches",ylab="Sold Within a Year (1 = Yes and 0 = No)",auto.key = list(group=DF$StyleCat,points = FALSE, x=.6,y=.7,corner = c(0,0),col=c("yellow","purple","green","black","red","cyan")),jitter.y=TRUE)
FirstMonthSellPlot <- histogram(~DF$SellDays|DF$StyleCat,data=DF,breaks=1253/30)
