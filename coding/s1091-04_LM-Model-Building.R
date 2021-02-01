#####-------------------------------------------------------
##### Business Analytics 
##### Dr. Jiun-Yu Yu
##### Department of Business Administration
##### National Taiwan University
##### Lecture 4, 06 Oct 2020
#####-------------------------------------------------------

### Regression Model Building ###

### Transformation ###

## Log on X
TVADS <- read.table("d1091-04_TVADS.txt",header=TRUE)
attach(TVADS)
fitTVADS <- lm(Y~X)

par(mfrow=c(1,2))
plot(X,Y,ylab = "Y= retained impressions (in $m)",xlab = "X = spending (in $m)")
abline(fitTVADS) 
summary(fitTVADS) 

fitTVADSlog <- lm(Y~log(X))
summary(fitTVADSlog)
plot(log(X),Y,ylab = "Y= retained impressions (in $m)",xlab = "X = Nature Log of spending")
abline(fitTVADSlog)

dev.off()	# close the plot
detach()

## Squares on X
HOMES4 <- read.table("d1091-04_HOMES4.txt",header=TRUE)
fitHOMES4 <- lm(data=HOMES4, Y~X)
summary(fitHOMES4)

fitHOMES4sqr <- lm(data=HOMES4, Y~X+Xsq)
summary(fitHOMES4sqr) 

attach(HOMES4)
newX <- seq(min(X),max(X),Length=100)
newXsq <- newX^2
plot(X,Y)
lines(newX,predict(fitHOMES4sqr,newdata=data.frame(X=newX,Xsq=newXsq)))
abline(fitHOMES4) 

dev.off()
detach()

## Reciprocal on X
CARS3 <- read.table("d1091-04_CARS3.txt",header=TRUE,sep="\t") 
fitCARS3 <- lm(data=CARS3,Y~X)
summary(fitCARS3) 
	par(mfrow=c(1,2))
plot(CARS3[,3],CARS3[,2],xlab="X = weight (thousand pounds)", ylab="Y = city miles per gallon")
abline(fitCARS3) 

recipX <- (1/CARS3$X)
fitCARS3recip <- lm(CARS3$Y~recipX)
summary(fitCARS3recip) 
plot(recipX,CARS3$Y,xlab="recip X = 1/weight", ylab="Y = city miles per gallon")
abline(fitCARS3recip) 

## Log on Y
WORKEXP <- read.table("d1091-04_WORKEXP.txt",header=TRUE)
fitWORKEXP <- lm(data=WORKEXP,Y~X)
summary(fitWORKEXP) 
plot(WORKEXP$X,WORKEXP$Y,xlab="X = experience (years)", ylab="Y = salary ($ thousands)")
abline(fitWORKEXP)

lnY <- log(WORKEXP$Y)
fitWORKEXPln <- lm(lnY~WORKEXP$X)
summary(fitWORKEXPln)
plot(WORKEXP$X, lnY, xlab="X = experience (years)", ylab="lnY = nature log of salary ($ thousands)")
abline(fitWORKEXPln)

## Transformations for Y & X
HOMETAX <- read.table("d1091-04_HOMETAX.txt",header=TRUE)
fitHOMETAX <- lm(data=HOMETAX,Y~X)
summary(fitHOMETAX) 
plot(HOMETAX$X,HOMETAX$Y,xlab="X = sale price ($ thousand)", ylab="Y = annual tax ($)")
abline(fitHOMETAX) 

lnY <- log(HOMETAX$Y)
lnX <- log(HOMETAX$X)
fitHOMETAXln <- lm(lnY~lnX)
summary(fitHOMETAXln) 
plot(lnX, lnY, xlab="X = long of sale price ($ thousand)", ylab="Y = log of annual tax ($)")
abline(fitHOMETAXln)

	Xnew <- data.frame(X=c(100, 150, 200))
	predict(fitHOMETAX, Xnew, interval="predict")
	lnXnew <- data.frame(lnX=log(c(100, 150, 200)))
	exp(predict(fitHOMETAXln, lnXnew, interval="predict"))


### Box-Cox Transformation for response ###

library(MASS)
par(mfrow=c(1,2))
boxcox(fitWORKEXP, plotit=T)
boxcox(fitWORKEXP, plotit=T, lambda=seq(-0.5, 0.5, by=0.1))


## Outlier ##------------------

CARS5 <- read.table("d1091-04_CARS5.txt",header=TRUE,sep="\t") #Read data for analysis
fitCARS5 <- lm(data=CARS5,Y~recipX1+recipX3+recipX5)
summary(fitCARS5)

resCARS5 <- rstandard(fitCARS5)	# studentized residuals
hist(resCARS5, breaks=20)

resCARS5[resCARS5<(-3) | resCARS5>3]
	CARS5[47,]

CARS5a <- CARS5[-47,]	# remove Volkswagen Jetta GLS TDI 4dr, row 47

fitCARS5a <- lm(data=CARS5a, Y~recipX1+recipX3+recipX5)
summary(fitCARS5a) 

resCARS5a <- rstandard(fitCARS5a)
hist(resCARS5a)

## Leverage ##------------------

levCARS5a <- hatvalues(fitCARS5a)

k <- 3; n <- 49
( thr3 <- 3*(k+1)/n )
( thr2 <- 2*(k+1)/n )

plot(levCARS5a, xlab="ID Number", ylab="Leverage") 
	abline(h=thr3, lty=2); abline(h=thr2,lty=3)

identify(levCARS5a, labels=CARS5a$name)

levCARS5a[levCARS5a>0.3]	# "0.3" is observed from the plot.

CARS5b <- CARS5a[-2,]		# remove Audi A4 1.8T 2dr, row 2

fitCARS5b <- lm(data=CARS5b,Y~recipX1+recipX3+recipX5)
summary(fitCARS5b)


## Cook's Distance ##------------------

cookCARS5 <- cooks.distance(fitCARS5)

plot(cookCARS5, xlab="ID number")

identify(cookCARS5, labels=CARS5$name)
cookCARS5[cookCARS5>1.0]

cookCARS5a <- cooks.distance(fitCARS5a)
plot(cookCARS5a, xlab="ID number")
identify(cookCARS5a, labels=CARS5a$name)


## Varaible Selection ##------------------

statedata <- read.table("d1091-04_statedata.txt", header=TRUE, sep="\t")
summary(statedata)

## Testing-Based Procedure

std <- lm(Life.Exp ~ ., data=statedata)
summary(std)

std <- update(std, . ~ . - Area)
summary(std)
std <- update(std, . ~ . - Illiteracy)
summary(std)
std <- update(std, . ~ . - Income)
summary(std)
std <- update(std, . ~ . - Population)
summary(std)

summary(lm(Life.Exp ~ Illiteracy+Murder+Frost, statedata))

# note: EDA...
	pairs(statedata)

## Criterion-Based Procedure

std <- lm(Life.Exp ~ ., data=statedata)

stdaic <- step(std)
summary(stdaic)


## Regression Pitfalls ##------------------
## Autocorrelation ##

OIL <- read.table("d1091-04_OIL.txt",header=TRUE) #Read data for analysis
fitOIL <- lm(data=OIL,log(Y)~X)
summary(fitOIL) 
resOIL <- rstandard(fitOIL)
plot(OIL$X, resOIL) 

attach(OIL)
Yt <- Y[1:20];   Yt1 <- Y[2:21];   Xt <- X[2:21]
OILlag <- lm(log(Yt1)~Xt+log(Yt))
summary(OILlag)

resOILlag <- rstandard(OILlag)
plot(Xt, resOILlag)

detach()

## Multicollinearity ##

SALES3 <- read.table("d03_SALES3.txt",header=TRUE)  # Read data for analysis
fitSALES3 <- lm(data=SALES3,Y~X1+X2)
summary(fitSALES3) 
anova(fitSALES3) 
pairs(cbind(Y=SALES3[,1], X1=SALES3[,2], X2=SALES3[,3]))

# VIF
library(car)
vif(fitSALES3)

	fitSALES3p2 <- lm(data=SALES3,Y~X2) 
	summary(fitSALES3p2)

sumX1X2 <- SALES3$X1+SALES3$X2
fitSALES3mix <- lm(SALES3$Y~sumX1X2)
summary(fitSALES3mix) 

## Excluding Important Explanatory Variables ##

PARADOX <- read.table("d1091-04_PARADOX.txt",header=TRUE)  # Read data for analysis
fitPA1 <- lm(data=PARADOX,Y~X1)
summary(fitPA1) # Table on p.176
plot(PARADOX$X1,PARADOX$Y,xlab="X1 = speed", ylab="Y = quality")
abline(fitPA1)

fitPA2 <- lm(data=PARADOX,Y~X1+X2)
summary(fitPA2) # Table on p.179
plot(PARADOX$X1,PARADOX$Y,xlab="X1 = speed", ylab="Y = quality")

plot(PARADOX$X1,PARADOX$Y, xlab="X1 = speed", ylab="Y = quality", type="n")
for(i in 1:27) points(PARADOX$X1[i],PARADOX$Y[i],
	pch=as.character(PARADOX$X2[i]))
legend(5, 1.5, legend="Points marked by X2 = angle")

par(mfrow=c(1,2))

## Missing Data ##

MISSING <- read.table("d1091-04_MISSING.txt",header=TRUE,sep="\t",na.strings="NA")

fitMISSING <- lm(data=MISSING,Y~X1+X2+X3+X4)
summary(fitMISSING) 

fitMISSING.1 <- lm(data=MISSING,Y~X2+X3+X4)
summary(fitMISSING.1) 

fitMISSING.2 <- lm(data=MISSING,Y~X1+X3+X4)
summary(fitMISSING.2)

fitMISSING.23 <- lm(data=MISSING,Y~X1+X4)
summary(fitMISSING.23)




## -----------------------------------------
## Some additional useful info for assinment

# Identify points and put on their labels on a plot
plot(x, y)		# x, y are axis variables
identify(x, y, label)

# Remove some columns or rows from dataset
dtnew <- dt[-c(1,4,9), ]	# rows 1, 4, 9 are removed to form new dataset
dtnew <- dt[ ,-c(2:4)]		# columns 2 to 4 are removed

