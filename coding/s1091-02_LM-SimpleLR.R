#####-------------------------------------------------------
##### Business Analytics
##### Dr. Jiun-Yu Yu
##### Department of Business Administration
##### National Taiwan University
##### Lecture 2, 22 Sep 2020
#####-------------------------------------------------------


###-----------------------------------------###
###     Exploratory Data Analysis (EDA)     ###
###-----------------------------------------###

### Summarizing data

HOMES1 <- read.table("d1091-02_HOMES1.txt",header=TRUE)   # Read data for analysis
attach(HOMES1)
Y

hist(Y)

histY <- hist(Y, freq = FALSE, breaks = c(150,175,200,225,250,275,300,325,350,375,400), 
         main="Histogram of Y", ylab = "Frequency", xlab = "Y(price in $ thousands)")   
		# p.2-8

boxplot(Y, main="Boxplot of Y")   # boxplot, p.2-8

mean(Y);  median(Y)
sd(Y);  var(Y)
min(Y);  max(Y);  range(Y)
quantile(Y, c(0.25,0.5,0.75));  summary(Y)
length(Y)


library(MASS)
library(fBasics)

skewness(Y)	# works with package ¡§fBasics¡¨
kurtosis(Y)	# works with package ¡§fBasics¡¨

detach()



###------------------------------------
### Representing Multivariate Data 
##  Everitt, Chapter 2: 2.1 ~ 2.2

airpoll <- source("d1091-02_airpoll.dat")$value
attach(airpoll)

# Setting plot area
par(mfrow=c(2,2))
par(pty="s")

# First scatter plot
plot(SO2, Mortality, pch=1, lwd=1)
title("(a)", lwd=2)

# Second scatter plot with regression line
plot(SO2, Mortality, pch=1, lwd=1)
abline(lm(Mortality~SO2), lwd=2)
title("(b)", lwd=2)

# Jittered plot
table(SO2)
subset(airpoll, SO2==1)$Mortality
airpoll1 <- jitter(cbind(SO2,Mortality), amount=3)
plot(airpoll1[,1], airpoll1[,2], xlab="SO2", ylab="Mortality", pch=1, lwd=1)
title("(c)", lwd=2)

# Rugged plot
plot(SO2, Mortality, pch=1, lwd=1)
rug(jitter(SO2), side=1)
rug(jitter(Mortality), side=2)
title("(d)", lwd=2)
#
dev.off()
#

# Integrated Plots
par(fig=c(0,0.7,0,0.7))
plot(SO2, Mortality, lwd=1)
abline(lm(Mortality~SO2), lwd=1)
#
par(fig=c(0,0.7,0.65,1), new=TRUE)
hist(SO2, lwd=1)
par(fig=c(0.65,1,0,0.7), new=TRUE)
boxplot(Mortality,lwd=1)
#
dev.off()
#

# Scatterplot Matrix
pairs(airpoll)

pairs(airpoll, panel=function(x,y)
	{abline(lsfit(x,y)$coef, lwd=1); points(x,y)})




###-----------------------------------------###
###n     Simple Linear Regression (SLR)     ###
###-----------------------------------------###


## Scatterplot + regression line
HOMES2 <- read.table("d1091-02_HOMES2.txt",header=TRUE) #Read data for analysis
attach(HOMES2)
fit <- lm(Y~X)
summary(fit)

plot(X,Y,ylab = "Y=sale price (in thousands of dollars)", xlab = "X=floor size")
abline(fit)

## Manual calculations ----------------------------------------------------
SXYd <- sum( (X-mean(X))*(Y-mean(Y)) )
SXd2 <- sum( (X-mean(X))^2 )
b1 <- SXYd/SXd2			# = coefficient of X = b1.hat
b0 <- mean(Y)-b1*mean(X)	# = coefficient of intercept = b0.hat

SYd2 <- sum( (Y-mean(Y))^2 )		# Note that this is in fact SST!
SSE <- sum(residuals(fit)^2)		# Also: sum( (fitted(fit)-Y)^2 )

R.sq <- 1-SSE/SYd2

n <- length(Y)

sigma.hat <- sqrt(SSE/(n-2))		# summary(fit)$sigma

se.b1 <- sqrt(summary(fit)$sigma^2/SXd2)
	# = standard error of b1.hat
se.b0 <- sqrt( summary(fit)$sigma^2 * (1/length(X) + mean(X)^2/SXd2) )
	# = standard error of b0.hat


## Summary of regression ---------------------------------------------------
summary(fit)			# R.sq, sigma.hat, se.b0, se.b1 are printed

confint(fit)			# confidence intervals for model parameters
confint(fit, "X", level=0.90)	# confidence interval for b_1, alpha=0.10

	## Advanced practice
	anova(fit)
	#! sqrt('Residual Mean Sq' in anova()) = 'Residual standard error' in summary()
	#! (In R) anova() and summary() cannot be derived from each other, as the info 
	#! about (X-X_bar) is not shown in anova(), and the info about (Y-Y_bar)
## -------------------------------------------------------------------------


## Diagnostic Plots - Residual plot
# par(mfrow=c(1,2))
plot(X, residuals(fit), xlab="X", ylab="Residuals")
abline(h=0)

plot(fitted(fit), residuals(fit), xlab="Fitted", ylab="Residuals")
abline(h=0)

## Diagnostic Plots - Histogram on Residuals
hist(residuals(fit))

## Diagnostic Plots - QQ-plot
fit$fitted;		fitted(fit)
fit$residual;	residuals(fit)
qqnorm(residuals(fit), ylab="Residuals")
qqline(residuals(fit))


## Prediction Interval of Y given new observed x
xnew <- data.frame(X=2)
predict(fit, xnew, interval="prediction", level=0.95)
# predict(fit, xnew, interval="prediction", level=0.95)
# predict(fit, xnew, interval="confidence", level=0.95)


## CI plot
xy <- data.frame(X=pretty(X))
yhat <- predict(fit, newdata=xy, interval="confidence")
ci <- data.frame(lower=yhat[,"lwr"], upper=yhat[,"upr"])
plot(X,Y, main ="Confidence Interval",
	ylab = "Y = sale price (in thousands of dollars)", 
	xlab = "X = floor size (in thousands of dollars)")
abline(fit)
lines(xy$X, ci$lower, lty=2, col="red")
lines(xy$X, ci$upper, lty=2, col="blue")



## Advanced Practice: CI & PI plot
yhat.pi <- predict(fit, newdata=xy, interval="prediction")
pi <- data.frame(lower=yhat.pi[,"lwr"], upper=yhat.pi[,"upr"])
plot(X,Y, main ="Confidence and Prediction Intervals",
	ylab = "Y = sale price (in thousands of dollars)", 
	xlab = "X = floor size (in thousands of dollars)")
abline(fit)
lines(xy$X, ci$lower, lty=2, col="red")
lines(xy$X, ci$upper, lty=2, col="red")
lines(xy$X, pi$lower, lty=2, col="blue")
lines(xy$X, pi$upper, lty=2, col="blue")



## -------------------------------------------------------------------------
## Some additional info 

summary(fit)$sigma		# residual standard error
summary(fit)$r.squared		# R^2
summary(fit)$adj.r.squared	# adjusted R^2

summary(fit)$cov.unscaled
summary(fit, correlation=TRUE)$correlation



