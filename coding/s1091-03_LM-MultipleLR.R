#####-------------------------------------------------------
##### Business Analytics 
##### Dr. Jiun-Yu Yu
##### Department of Business Administration
##### National Taiwan University
##### Lecture 3, 29 Sep 2020
#####-------------------------------------------------------

#### HOMES3 Data
HOMES3 <- read.table("d1091-03_HOMES3.txt", header=TRUE) # Read data for analysis
attach(HOMES3)
HOMES3

pairs(HOMES3)

# Summary of regression on data HOMES3, Y~X1+X2
hm3.lm <- lm(Y ~ X1 + X2)
summary(hm3.lm)

# Beta coefficients 	# Note: such coef does not exist for intercept
hm3.lm$coef["X1"]		# hm3.lm$coef[2]
beta.X1 <- hm3.lm$coef["X1"]*sd(X1)/sd(Y)
beta.X2 <- hm3.lm$coef["X2"]*sd(X2)/sd(Y)

# Summary of regression on data HOMES3, Y~X1
hm1.lm <- lm(Y ~ X1)
summary(hm1.lm)

# Multiple Correlation Coefficient
sqrt(summary(hm3.lm)$r.squared)

detach()


#### SHIPDEPT Data
# Regression on data SHIPDEPT, Y~X1+X2+X3+X4
SHIPDEPT <- read.table("d1091-03_SHIPDEPT.txt", header=TRUE)
ship.full <- lm(Y ~ X1+X2+X3+X4, SHIPDEPT)	# complete model
summary(ship.full)

ship.13 <- lm(Y ~ X1+X3, SHIPDEPT) 		# reduced model
summary(ship.13)


## ------------------------------------------------
## Discussions on Correlation 

# Summary of regression on data SALES2 		(Slides p.3-21 ~ 3-24)
SALES2 <- read.table("d1091-03_SALES2.txt", header=TRUE)
attach(SALES2)

plot(X1, Y, xlab="X1=advertising (in $m)", ylab="Y=sales (in $m)")
plot(X1, Y, type="n",xlab="X1=advertising (in $m)", ylab="Y=sales (in $m)")
for(i in 1:12) points(X1[i], Y[i], pch=as.character(X2[i]))

fitSALES2.1 <- lm(Y ~ X1, data=SALES2)
summary(fitSALES2.1)

fitSALES2 <- lm(Y ~ X1+X2, data=SALES2)
summary(fitSALES2)

detach()


# Summary of regression on data SALES3		(Slides p.3-25 ~ 3-27)
SALES3 <- read.table("d1091-03_SALES3.txt", header=TRUE)
attach(SALES3)

plot(X1, Y, xlab="X1=advertising (in $m)", ylab="Y=sales (in $m)")
pairs(SALES3)

fitSALES3 <- lm(data=SALES3, Y ~ X1+X2)
summary(fitSALES3)

fitSALES3.1 <- lm(data=SALES3, Y ~ X2) 
summary(fitSALES3.1)

detach()


## ------------------------------------------------
## Global Usefulness Test & Nested Model Test

# HOMES3 data
summary(hm3.lm)$fstatistic	# Slide p.3-31
(qf(0.95, 2, 3))			# critical value, n=6, k=2
(1-pf(51.434, 2, 3))		# p-value
anova(hm3.lm)			# ANOVA table p.3-31

# SHIPDEPT data
summary(ship.full)$fstatistic	# ANOVA table p.3-32
(qf(0.95, 4, 15))			# critical value, n=20, k=4
(1-pf(17.035, 4, 15))		# p-value


## Nested F-statistic
ship.0 <- lm(Y~1, data=SHIPDEPT)
anova(ship.0, ship.full)	# ANOVA table p.3-35, upper (C)
anova(ship.0, ship.13)		# ANOVA table p.3-35, lower (R)
anova(ship.13, ship.full)	# Nested F-stat p.3-35 & 3-37
	# anova(ship.full), anova(ship.13)

(qf(0.95, 2, 15))
(1-pf(0.472, 2 ,15))



## ------------------------------------------------
## Individual t-test

# SHIPDEPT data
summary(ship.13)



## ------------------------------------------------
## Confidence Intervals for regression coefficients & for E(Y) 
## Prediction Interval for a single Y

confint(ship.13)

xnew <- data.frame(X1=6, X3=20)
predict(ship.13, xnew, interval="confidence", level=0.95)
predict(ship.13, xnew, interval="prediction", level=0.95)



