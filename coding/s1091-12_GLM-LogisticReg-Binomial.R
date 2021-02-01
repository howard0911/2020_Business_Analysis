#####-------------------------------------------------------
##### Business Analytics 
##### Dr. Jiun-Yu Yu
##### Department of Business Administration
##### National Taiwan University
##### Lecture 12, 15 Dec 2020
#####-------------------------------------------------------

library(Sleuth2)

### Case 2101 ----------------------------------------------
case2101
attach(case2101)
Left <- Atrisk - Extinct		# this step is important!!
c2101.lg1 <- glm(cbind(Extinct, Left)~log(Area), binomial)
summary(c2101.lg1)

	eta <- c2101.lg1$coef[1]+c2101.lg1$coef[2]*log(Area)
	exp(eta)/(1+exp(eta))

# Display 21.5
( proportion <- Extinct/Atrisk )
( pihat <- fitted(c2101.lg1) )	# the fitted mean values, 
						# obtained by transforming the linear predictors 
						# by the inverse of the link function.
( rawres <- proportion - pihat )

( respea <- residuals(c2101.lg1, type="pearson") )
( resdev <- residuals(c2101.lg1, type="deviance") )

cbind(proportion, estm, rawres, respea, resdev)

qqnorm(respea);	qqline(respea)
qqnorm(resdev, pch=16);	qqline(resdev)


# Display 21.2
logit2101 <- log(proportion/(1-proportion))
plot(log(Area), logit2101, pch=20)


# Goodness-of-Fit: Deviance (Gsq)
deviance(c2101.lg1)	# = sum(resdev^2) = c2101.lg1$deviance
c2101.lg1$df.residual
1-pchisq(deviance(c2101.lg1), c2101.lg1$df.residual)

# Goodness-of-Fit: Pearson (Xsq)
( Xsq <- sum(respea^2) )
1-pchisq(Xsq, df=c2101.lg1$df.residual)


# Wald's Test & C.I. for beta_1
confint(c2101.lg1)

# LRT / Drop-in-Deviance Test for beta_1 (p. 13-21)
1-pchisq(c2101.lg1$null.deviance-deviance(c2101.lg1), 1)


detach()



### Case 2102 ----------------------------------------------
case2102
attach(case2102)

# Display 21.4
( Proportion <- Removed/Placed )

plot(Distance, Proportion, type="n")
points(Distance, Proportion, pch=ifelse(Morph=="light", 1, 16))
lines(Distance[Morph=="light"], Proportion[Morph=="light"], lty=2)
lines(Distance[Morph=="dark"], Proportion[Morph=="dark"], lty=1)


# Slide p. 13-34  (Display 21.8)
Left <- Placed - Removed
full <- glm(cbind(Removed, Left) ~ Morph * Distance, family = binomial)
summary(full)

reduced <- glm(cbind(Removed, Left) ~ Morph + Distance, family = binomial)
summary(reduced)

( x2 <- deviance(reduced) - deviance(full) )
( df0 <- (summary(reduced))$df.residual - (summary(full))$df.residual )
( p0 <- 1 - pchisq(x2, df0) )

	anova(reduced, full, test="Chisq")


# Goodness-of-Fit Test of full model,  Slide p. 13-35
1-pchisq(deviance(full), full$df.residual)

( resdev.f <- residuals(full, type="deviance") )




