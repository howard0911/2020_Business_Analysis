#####-------------------------------------------------------
##### Business Analytics 
##### Dr. Jiun-Yu Yu
##### Department of Business Administration
##### National Taiwan University
##### Lecture 7, 27 Oct 2020
#####-------------------------------------------------------

#### ANOVA ####
coffee <- read.table("d1091-07_coffee.txt", header=TRUE)
attach(coffee)
table(OCCU)
par(mfrow=c(1,2))
plot(INTENST~factor(OCCU), ylab="Intensity", xlab="Occupation")
with(coffee, stripchart(INTENST~factor(OCCU), vertical=TRUE, method="stack",
     ylab="Intensity", xlab="Occupation"))

cfm1 <- lm(INTENST ~ factor(OCCU))
summary(cfm1)

anova(cfm1)


## Multiple Comparison - Fisher's LSD ##
# Compare Group 1 and 2
summary(cfm1)$sigma	# sigma hat = residual standard error
(t12 <- 25.101/(15.85486*sqrt(1/5+1/8)))
	(tc.lsd <- qt(0.975, 35))	# critival value
(1-pt(t12, 35))*2 		
# equal to p-value in summary(cfm1)

## Multiple Comparison - Bonferroni correction ##
# Compare Group 1 and 2
(t12 <- 25.101/(15.85486*sqrt(1/5+1/8)))
	(tc.bon <- qt(1-0.05/20, 35))	# critival value
(1-pt(t12, 35))*2*10	
# now Groups 1 & 2 are not significantly different

## Multiple Comparison - Tukey's HSD ##
# Compare Group 1 and 2
(t12 <- 25.101/(15.85486*sqrt(1/5+1/8)))
	(tc.hsd <- qtukey(0.95, 5, 35)/sqrt(2))	# critival value
1-ptukey(t12*sqrt(2), 5, 35)

cfhsd <- TukeyHSD(aov(INTENST~factor(OCCU), coffee))
plot(cfhsd)

# Compare Group 2 and 3
(49.970-25.101)/(15.85486*sqrt(1/13+1/5))
1-ptukey(2.980687*sqrt(2), 5, 35)

## Diagnostics ##
par(mfrow=c(1,2))
qqnorm(residuals(cfm1))
plot(fitted(cfm1), residuals(cfm1), xlab="Fitted", ylab="Residuals")

detach()



#### Credibility of Ads
credads <- read.table("d1091-07_credibility.txt", header=TRUE)
attach(credads)

plot(Credibility ~ Treatment)

credads.lm <- lm(Credibility ~ Treatment)
summary(credads.lm)

qqnorm(residuals(credads.lm), ylab="Residuals")
qqline(residuals(credads.lm))

Treatment <- relevel(Treatment, ref="Tame")
	credads.lm <- lm(Credibility ~ Treatment)
	summary(credads.lm)

credads.hsd <- TukeyHSD(aov(Credibility ~ Treatment))
credads.hsd
par(fig=c(0.2,1,0,1))
plot(credads.hsd, las=1)

detach()



#### Diet Restriction & Longevity: case0501

library(Sleuth2)

summary(case0501)
attach(case0501)

Dn <- summary(Diet)
Dm <- tapply(Lifetime, Diet, mean)
Ds <- tapply(Lifetime, Diet, sd)
Dx <- tapply(Lifetime, Diet, max)
Dy <- tapply(Lifetime, Diet, min)
	tapply(Lifetime, Diet, quantile)
Dsum <- cbind(Dn, Dm, Ds, Dx, Dy)
colnames(Dsum) <- c("n", "Mean", "SD", "Max", "Min")
Dsum

par(mfrow=c(1,2))
boxplot(Lifetime~Diet)
stripchart(Lifetime~Diet, vertical=T, method="stack")

c501.lm <- lm(Lifetime~Diet)
summary(c501.lm)
confint(c501.lm)	# (e)

Diet <- relevel(Diet, ref="N/N85")
c501.lm2 <- lm(Lifetime~Diet)
summary(c501.lm2)
confint(c501.lm2)	# (a)

Diet <- relevel(Diet, ref="N/R50")
c501.lm3 <- lm(Lifetime~Diet)
summary(c501.lm3)
confint(c501.lm3)	# (b), (c), (d), (a)

( c501hsd <- TukeyHSD(aov(Lifetime~Diet)) )
par(fig=c(0.1,1,0,1))
plot(c501hsd, las=1)


# Residual plot
plot(fitted(c501.lm),residuals(c501.lm))
abline(h=0, lty=2, col='grey')



