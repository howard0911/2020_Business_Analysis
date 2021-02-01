#####-------------------------------------------------------
##### Business Analytics 
##### Dr. Jiun-Yu Yu
##### Department of Business Administration
##### National Taiwan University
##### Lecture 11, 24 Nov 2020
#####-------------------------------------------------------

library(Sleuth2)

### Case 2001 [Donner Party] -------------------------------
attach(case2001)
summary(case2001)


case2001.lg1 <- glm(Status~Age, binomial)
summary(case2001.lg1)

case2001.lg2 <- glm(Status~Age+Sex, binomial)
summary(case2001.lg2)

#
case2001.lg3 <- glm(Status~Age*Sex, binomial)
summary(case2001.lg3)
	confint(case2001.lg3)
	confint(case2001.lg2);	exp(confint(case2001.lg2)[3,])

#
Agesq <- Age^2
case2001.lg4 <- glm(Status~(Age+Agesq)*Sex, binomial)
summary(case2001.lg4)

summary(update(case2001.lg4, .~.-Agesq:Sex))
summary(update(case2001.lg4, .~.-Agesq:Sex-Agesq))

anova(case2001.lg3,case2001.lg4,  test="Chisq")

   drop1(case2001.lg4, , test="Chisq")

step(case2001.lg4)
 # Call:  glm(formula = Status ~ Agesq + Sex + Agesq:Sex, family = binomial) 
 # Residual Deviance: 46.57        AIC: 54.57 
 #
 # cf lg2 & lg3:
 # lg2: Residual deviance: 51.256  on 42 df,  AIC: 57.256
 # lg3: Residual deviance: 47.346  on 41 df,  AIC: 55.346

anova(case2001.lg2, case2001.lg3, test="Chisq")


## Display 20.9, with case2001.lg2, slide p.12-42
SF <- subset(case2001, Sex=="Female")
SM <- subset(case2001, Sex=="Male")
sq <- seq(14,66,1)
respF <- predict(case2001.lg2, type="response", newdata=data.frame(Age=sq, Sex="Female"))
respM <- predict(case2001.lg2, type="response", newdata=data.frame(Age=sq, Sex="Male"))

plot(sq, respF, type="l", col=2, ylim=c(0,1), ylab="Predicted Probability", 
	xlab="Age (years)", main="Logistic regression without interaction")	# bty="L",
lines(sq, respM, col=4)
text(x=50, y=0.4, "Females", adj=c(0,0))
text(x=40, y=0.2, "Males", adj=c(0,0))

case2001$SP <- ifelse(case2001$Status=="Survived", 1, 0)	# survived or died
points(SF$Age, jitter(SF$SP, factor=0.2), pch=17)
points(SM$Age, jitter(SM$SP, factor=0.2), pch=1)
legend(50,1, legend=c("Female","Male"), pch=c(17,1))


## Display 20.9: Plot points first then lines, with case2001.lg3
respF <- predict(case2001.lg3, type="response", newdata=data.frame(Age=sq, Sex="Female"))
respM <- predict(case2001.lg3, type="response", newdata=data.frame(Age=sq, Sex="Male"))

plot(SF$Age, jitter(SF$SP, factor=0.2), pch=17, 
	ylim=c(0,1), xlim=c(14,66), ylab="Predicted Probability",  
	xlab="Age (years)", main="Logistic regression with interaction")
points(SM$Age, jitter(SM$SP, factor=0.2), pch=1)
legend(50,1, legend=c("Female","Male"), pch=c(17,1))
lines(sq, respF, col=2)
lines(sq, respM, col=4)
text(x=40, y=0.4, "Females", adj=c(0,0))
text(x=60, y=0.2, "Males", adj=c(0,0))


detach()



### Case 2002 [birdkeeping & lung cancer] ----------------------------
attach(case2002)
summary(case2002)

## Display 20.7
full <- glm(LC ~ FM + SS + AG + YR + BK, family = "binomial")
reduced <- glm(LC ~ FM + SS + AG + YR, family = "binomial")
summary(full)
summary(reduced)

	( dvr <- deviance(reduced) )
	( dvf <- deviance(full) )
	( dfr <- df.residual(reduced) )
	( dff <- df.residual(full) )
	1 - pchisq(dvr-dvf, dfr-dff)

anova(reduced, full, test="Chisq")


## Display 20.10
BC <- subset(case2002, (BK=="Bird" & LC=="LungCancer"))
BN <- subset(case2002, (BK=="Bird" & LC=="NoCancer"))
NC <- subset(case2002, (BK=="NoBird" & LC=="LungCancer"))
NN <- subset(case2002, (BK=="NoBird" & LC=="NoCancer"))

plot(YR~AG, type="n", ylab="Years of Smoking", xlab="Age")
points(BC$AG, BC$YR, pch=17)
points(BN$AG, BN$YR, pch=2)
points(NC$AG, NC$YR, pch=19)
points(NN$AG, NN$YR, pch=1)

legend(40,45, legend=c("BC","BN","NC","NN"), pch=c(17,2,19,1))


## Display 20.11
YR.g <- cut(YR, breaks = c(0,1,20,30,40,50), include.lowest = T)
( avgYR.g <- tapply(YR, YR.g, mean) )	# group average year of smoking
( n.g <- table(YR.g) )				# group total no.
( LC.b <- ifelse(case2002$LC=="LungCancer", 1, 0) )	# lungcancer or not
( lc.g <- tapply(LC.b, YR.g, sum) )		# group total no. of lungcancer
( nc.g <- n.g - lc.g )
( logit.g <- log(lc.g/nc.g) )
plot(avgYR.g, logit.g, pch=19, xlab="Year Smoked", ylab="Sample Logit")




