#####-------------------------------------------------------
##### Business Analytics
##### Dr. Jiun-Yu Yu
##### Department of Business Administration
##### National Taiwan University
##### Lecture 5, 13 Oct 2020
#####-------------------------------------------------------


##----------------------------------------------------------------------------
mm <- read.table("d1091-05_managers.txt", header=T, sep="\t")

attach(mm)
plot(Years.of.Experience, Salary, xlab="Years of Experience", ylab="Salary ($000)")

plot(Years.of.Experience, Salary, type="n", xlab="Years of Experience", ylab="Salary ($000)")
for (i in 1:174) 
	points(Years.of.Experience[i], Salary[i], pch=as.character(substr(Sex[i],1,1)), col=(Group[i]+1)*2 )

cor(Years.of.Experience, Salary)


## Regression on Subsets
lmf <- lm(Salary~Years.of.Experience, data=subset(mm, Sex=="female"))
lmm <- lm(Salary~Years.of.Experience, data=subset(mm, Sex=="male"))
summary(lmf); summary(lmm)
abline(coef=coef(lmf), col=2)
abline(coef=coef(lmm), col=4)

# Combining Regressions with Interaction
lm1 <- lm(Salary ~ Years.of.Experience * Sex, data=mm)
summary(lm1)

plot(Years.of.Experience, Salary, type="n", xlab="Years of Experience", ylab="Salary ($000)")
for (i in 1:174) 
	points(Years.of.Experience[i], Salary[i], pch=as.character(substr(Sex[i],1,1)), col=(Group[i]+1)*2 )
for (i in 0:1)
	lines(sort(Years.of.Experience[Group==i]), sort(fitted(lm1)[Group==i]), col=(i+1)*2 )


## Comparing the Variances of Residuals
par(mfrow=c(1,2))
plot(fitted(lm1), residuals(lm1), type="n", xlab="Estimated Salary", ylab="Residual Salary")
for (i in 1:174) 
	points(fitted(lm1)[i], residuals(lm1)[i], pch=as.character(substr(Sex[i],1,1)), col=(Group[i]+1)*2 )
abline(h=0, lty=3)

boxplot(lm1$residual~Sex)

qqnorm(lm1$residual);  qqline(lm1$residual)

	# plot(lm1)

## Interactions & Inference
library(car)
vif(lm1)

lm0 <- lm(Salary ~ Years.of.Experience + Sex, data=mm)
summary(lm0)
vif(lm0)

plot(Years.of.Experience, Salary, type="n", xlab="Years of Experience", ylab="Salary ($000)")
for (i in 1:174) 
	points(Years.of.Experience[i], Salary[i], pch=as.character(substr(Sex[i],1,1)), col=(Group[i]+1)*2 )
for (i in 0:1) 
	lines(sort(Years.of.Experience[Group==i]), sort(fitted(lm0)[Group==i]), col=(i+1)*2 )

detach()


##----------------------------------------------------------------------------
## ANCOVA with qualitative predictor having more than 2 levels ##
RC <- read.table("d1091-05_revcost.txt", header=T)
Rv <- RC[,1];	Cc <- RC[,2];	St <- RC[,3]
    D1 <- factor(as.numeric(RC[,3]=="CA"))	# note the use of "factor"
    D2 <- factor(as.numeric(RC[,3]=="WA"))
    fitRC <- lm(Rv~Cc+D1+D2+Cc:D1+Cc:D2)
summary(fitRC)
	model.matrix(fitRC)

plot(Cc,Rv,type="n", xlab="annual cost ($m)", ylab="annual revenue ($m)")
for(i in 1:12) points(Cc[i],Rv[i], pch=as.character(St[i]))
for(i in 0:1) {
  for(j in 0:1) {
    lines(Cc[D1==i & D2==j],fitted(fitRC)[D1==i & D2==j])
  }
}


## Advanced technique ##
fitRC1 <- lm(Rv~Cc*St)
summary(fitRC1)		# Note the reference level of St.

St <- relevel(St, ref="OR")
fitRC1 <- lm(Rv~Cc*St)
summary(fitRC1)
	model.matrix(fitRC1)

St <- relevel(St, ref="WA")
fitRC2 <- lm(Rv~Cc*St)
summary(fitRC2)



##----------------------------------------------------------------------------
ss <- read.table("d1091-05_retail_sales.txt", header=T, sep="\t")

summary(ss[,1:4])
cor(ss[,1:3])
pairs(ss[,1:3], col=as.numeric(ss[,4])+1, pch=as.numeric(ss[,4]),
	main="Rural:Red, Suburban:Green, Urban:Blue")

levels(ss[,4])

attach(ss)
ss.lm1 <- lm(Sales~Income*Market+Population)
summary(ss.lm1)

# Model Diagnosis: Checking Assumptions
par(mfrow = c(2,2))
plot(ss.lm1)

detach()


