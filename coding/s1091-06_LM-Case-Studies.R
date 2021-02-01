#####-------------------------------------------------------
##### Business Analytics 
##### Dr. Jiun-Yu Yu
##### Department of Business Administration
##### National Taiwan University
##### Lecture 6, 20 Oct 2020
#####-------------------------------------------------------

############ HOMES6 example ###--------------------------------
## Scatterplot matrix ##
HOMES6 <- read.table("d1091-06_HOMES6.txt", header=TRUE, sep="\t")
colnames(HOMES6)
attach(HOMES6)
	levels(status);  levels(elem)
plot(HOMES6[,-c(1,7,8,10,12,13,14,15,16,17,18,19)])

## boxplot  ##
table(D7);	table(status)
table(elem)
par(mfrow=c(1,2))
boxplot(Y~D7, ylab="Y=sale price in $thousands", xlab="D7=indicator for active status")
boxplot(Y~elem, ylab="Y=sale price in $thousands", xlab="nearest elementary school")

##--------------------------------------------------------------------XXX
  ## Model ##
  mod1 <- lm(Y ~ X1+X2+X3+X4+X5+X6+D7+D8+D9+D10+D11+D12)
  summary(mod1)

  plot(X5,residuals(mod1),xlab="X5",ylab="Residuals")
  abline(h=0, lty=2)

  mod2 <- lm(Y ~ X1+X2+X3+X4+X3X4+X5+X5sq+X6+D7+D8+D9+D10+D11+D12)
  summary(mod2)

  # Nested model test#
  anova(mod1, mod2)

  plot(X5,residuals(mod2),main="mod2",xlab="X5",ylab="Residuals")
  abline(h=0, lty=2)

  mod3 <- update(mod2, .~. -D10-D11-D12)	# Q: What is the implication of this step?
  	# mod3 <- lm(Y ~ X1+X2+X3+X4+X3X4+X5+X5sq+X6+D7+D8+D9)
  summary(mod3)

  anova(mod3,mod2)

  ## Final model ##
  confint(mod3)

  plot(X5,residuals(mod3),main="mod3",xlab="X5",ylab="Residuals")
  abline(h=0, lty=3)

  # Outlier
  resmod3 <- rstandard(mod3)
  hist(resmod3)

  # Leverage
  levmod3 <- hatvalues(mod3)
  plot(levmod3, xlab="id",ylab="Leverage")
  identify(levmod3, labels=HOMES6$id)
	
  k <- 11; n <- 76
  levmod3[levmod3 > 3*(k+1)/n]
  levmod3[c(76, 54)]

  # Cook's distance
  cookmod3 <- cooks.distance(mod3)
  plot(cookmod3, xlab="id",ylab="Cook's distance")
  identify(cookmod3, labels=HOMES6$id)
##--------------------------------------------------------------------XXX

####-----------------------------------------------####
#### !!!!  Alternative (and better) solution  !!!! ####
####-----------------------------------------------####

mh1 <- lm(Y ~ X1+X2+X3*X4+X5+X5sq+X6+status+elem)
summary(mh1)

# Change the levels of "status"
detach()
HOMES6$status.n <- HOMES6$status
levels(HOMES6$status.n) <- list(act="act", nac="pen", nac="sld")
HOMES6$status.n <- relevel(HOMES6$status.n, ref="nac")

attach(HOMES6)
mh2 <- lm(Y ~ X1+X2+X3*X4+X5+X5sq+X6+status.n+elem)
summary(mh2)

elem <- relevel(elem, ref="edge")
mh2.1 <- lm(Y ~ X1+X2+X3*X4+X5+X5sq+X6+status.n+elem)
summary(mh2.1)	#-> the same as mod2


detach()



############ CARS6 example ###--------------------------------
## scatterplot ##
CARS6 <- read.table("d1091-06_CARS6.txt", header=TRUE, sep="\t")
colnames(CARS6);   summary(CARS6)

attach(CARS6)
table(type);  table(drive)
plot(CARS6[,-c(1,2,3,4,6,12,13,14,15,16,17,18,19,20)])

## boxplot ## 
par(mfrow=c(1,2))
plot(Y~factor(type), ylab="Y=city MPG", xlab="type of vehicle")
plot(Y~factor(drive), ylab="Y=city MPG", xlab="drive type")

##--------------------------------------------------------------------XXX
  ## Model 1 ##
  carsmod1 <- lm(Y ~ X1+X2+X3+X4+X5+D6+D7+D8+D9+D10+D11+D12)
  summary(carsmod1)
  rescarsmod1 <- rstandard(carsmod1)
  hist(rescarsmod1)

  rescarsmod1[rescarsmod1<(-3)|rescarsmod1>3]

  ## Model 2 ##
  CARS6a <- CARS6[-c(88,13),]	# remove outliers...
  carsmod2 <- lm(Y ~ X1+X2+X3+X4+X5+D6+D7+D8+D9+D10+D11+D12,CARS6a)
  summary(carsmod2)

  plot(residuals(carsmod2))
  abline(h=0, lty=2)	# fail to satify the zero assumption...

  ## Model 3: reciprocal transformation ##
  CARS6$recipX1 <- 1/X1
  CARS6$recipX2 <- 1/X2
  CARS6$recipX3 <- 1/X3
  CARS6$recipX4 <- 1/X4
  CARS6$recipX5 <- 1/X5
  CARS6a <- CARS6[-c(88,13),]
  carsmod3 <- lm(Y ~ recipX1+recipX2+recipX3+recipX4+recipX5+D6+D7+D8+D9+D10+D11+D12,CARS6a)
  summary(carsmod3)

  rescarsmod3 <- rstandard(carsmod3)
  hist(rescarsmod3)
  qqnorm(rescarsmod3);  qqline(rescarsmod3)
  plot(rescarsmod3)
  plot(fitted(carsmod3), rescarsmod3, xlab="fitted values", ylab="residuals")

  levcarsmod3 <- hatvalues(carsmod3)
  plot(levcarsmod3, xlab="id",ylab="Leverage")

  cookcarsmod3 <- cooks.distance(carsmod3)
  plot(cookcarsmod3, xlab="id",ylab="Cook's distance")

  carsmod <- update(carsmod3, .~.-D8-D9)
  anova(carsmod, carsmod3)
  summary(carsmod)


  ## Final model ##
  CARS6$recipX1<-1/X1
  CARS6$recipX2<-1/X2
  CARS6$recipX3<-1/X3
  CARS6$recipX4<-1/X4
  CARS6$recipX5<-1/X5
  CARS6a<-CARS6[-c(88,13),]	# remove outliers...

  carsmodf <- lm(Y ~ (recipX1+recipX2+recipX3+recipX4+recipX5)*(D6+D7+D8+D9+D10+D11+D12),CARS6a)
  summary(carsmodf)

  carsmods <- step(carsmodf)
  summary(carsmods)
    	# library(MASS)
	# carsmodt <- stepAIC(carsmod)
	# summary(carsmodt)

  carsmod4 <- lm(Y ~ recipX1+recipX2+recipX3+recipX4+recipX5+D6+D7+D10+D11+D12
                  +D7*recipX1+D11*recipX1+D12*recipX1+D6*recipX2+D10*recipX2
                  +D12*recipX2+D6*recipX3+D10*recipX3+D10*recipX4+D11*recipX4
                  +D12*recipX4+D6*recipX5+D7*recipX5+D10*recipX5,CARS6a)
  summary(carsmod4)		# This is actually the same as carsmods.

	plot(residuals(carsmod4),xlab="X5",ylab="Residuals")
	abline(h=0, lty=3)
	identify(residuals(carsmod4), labels=CARS6a$id)

  # Outlier
  rescarsmod4 <- rstandard(carsmod4)
  hist(rescarsmod4)

  # Leverage
  levcarsmod4 <- hatvalues(carsmod4)
  plot(levcarsmod4, xlab="id",ylab="Leverage")
  identify(levcarsmod4, labels=CARS6a$id)

  # Cook's distance
  cookcarsmod4 <- cooks.distance(carsmod4)
  plot(cookcarsmod4, xlab="id",ylab="Cook's distance")
  identify(cookcarsmod4, labels=CARS6a$id)
##--------------------------------------------------------------------XXX

####-----------------------------------------------####
#### !!!!  Alternative (and better) solution  !!!! ####
####-----------------------------------------------####

	CARS6 <- read.table("d1091-06_CARS6.txt", header=TRUE, sep="\t")
	colnames(CARS6)

CARS6$type <- relevel(CARS6$type, ref="sedan")
CARS6$drive <- relevel(CARS6$drive, ref="fwd")
CARS6$recipX1<-1/X1
CARS6$recipX2<-1/X2
CARS6$recipX3<-1/X3
CARS6$recipX4<-1/X4
CARS6$recipX5<-1/X5
CARS6a <- CARS6[-c(88,13),]	# remove outliers...

mod1 <- lm(Y ~ recipX1+recipX2+recipX3+recipX4+recipX5+type+drive, data=CARS6a)
summary(mod1)

mod2 <- lm(Y ~ (recipX1+recipX2+recipX3+recipX4+recipX5)*(type+drive), data=CARS6a)
summary(mod2)

mod3 <- step(mod2)
summary(mod3)

anova(mod2, mod3)

detach()



############ Block Cost example ###--------------------------------
cost <- read.table("d1091-06_block_cost.txt", header=T, sep='\t')
dim(cost);  names(cost)
summary(cost)

table(cost[,17:18])
table(cost[,c(22,19)]);  table(cost[,19:22])

pairs(cost[,c(1:7)])
pairs(cost[,c(1,8:12)])
cor(cost[,4:9])

attach(cost)
plot(Labor.Hours, Average.Cost)
identify(Labor.Hours, Average.Cost)	
costa <- cost[-c(19, 94),]	# remove outliers...
	cor(costa[,4:9])
detach()

cost.0 <- lm(Average.Cost ~ ., data=costa)
summary(cost.0)

### (1): backward elimination
cost.1 <- update(cost.0, .~. - Manager)
summary(cost.1)
cost.2 <- update(cost.1, .~. - Music - Shift)
summary(cost.2)
anova(cost.2, cost.1)	# Nested-model test

cost.2 <- update(cost.2, .~. - Stamp.Ops - Chisel.Ops, data=costa)
summary(cost.2)
anova(cost.2, cost.1)

cost.21 <- step(cost.2, data=costa)
summary(cost.21)

drop1(cost.21, test="F")	# Drop all possible single terms to a model

cost.22 <- update(cost.21, .~. - Weight.Final - Room.Temp, data=costa)
anova(cost.22, cost.21)
summary(cost.22)

cost.22 <- update(cost.22, .~. - Weight.Rem - Machine.Hours, data=costa)
anova(cost.22, cost.21)
summary(cost.22)


### (2): start step from full model
cost.01 <- step(cost.0)
summary(cost.01)
drop1(cost.01, test="F")
cost.02 <- update(cost.01, .~. - Machine.Hours - Room.Temp - Weight.Rem)
anova(cost.02, cost.01)
summary(cost.02)
cost.03 <- update(cost.02, .~. - Manager + Plant)
anova(cost.03, cost.02)
summary(cost.03)		# same result as summary(cost.22)




