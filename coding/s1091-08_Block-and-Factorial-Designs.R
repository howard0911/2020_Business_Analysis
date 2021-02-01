#####-------------------------------------------------------
##### Business Analytics
##### Dr. Jiun-Yu Yu
##### Department of Business Administration
##### National Taiwan University
##### Lecture 8, 03 Nov 2020
#####-------------------------------------------------------

####------------------------
####  Block Designs       
####------------------------ 

### Quantitative Factors and Orthogonal Polynomials
( composite <- read.table("d1091-08_composite.txt", header=T, sep="\t") )
class(composite$laser)

cm <- lm(strength ~ laser, composite)
anova(cm)
summary(cm)
# model.matrix(cm)

plot(composite$strength ~ composite$laser)

## make 'laser' ordered factors...
composite$laser.o <- as.ordered(composite$laser)
cm.o <- lm(strength ~ laser.o, composite)

	contr.poly(3)
	# model.matrix(cm.o)

anova(cm.o)
summary(cm.o)	#-> anova() unchanged, but summary() informative


## make 'laser' numerical predictors... 
composite$laser.n <- rep(c(40,50,60),3)
cm.n <- lm(strength ~ laser.n, composite)		# similar to SLM
	# summary(cm.n)
cm.m <- lm(strength ~ poly(laser.n, 2), composite)
	# summary(cm.m)

	# model.matrix(cm.m);  model.matrix(cm.o)

## 'cm.o' cannot be used for prediction:
predict(cm.n, data.frame(laser.n = 55))
predict(cm.m, data.frame(laser.n = 55))



# -----------------------------------------------------------------------
### Randomized Block Design
( girder <- read.table("d1091-08_girder.txt", header=T) )
attach(girder)
xtabs(ratio ~ method + gtype)		# table on p. 8-9

	plot(ratio ~ method)
	plot(ratio ~ gtype)

gd <- lm(ratio ~ method + gtype, girder)
anova(gd);   summary(gd)


## Multiple comparison  (p. 8-10)
(gdmean <- tapply(girder$ratio, girder$method, mean))

t.st <- rep(0,6)
names(t.st) <- c("A-C", "A-K", "A-L", "C-K", "C-L", "K-L")
se <- summary(gd)$sigma*sqrt(1/9+1/9)
t.st[1] <- as.numeric(gdmean[2]-gdmean[1]) / se
t.st[2] <- as.numeric(gdmean[3]-gdmean[1]) / se
t.st[3] <- as.numeric(gdmean[4]-gdmean[1]) / se
t.st[4] <- as.numeric(gdmean[3]-gdmean[2]) / se
t.st[5] <- as.numeric(gdmean[4]-gdmean[2]) / se
t.st[6] <- as.numeric(gdmean[4]-gdmean[3]) / se
t.st

# (p. 8-11)
(tc.lsd <- qt(0.975, 24))
(tc.bon <- qt(1-0.05/12, 24))
(tc.hsd <- qtukey(0.95, 4, 24)/sqrt(2))
   1-ptukey(t.st[1]*sqrt(2), 4, 24)

# (p. 8-12)
gdv <- aov(ratio ~ method + gtype, girder)
gdhsd <- TukeyHSD(gdv, "method")
plot(gdhsd)

	## Diagnostics...
	plot(fitted(gd), residuals(gd), xlab="Fitted", ylab="Residuals")
	abline(h=0, lty=3)
	qqnorm(residuals(gd));	qqline(residuals(gd))

   	gd0 <- lm(log(ratio)~method+gtype, girder)
   	anova(gd0);   summary(gd0)
   	qqnorm(residuals(gd0));	qqline(residuals(gd0))



# -----------------------------------------------------------------------
### Latin Sqaure Design
( abrasion <- read.table("d1091-08_abrasion.txt", header=T, sep="\t") )
matrix(abrasion$material,4,4)

attach(abrasion)
xtabs(wear ~ application + position)	# table on p. 8-15

# (p. 8-16)
par(mfrow=c(1,3))
with(abrasion,stripchart(wear ~ material,xlab="Material",vert=T))
with(abrasion,stripchart(wear ~ application,xlab="Application",vert=T))
with(abrasion,stripchart(wear ~ position,xlab="Position",vert=T))

abrasion$application <- as.factor(abrasion$application)
abrasion$position <- as.factor(abrasion$position)
ab <- lm(wear ~ material + application + position, abrasion)
	drop1(ab,test="F")
anova(ab);	summary(ab)

## Multiple comparison
(abmean <- tapply(abrasion$wear, abrasion$material, mean))
t.st <- rep(0,6)
names(t.st) <- c("A-B", "A-C", "A-D", "B-C", "B-D", "C-D")
se <- summary(ab)$sigma*sqrt(1/4+1/4)
t.st[1] <- as.numeric(abmean[2]-abmean[1]) / se
t.st[2] <- as.numeric(abmean[3]-abmean[1]) / se
t.st[3] <- as.numeric(abmean[4]-abmean[1]) / se
t.st[4] <- as.numeric(abmean[3]-abmean[2]) / se
t.st[5] <- as.numeric(abmean[4]-abmean[2]) / se
t.st[6] <- as.numeric(abmean[4]-abmean[3]) / se
t.st

(tc.lsd <- qt(0.975, 6))
(tc.bon <- qt(1-0.05/12, 6))
(tc.hsd <- qtukey(0.95, 4, 6)/sqrt(2))
   1-ptukey(t.st[1]*sqrt(2), 4, 6)

abv <- aov(wear ~ material + application + position, abrasion)
abhsd <- TukeyHSD(abv, "material")
plot(abhsd)

## Efficiency of Latin Squares
ar <- lm(wear ~ material, abrasion)
anova(ar)
summary(ar)

summary(ar)$sig^2 / summary(ab)$sig^2



# -----------------------------------------------------------------------
### Balanced Incomplete Block Design
( tirewear <- read.table("d1091-08_tirewear.txt", header=T, sep="\t") )
attach(tirewear)
xtabs(wear ~ tire + compound)		# table on p. 8-23

par(mfrow=c(1,2))
with(tirewear,stripchart(wear ~ tire,xlab="Tire",vert=T))
with(tirewear,stripchart(wear ~ compound,xlab="Compound",vert=T))

tw <- lm(wear ~ factor(tire) + compound)
anova(tw)
	# tx <- lm(wear ~ compound + factor(tire))
	# anova(tx)
summary(tw)

drop1(tw, test="F")
	tc <- lm(wear ~ compound)
	anova(tc)
	tt <- lm(wear ~ factor(tire))
	anova(tt)

k <- 3;   lambda <- 2;   t <- 4;   r <- 3;   b <- 4;
coef(summary(tw))
(twmean <- c(0, coef(summary(tw))[5:7,1] ) )
se <- summary(tw)$sigma * sqrt(2*k/(lambda*t))

t.st <- rep(0,6)
names(t.st) <- c("A-B", "A-C", "A-D", "B-C", "B-D", "C-D")
t.st[1] <- as.numeric(twmean[2]-twmean[1]) / se
t.st[2] <- as.numeric(twmean[3]-twmean[1]) / se
t.st[3] <- as.numeric(twmean[4]-twmean[1]) / se
t.st[4] <- as.numeric(twmean[3]-twmean[2]) / se
t.st[5] <- as.numeric(twmean[4]-twmean[2]) / se
t.st[6] <- as.numeric(twmean[4]-twmean[3]) / se
t.st

(tc.hsd <- qtukey(0.95, t, t*r-b-t+1)/sqrt(2))

ftire <- as.factor(tire)
twv <- aov(wear ~ ftire + compound)
twhsd <- TukeyHSD(twv, "compound")
plot(twhsd)





####------------------------
####  Factorial Designs       
####------------------------ 

### Two-Way ANOVA
xtabs( fac1 <- read.table("d1091-09_factorial1.txt", header=T) )
xtabs( fac2 <- read.table("d1091-09_factorial2.txt", header=T) )

	f1 <- lm(Response~Sex+Drug, fac1)
	f2 <- lm(Response~Drug+Sex, fac2)

par(mfrow=c(1,2))
with(fac1, interaction.plot(Sex,Drug,Response))
with(fac2, interaction.plot(Sex,Drug,Response))



# -----------------------------------------------------------------------
### Factorial Design - Two-way ANOVA
composite <- read.table("d1091-08_composite.txt", header=T, sep="\t")
class(composite$laser);   class(composite$tape)
summary(composite)

cm <- lm(strength ~ laser + tape, composite)

# Interaction plots
par(mfrow=c(1,2))
with(composite, interaction.plot(laser,tape,strength,legend=F))
with(composite, interaction.plot(tape,laser,strength,legend=F))

summary(lm(strength ~ laser * tape, composite))

summary(cm);   anova(cm)
model.matrix(cm)

## make 'laser' & 'tape' to be ordered factors...
composite$laser.o <- as.ordered(composite$laser)
composite$tape.o <- as.ordered(composite$tape)

cm.o <- lm(strength ~ laser.o + tape.o, composite)

contr.poly(3)
model.matrix(cm.o)
summary(cm.o);   anova(cm.o)

## make 'laser' & 'tape' to be numerical predictors...
composite$tape.n <- rep(c(6.42,13,27), each=3)
composite$laser.n <- rep(c(40,50,60), 3)
cm.n <- lm(strength ~ laser.n + poly(log(tape.n),2), composite)
summary(cm.n)
	summary(cm);	summary(cm.o)



# -----------------------------------------------------------------------
### Two-Way ANOVA - More than one observations per cell (1)
torque <- read.table("d1091-09_torque.txt", header=T, sep="\t")
summary(torque)

tq <- lm(value ~ test * plating, torque)
summary(tq);   anova(tq)

# Interaction plots
par(mfrow=c(1,2))
with(torque, interaction.plot(test,plating,value))
with(torque, interaction.plot(plating,test,value))

plot.design(value~test+plating, data=torque)	# main-effect plot
	model.tables(aov(value~test*plating, torque))
	mean(torque$value[torque$test=="bolt"]);   mean(torque$value)

	model.matrix(tq)		# treatment contrast

# Diagnostics
torque$tt <- factor(substr(torque$test, 1,1))

# Box-Whisker Plots
plot(torque$tt:torque$plating, tq$residuals,
 xlab="Treatment Groups",ylab="Residuals",
 main="Box-Whisker Plots of Residuals, Bolt Experiment")

plot(fitted(tq), residuals(tq), xlab="Fitted", ylab="Residuals")
abline(h=0)
qqnorm(residuals(tq));  qqline(residuals(tq))

# Transformation on response
tq0 <- lm(log(value) ~ test * plating, torque)
summary(tq0);   anova(tq0)
   par(mfrow=c(1,2))
   plot(torque$tt:torque$plating, tq0$residuals)
   qqnorm(residuals(tq0))
   qqline(residuals(tq0))

# Multiple Comparison
tqhsd <- TukeyHSD(aov(value~tt*plating, torque) )
tq0hsd <- TukeyHSD(aov(log(value)~tt*plating, torque) )
   par(mfrow=c(2,3))
   plot(tqhsd,las=2);  plot(tq0hsd,las=2)



# -----------------------------------------------------------------------
### Two-Way ANOVA - Nested Analysis
tomato <- read.table("d1091-09_tomato.txt", header=T, sep="\t")
summary(tomato)

par(mfrow=c(1,2))
with(tomato, interaction.plot(density,variety,yield,legend=F))
with(tomato, interaction.plot(variety,density,yield,legend=F))

yn <- lm(yield ~ variety / as.ordered(density), tomato)
summary(yn);  anova(yn)

summary(yn)$sigma
(t23 <- (3.93623-2.56915)/(1.2172*sqrt(1/3+1/3)) )
(tc.lsd <- qt(0.975, 12))
(1-pt(1.37554, 12))*2



# -----------------------------------------------------------------------
### General Factorial Design
softdrink <- read.table("d1091-09_softdrink.txt", header=T, sep="\t")
summary(softdrink)
attach(softdrink)

# View data in table
xtabs(heightdev ~ carbonation + speed + pressure)

# Obtain sums for each level of carbonation
m <- rep(0,3)
for (i in 1:3) m[i] <- sum(heightdev[carbonation==2*i+8])
m

# Factor
softdrink$carb.o <- as.factor(softdrink$carbonation)
softdrink$pres.o <- as.factor(softdrink$pressure)
softdrink$sped.o <- as.factor(softdrink$speed)

sd.lm <- lm(heightdev ~ carb.o * pres.o * sped.o, softdrink)
anova(sd.lm);   # summary(sd.lm)

# Obtain table of means or effects
	model.tables(aov(heightdev~carb.o*pres.o*sped.o, data=softdrink))
model.tables(aov(heightdev~carb.o+pres.o+sped.o, data=softdrink), type="mean")
model.tables(aov(heightdev~carb.o+pres.o+sped.o, data=softdrink), type="effect")

# Effect plots
plot.design(heightdev~carb.o+pres.o+sped.o, data=softdrink)
with(softdrink, interaction.plot(carb.o, pres.o, heightdev))

	par(mfrow=c(1,2))
	plot(fitted(sd.lm), residuals(sd.lm), xlab="Fitted", ylab="Residuals")
	qqnorm(residuals(sd.lm))


## keep only two densities of carbonation precentage
sds <- subset(softdrink, carbonation!=14)		# remove carbonation=14 data
sds.lm <- lm(heightdev ~ carb.o * pres.o* sped.o, sds)
anova(sds.lm);   summary(sds.lm)

# Table of effect / main-effect plot
model.tables(aov(heightdev~carb.o*pres.o*sped.o, data=sds), type="effect")
	model.matrix(sds.lm)
plot.design(heightdev~carb.o+pres.o+sped.o, data=sds)

# Remove insignificant interactions
sds.lm1 <- lm(heightdev ~ carb.o * pres.o + sped.o, sds)
anova(sds.lm1);   summary(sds.lm1)

# Linear model with numerical predictor values
sds.lm.n <- lm(heightdev ~ carbonation * pressure * speed, sds)
anova(sds.lm.n);	summary(sds.lm.n)

sds.lm1.n <- lm(heightdev  ~carbonation * pressure + speed, sds)
anova(sds.lm1.n);	summary(sds.lm1.n)

sds.lm2.n <- lm(heightdev ~ carbonation + pressure + speed, sds)
anova(sds.lm2.n);	summary(sds.lm2.n)


