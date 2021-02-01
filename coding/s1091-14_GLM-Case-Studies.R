#####-------------------------------------------------------
##### Business Analytics 
##### Dr. Jiun-Yu Yu
##### Department of Business Administration
##### National Taiwan University
##### Lecture 14, 24 Dec 2019
#####-------------------------------------------------------

#### Example: SUV ------------------------------------------------------------
suv.dat <- read.csv('d1091-14_suv.csv')
names(suv.dat)
table(suv.dat[,c(1,4)])

chisq.test(table(suv.dat[,c(1,4)]))

attach(suv.dat)
summary(suv.dat)
ind <- vehicle.age + 6*passengers + 100*(make=='Ford')
tab <- t(table(cause,ind))
tab <- as.data.frame(cbind(tab[,1],tab[,2]), rownames=row.names(tab))
id <- as.numeric(row.names(tab))
tab$make <- id %/% 100	# quotient
id <- id %% 100		# remainder
tab$passengers <- id %/% 6
tab$vehicle.age <- id %% 6
names(tab)[1:2] <- c('Other','Tyre')
detach()
tab				# data ready for regression analysis

	options(contrasts=c('contr.treatment','contr.poly'))

suv.lg <- glm(cbind(Tyre,Other) ~ vehicle.age + passengers + make, 
	data=tab, family=binomial)
summary(suv.lg, cor=F)	# you can try 'cor=T' to see the difference
exp(coef(suv.lg)[4])

suv.lg.q <- glm(cbind(Tyre,Other)~ vehicle.age + passengers + make + I(vehicle.age^2), 
	data=tab, family=binomial)
summary(suv.lg.q, cor=F)

anova(suv.lg, suv.lg.q, test="Chisq")	# p-value = 0.04188 < 0.05


suv.lg.qi <- glm(cbind(Tyre,Other)~ vehicle.age + passengers * make + I(vehicle.age^2), 
	data=tab, family=binomial)
summary(suv.lg.qi, cor=F)

anova(suv.lg.q, suv.lg.qi, test="Chisq")	# p-value = 0.0219 < 0.05


####
# consider passengers and vehicle.age as factors --> group further
tab.gr <- tab
tab.gr$vehicle.age[tab$vehicle.age<1] <- 1
tab.gr$vehicle.age[tab$vehicle.age>3] <- '4+'
tab.gr$passengers[tab$passengers>3] <- '4+'

suv.lg.f <- glm(cbind(Tyre,Other) ~ factor(vehicle.age) + factor(passengers) + make, 
	data=tab.gr, family=binomial)
summary(suv.lg.f, cor=F)

	# par(mfrow=c(2,2));   plot(fit.f)


library(MASS)
dropterm(suv.lg.f, test='Chisq')

	# All the terms are significant, and the fit is good, unlike:

	suv.lg.m <- glm(cbind(Tyre, Other) ~ make, data=tab, family=binomial)
	summary(suv.lg.m)



#### Example: Copenhagen Housing Satisfaction --------------------------------
library(MASS)
housing
summary(housing)
class(housing$Sat)

## Slide p. 14-10
xtabs(Freq~Type+Infl+Cont+Sat, data=housing)
ftable(xtabs(Freq~Type+Infl+Cont+Sat, data=housing), row.vars=1:2)
#	ftable(xtabs(Freq~Type+Infl+Cont+Sat, data=housing), row.vars=c(1,3))


## Mosaic Plot
mosaicplot(xtabs(Freq~Type+Infl+Cont+Sat, data=housing), color=TRUE)

# See the difference! Mosaic Plot: Layout as Table 14.1:
#	mosaicplot(xtabs(Freq~Cont+Type+Sat+Infl, data=housing), color=TRUE)


## Poisson log-linear model fitting~
house.glm0 <- glm(Freq~Infl*Type*Cont+Sat, family = poisson, data = housing)
summary(house.glm0)

addterm(house.glm0, ~. + Sat:(Infl+Type+Cont), test = 'Chisq')

house.glm1 <- update(house.glm0, .~. + Sat:(Infl+Type+Cont))
summary(house.glm1, cor = F)

dropterm(house.glm1, test = 'Chisq')	
	# No term to be dropped...

addterm(house.glm1, ~. + Sat:(Infl+Type+Cont)^2, test = 'Chisq')
	# No term to be added...

## Probability Estimation
hnames <- lapply(housing[,-5], levels) # omit Freq
house.pm <- predict(house.glm1, expand.grid(hnames), type='response') # poisson means
house.pm <- matrix(house.pm, ncol=3, byrow=T, dimnames=list(NULL, hnames[[1]]))
house.pr <- house.pm/apply(house.pm, 1, sum)
cbind(expand.grid(hnames[-1]), prob = round(house.pr, 2))

house.fp <- cbind(expand.grid(hnames[c(2:4,1)]), Prob=as.vector(round(house.pr, 2)))

ftable(xtabs(Prob~Type+Infl+Cont+Sat, data=house.fp), row.vars=1:2)

   mosaicplot(xtabs(Prob~Type+Infl+Cont+Sat, data=house.fp), color=TRUE, 
		  main="Fitted probabilities")

anova(house.glm1)

	# par(mfrow=c(2,2));   plot(house.glm1)



