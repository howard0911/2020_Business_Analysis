#####-------------------------------------------------------
##### Business Analytics
##### Dr. Jiun-Yu Yu
##### Department of Business Administration
##### National Taiwan University
##### Lecture 10, 17 Nov 2020
#####-------------------------------------------------------

library(Sleuth2)

###################################
####    Proportion and Odds    ####
###################################

### Case1801 ---------------------------------------- Obesity &Heart Disease
y <- c(22,1179,22,1409)				# listed by row
obese <- gl(2,2,4, c("Yes","No"))		# row names
CVDdeath <- gl(2,1,4, c("Yes","No"))	# col names
	# case1801 <- data.frame(y, obese, CVDdeath)
( case1801 <- xtabs(y ~ obese + CVDdeath) )

	( obese.ptest <- prop.test(case1801) )
	( p.i <- obese.ptest$estimate	)		# p.i1hat & p.i2hat
		obese.ptest$conf.int

	## Inference about Difference between Two Population Proportions 
	diff <- abs(obese.ptest$estimate[2]-obese.ptest$estimate[1])
	ndeath <- sum(case1801[,1])
	n <- sum(case1801)
	pic <- ndeath/n			# 0.007390746

	n.r <- apply(case1801,1,sum)
	noby <- n.r[1]
	nobn <- n.r[2]
 
	se0 <- sqrt(pic*(1-pic)/noby+pic*(1-pic)/nobn)	# test version
	1 - pnorm(diff/se0)					# p-value

	se <- sqrt( sum(p.i*(1-p.i)/n.r) )			# CI version
	diff + qnorm(c(0.025, 0.975))*se			# CI from SE


## Odds Ratio
( obese.odds <- obese.ptest$estimate/(1-obese.ptest$estimate) )
( obese.phi <- obese.odds[1]/obese.odds[2] )
log(obese.phi)				# log odds ratio

( obese.ase <- sqrt(sum(1/case1801)) )		# CI version
( obese.se0 <- sqrt(sum(1/(pic*(1-pic)*n.r))) )	# test version
obese.ase
1-pnorm(log(obese.phi)/obese.se0)			# p-value
obese.ase
log(obese.phi) + qnorm(c(0.025, 0.975))*obese.ase   # CI 


### Case1802 ---------------------------------------- Vitamin C & Common Cold
y <- c(335,76,302,105)				# listed by row
take <- gl(2,2,4, c("Placebo","VC"))	# row names
cold <- gl(2,1,4, c("Yes","No"))		# col names
	# case1802 <- data.frame(y, take, cold)
( case1802 <- xtabs(y ~ take + cold) )

	( vc.ptest <- prop.test(case1802) )
	( p.i <- vc.ptest$estimate )
		vc.ptest$conf.int

	## Inference about Difference between Two Population Proportions 
	diff <- abs(vc.ptest$estimate[2]-vc.ptest$estimate[1])
	ncold <- sum(case1802[,1])
	n <- sum(case1802)
	pic <- ncold/n		# 0.7787286

	nvc.r <- apply(case1802,1,sum)
	npl <- sum(case1802[1,])
	nvc <- sum(case1802[2,])

	se0 <- sqrt(pic*(1-pic)/npl+pic*(1-pic)/nvc)	# test version
	1 - pnorm(diff/se0)					# p-value

	## C.I. for Difference between Two Proportions	# 
	se <- sqrt( sum(p.i*(1-p.i)/nvc.r) )		# CI version
	( ci <- diff + c(-1,1)*qnorm(0.975)*se )		# qnorm(0.975)=1.959964


## Odds Ratio	
( vc.odds <- vc.ptest$estimate/(1-vc.ptest$estimate) )
( vc.phi <- vc.odds[1]/vc.odds[2] )	
log(vc.phi)				# log odds ratio

( vc.ase <- sqrt(sum(1/case1802)) )			# CI version
( vc.se0 <- sqrt(sum(1/(pic*(1-pic)*nvc.r))) )	# test version

## C.I. for Odds Ratio
( vc.logphi.ci <- log(vc.phi)+c(-1,1)*qnorm(0.975)*vc.ase )
exp(vc.logphi.ci)



### Case1803 ---------------------------------------- Smoking & Lung Cancer
y <- c(83,72,3,14)					# listed by row
smoker <- gl(2,2,4, c("Yes","No"))			# row names
patient <- gl(2,1,4, c("Cancer","Control"))	# col names
	# case1803 <- data.frame(y, smoker, patient)
( case1803 <- xtabs(y ~ smoker + patient) )

		lc.ptest <- prop.test(case1803)		# This part is actually
		lc.ptest						# NOT OK...
		p.i <- lc.ptest$estimate			#
			lc.ptest$conf.int				#

	ncancer <- sum(case1803[,1])
	n <- sum(case1803)
	pic <- ncancer/n
	nlc.r <- apply(case1803,1,sum)


## Odds Ratio
( lc.odds <- p.i /(1-p.i) )
( lc.phi <- lc.odds[1]/lc.odds[2])
log(lc.phi)

( lc.ase <- sqrt(sum(1/case1803)) )			# CI version
( lc.se0 <- sqrt(sum(1/(pic*(1-pic)*nlc.r))) )	# test version

## C.I. for Odds Ratio
( lc.logphi.ci <- log(lc.phi)+c(-1,1)*1.96*lc.ase )
exp(lc.logphi.ci)


##################################
####    Contingency Tables    ####
##################################

### Case 1902 ----------------------------------------------
case1902
attach(case1902)
c1902 <- xtabs(cbind(Death, Nodeath)~Victim)

chisq.test(c1902, correct=F)

( Xsq <- chisq.test(c1902, correct=F)$statistic )
pchisq(Xsq, df=1, lower.tail=F)

( pij <- prop.table(xtabs(cbind(Death, Nodeath)~Victim)) )
( mpr.r <- apply(pij,1,sum) )		# marginal probability by row
( mpr.c <- apply(pij,2,sum) )		# marginal probability by column
( mu.hat <- outer(mpr.r, mpr.c)*sum(cbind(Death, Nodeath)) )

( Gsq <- 2*sum(c1902*log(c1902/mu.hat)) )
pchisq(Gsq, df=1, lower.tail=F)




### Case 1901 ----------------------------------------------
case1901 <- matrix(c(21,3,14,10),nrow=2,byrow=TRUE)
dimnames(case1901) <- list(c("Male","Female"),c("Yes","No"))
names(dimnames(case1901)) <- c("Gender","Promoted")
case1901

fisher.test(case1901, alternative="greater")

# change the order of rows 
case1901.1 <- matrix(c(14,10,21,3),nrow=2,byrow=TRUE)
dimnames(case1901.1) <- list(c("Female","Male"),c("Yes","No"))
names(dimnames(case1901.1)) <- c("Gender","Promoted")
case1901.1

fisher.test(case1901.1, alternative="less")	# Note: 'less'

## Case1901 with Mantel Haenszel Test
R1 <- sum(case1901[,1]);	R2 <- sum(case1901[,2])
C1 <- sum(case1901[1,]);	C2 <- sum(case1901[2,])
T <- sum(case1901)
mu11 <- R1*C1/T
( Excess <- case1901[1,1] - mu11 )
( VarEx <- R1*R2*C1*C2/(T*T*(T-1)) )
( zstat <- Excess/sqrt(VarEx) )
1-pnorm(zstat)

# Compare with...
	prop.test(case1901)
	chisq.test(case1901)




	######---------------------------------------------------------------
	# Mantel Haenszel Test, as an alternative
	table1902 <- with(case1902, rbind(Death,Nodeath))
	dim(table1902) <- c(2,2,6)
	table1902
	mantelhaen.test(table1902, alternative="greater", correct=F)




