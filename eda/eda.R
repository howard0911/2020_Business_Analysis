### Import data
data <- read.csv("University HW\\109-1 Business Analytics\\project\\¸ê®Æ¶°_ver1\\NTU_COOP_DCB_TRAIN.csv",header=TRUE, sep='|')
attach(data)


### eda
## capturing monthly data 
data1 <- data[,57:65]
data1 <- cbind(data1, data[,52])
data1 <- cbind(data1, data[,3])
names(data1)[10] <- "RATE_PLAN_RNG"
names(data1)[11] <- "FLAG"
data2 <- data[,13:14]
focus <- cbind(data1, data2)
focus <- cbind(focus, data['CURRENT_LEVEL'])
boxplot(data1[,1:8])

diff <- focus['TOTAL_MONTHLY_FEE']-focus['TOTAL_MONTHLY_RATE']
focus <- cbind(focus, diff)
names(focus)[15] <- "DIFF"
summary(focus)


boxplot(focus[,13])
boxplot(focus$DIFF~focus$FLAG, xlab="FLAG", ylab="DIFF")
boxplot(CURRENT_LEVEL~FLAG)
