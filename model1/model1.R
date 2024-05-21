### Neccessarry library
library(car)
### Impor
data <- read.csv("University HW\\109-1 Business Analytics\\project\\資料集_ver1\\NTU_COOP_DCB_TRAINt data.csv",header=TRUE, sep='|')
attach(data)



### data cleaning for model 1
# add PAY
PAY <- N3_PAID_MONEY_PER_MONTH - L3MN_AVG_BILL_EXCL_PNLTY_AMT
data <- cbind(data, PAY) 
# eliminate Google relatives
data <- data[,-c(96:206)]
# remove PAY == NA
data <- subset(data,PAY!="NA")
# replace all left NA to 0
for(i in c(1:177)){ 
  na.rows <- is.na(data[, i])
  data[na.rows,i] <- 0
}


## checking unvariated variables
summary(data)



### Building model (method 1)

## Original model: 
# 1. all interesting variables in data_model1, but only focus on 3 month. 
# 2. remove linear dependent variables(N3_PAID_MONEY_PER_MONTH, L3MN_AVG_BILL_EXCL_PNLTY_AMT, L3MN_AVG_BILL_EXCL_PNLTY_RNG) 
#    and only one value variables(all 0, all N or all Y).
# 3. remove google relatives variables.
# 4. remove: BILL_CITY, STAR_SIGNS
 
full_model <- lm(PAY ~ FLAG+FT_CUST_TRIAL+CUR_AGE+TNR+GNDR+MAIN_MSISDN+NP_OUT_GRP+
                   
                   N3_PAID_CHANNEL_CATE+N3_PAID_CNT_PER_MONTH+N3_OVER_PAY_FLAG+
                   
                   N3_INTERNATION_CALL_FLAG+N3_VAS_FEE_PER_MON+N3_MSG_FEE_PER_MON+N3_INTER_MSG_FEE_PER_MON+
                   N3_VAS_APPLY_FLAG+N3_INTERNATIONAL_MSG_FLAG+N3_MSG_FLAG+
                   
                   USE_STAT_NM+CNTRB_LVL+HANDSET+PROJ_USE_HANDSET+ACTV_CLASS_NM+PREPAY_INDIC+CON_MONTHLY_FEE+FEW_CALL_INDIC+
                   NO_CALL_INDIC+BILL_AMT+BILL_EXCL_PNLTY_AMT+BILL_PRE_CHRG_AMT+
                   
                   RATE_PLAN_RNG+MONTHLY_RATE_S+MONTHLY_RATE_I+MONTHLY_RATE_V+TOTAL_MONTHLY_RATE+MONTHLY_FEE_S+MONTHLY_FEE_I+
                   MONTHLY_FEE_V+TOTAL_MONTHLY_FEE+VOICE_UNLIM+
                   
                   DCB_CHANEL+CURRENT_LEVEL+L3_PSUSPEND_FLAG+L3_SUSPEND_FLAG+AUTO_TRANS_FLAG+
                   
                   L3_AVG_LEVEL_DOWN_CNT+L3_MODI_MAX_LEVEL+L3_AVG_MODI_CNT+L3_MODI_FLAG+
                   L3_AVG_LEVEL_UP_CNT+L3_MAX_LEVEL+L3_MAX_LEVEL_MONTH_DIFF+L3_AVG_USED_QUOTA+L3_SERV_FLAG+
                   
                   N3_MONTH_PRCH_CNT_PER_MONTH+N3_MONTH_PRCH_AMOUNT_PER_MONTH+LAST_MONTH_PRCH_CNT+LAST_MONTH_PRCH_AMOUNT+
                   LASTEST_ACC_HR_TO_BUY+LATEST_BUY_PRICE_PER_QUOTA+N3_BUY_AMT_PER_QUOTA+N3_FAIL_AMT_PER_QUOTA+
                   LATEST_FAIL_PRICE_PER_QUOTA+RECENT_PRICE_OVER_L3_AVG_PRICE_PER_DIST_PEOP_FLAG+
                   L3_AVG_AMT_NITILE_OVER_50_FLAG+L3_AVG_AMT_NITILE_OVER_30_FLAG+L3_AVG_AMT_NITILE_OVER_10_FLAG+	
                   N3_FAIL_NORM_TIME_CNT_PER_MONTH+N3_NORM_TIME_AMOUNT_PER_MONTH+ 
                   N3_FAIL_NORM_TIME_AMOUNT_PER_MONTH+N3_NORM_TIME_CNT_PER_MONTH+N3_REFUND_NORM_TIME_CNT_PER_MONTH+
                   N3_REFUND_NORM_TIME_AMOUNT_PER_MONTH+
                   N3_FAIL_PEAK_TIME_CNT_PER_MONTH+N3_PEAK_TIME_CNT_PER_MONTH+N3_FAIL_PEAK_TIME_AMOUNT_PER_MONTH+
                   N3_PEAK_TIME_AMOUNT_PER_MONTH+N3_REFUND_PEAK_TIME_CNT_PER_MONTH+N3_REFUND_PEAK_TIME_AMOUNT_PER_MONTH+
                   N3_PEAK_TIME_NTILE_10_AMT_PER_MONTH_FLAG+N3_PEAK_TIME_NTILE_30_AMT_PER_MONTH_FLAG+
                   N3_PEAK_TIME_NTILE_50_AMT_PER_MONTH_FLAG+N3_PEAK_TIME_REFUND_NTILE_10_AMT_PER_MONTH_FLAG+
                   N3_PEAK_TIME_REFUND_NTILE_30_AMT_PER_MONTH_FLAG+N3_PEAK_TIME_REFUND_NTILE_50_AMT_PER_MONTH_FLAG
                   , data)

summary(full_model)


# change the level of some variables, factorize variable that is categorical
data$HANDSET <- factor(data$HANDSET)
data$PROJ_USE_HANDSET <- factor(data$PROJ_USE_HANDSET)
data$ACTV_CLASS_NM <- factor(data$ACTV_CLASS_NM)
data$PREPAY_INDIC <- factor(data$PREPAY_INDIC)
data$PREPAY_INDIC <- relevel(data$PREPAY_INDIC, ref = "N")
data$VOICE_UNLIM <- factor(data$VOICE_UNLIM)
data$VOICE_UNLIM <- relevel(data$VOICE_UNLIM, ref = "網內語音計費")
data$RECENT_PRICE_OVER_L3_AVG_PRICE_PER_DIST_PEOP_FLAG <- factor(data$RECENT_PRICE_OVER_L3_AVG_PRICE_PER_DIST_PEOP_FLAG)
data$RECENT_PRICE_OVER_L3_AVG_PRICE_PER_DIST_PEOP_FLAG <- relevel(data$RECENT_PRICE_OVER_L3_AVG_PRICE_PER_DIST_PEOP_FLAG, ref = "N")
data$L3_AVG_AMT_NITILE_OVER_50_FLAG <- factor(data$L3_AVG_AMT_NITILE_OVER_50_FLAG)
data$L3_AVG_AMT_NITILE_OVER_30_FLAG <- factor(data$L3_AVG_AMT_NITILE_OVER_30_FLAG)
data$L3_AVG_AMT_NITILE_OVER_10_FLAG <- factor(data$L3_AVG_AMT_NITILE_OVER_10_FLAG)
data$L3_AVG_AMT_NITILE_OVER_50_FLAG <- relevel(data$L3_AVG_AMT_NITILE_OVER_50_FLAG, ref = "N")
data$L3_AVG_AMT_NITILE_OVER_30_FLAG <- relevel(data$L3_AVG_AMT_NITILE_OVER_30_FLAG, ref = "N")
data$L3_AVG_AMT_NITILE_OVER_10_FLAG <- relevel(data$L3_AVG_AMT_NITILE_OVER_10_FLAG, ref = "N")
data$N3_PEAK_TIME_NTILE_10_AMT_PER_MONTH_FLAG <- factor(data$N3_PEAK_TIME_NTILE_10_AMT_PER_MONTH_FLAG)
data$N3_PEAK_TIME_NTILE_30_AMT_PER_MONTH_FLAG <- factor(data$N3_PEAK_TIME_NTILE_30_AMT_PER_MONTH_FLAG)
data$N3_PEAK_TIME_NTILE_50_AMT_PER_MONTH_FLAG <- factor(data$N3_PEAK_TIME_NTILE_50_AMT_PER_MONTH_FLAG)
data$N3_PEAK_TIME_NTILE_10_AMT_PER_MONTH_FLAG <- relevel(data$N3_PEAK_TIME_NTILE_10_AMT_PER_MONTH_FLAG, ref = "N")
data$N3_PEAK_TIME_NTILE_30_AMT_PER_MONTH_FLAG <- relevel(data$N3_PEAK_TIME_NTILE_30_AMT_PER_MONTH_FLAG, ref = "N")
data$N3_PEAK_TIME_NTILE_50_AMT_PER_MONTH_FLAG <- relevel(data$N3_PEAK_TIME_NTILE_50_AMT_PER_MONTH_FLAG, ref = "N")
data$N3_PEAK_TIME_REFUND_NTILE_10_AMT_PER_MONTH_FLAG <- factor(data$N3_PEAK_TIME_REFUND_NTILE_10_AMT_PER_MONTH_FLAG)
data$N3_PEAK_TIME_REFUND_NTILE_30_AMT_PER_MONTH_FLAG <- factor(data$N3_PEAK_TIME_REFUND_NTILE_30_AMT_PER_MONTH_FLAG)
data$N3_PEAK_TIME_REFUND_NTILE_50_AMT_PER_MONTH_FLAG <- factor(data$N3_PEAK_TIME_REFUND_NTILE_50_AMT_PER_MONTH_FLAG)
data$N3_PEAK_TIME_REFUND_NTILE_10_AMT_PER_MONTH_FLAG <- relevel(data$N3_PEAK_TIME_REFUND_NTILE_10_AMT_PER_MONTH_FLAG, ref = "N")
data$N3_PEAK_TIME_REFUND_NTILE_30_AMT_PER_MONTH_FLAG <- relevel(data$N3_PEAK_TIME_REFUND_NTILE_30_AMT_PER_MONTH_FLAG, ref = "N")
data$N3_PEAK_TIME_REFUND_NTILE_50_AMT_PER_MONTH_FLAG <- relevel(data$N3_PEAK_TIME_REFUND_NTILE_50_AMT_PER_MONTH_FLAG, ref = "N")

# rebuild the full model


# checking assumptions
plot(full_model) # residual is skewed distributed



## step function
step(full_model)
## reduced model
reduced_model1 <- lm(formula = PAY ~ FLAG + FT_CUST_TRIAL + CUR_AGE + TNR + GNDR + 
                       MAIN_MSISDN + NP_OUT_GRP + N3_PAID_CHANNEL_CATE + N3_PAID_CNT_PER_MONTH + 
                       N3_OVER_PAY_FLAG + N3_INTERNATION_CALL_FLAG + N3_VAS_FEE_PER_MON + 
                       N3_MSG_FEE_PER_MON + N3_VAS_APPLY_FLAG + N3_INTERNATIONAL_MSG_FLAG + 
                       N3_MSG_FLAG + USE_STAT_NM + CNTRB_LVL + HANDSET + PROJ_USE_HANDSET + 
                       ACTV_CLASS_NM + PREPAY_INDIC + FEW_CALL_INDIC + NO_CALL_INDIC + 
                       BILL_AMT + BILL_EXCL_PNLTY_AMT + BILL_PRE_CHRG_AMT + RATE_PLAN_RNG + 
                       MONTHLY_RATE_V + TOTAL_MONTHLY_RATE + MONTHLY_FEE_V + TOTAL_MONTHLY_FEE + 
                       VOICE_UNLIM + CURRENT_LEVEL + L3_PSUSPEND_FLAG + L3_SUSPEND_FLAG + 
                       AUTO_TRANS_FLAG + L3_AVG_LEVEL_DOWN_CNT + L3_MODI_MAX_LEVEL + 
                       L3_AVG_MODI_CNT + L3_MODI_FLAG + L3_AVG_LEVEL_UP_CNT + L3_MAX_LEVEL + 
                       L3_MAX_LEVEL_MONTH_DIFF + L3_AVG_USED_QUOTA + L3_SERV_FLAG + 
                       N3_MONTH_PRCH_CNT_PER_MONTH + N3_MONTH_PRCH_AMOUNT_PER_MONTH + 
                       LAST_MONTH_PRCH_AMOUNT + N3_BUY_AMT_PER_QUOTA + LATEST_FAIL_PRICE_PER_QUOTA + 
                       L3_AVG_AMT_NITILE_OVER_50_FLAG + L3_AVG_AMT_NITILE_OVER_30_FLAG + 
                       L3_AVG_AMT_NITILE_OVER_10_FLAG + N3_FAIL_NORM_TIME_CNT_PER_MONTH + 
                       N3_NORM_TIME_AMOUNT_PER_MONTH + N3_FAIL_NORM_TIME_AMOUNT_PER_MONTH + 
                       N3_REFUND_NORM_TIME_CNT_PER_MONTH + N3_FAIL_PEAK_TIME_CNT_PER_MONTH + 
                       N3_FAIL_PEAK_TIME_AMOUNT_PER_MONTH + N3_PEAK_TIME_AMOUNT_PER_MONTH + 
                       N3_PEAK_TIME_NTILE_10_AMT_PER_MONTH_FLAG + N3_PEAK_TIME_NTILE_50_AMT_PER_MONTH_FLAG, 
                       data)

summary(reduced_model1)
anova(reduced_model1, full_model)

# appear lots of NA, means that still many linear dependent variables
vif(reduced_model1)
# remove variables: PREPAY_INDIC, VOICE_UNLIM, L3_AVG_AMT_NITILE_OVER_30_FLAG, L3_AVG_AMT_NITILE_OVER_10_FLAG,
#                   N3_PEAK_TIME_NTILE_50_AMT_PER_MONTH_FLAG


reduced_model2 <- lm(formula = PAY ~ FLAG + FT_CUST_TRIAL + CUR_AGE + TNR + GNDR + 
                       MAIN_MSISDN + NP_OUT_GRP + N3_PAID_CHANNEL_CATE + N3_PAID_CNT_PER_MONTH + 
                       N3_OVER_PAY_FLAG + N3_INTERNATION_CALL_FLAG + N3_VAS_FEE_PER_MON + 
                       N3_MSG_FEE_PER_MON + N3_VAS_APPLY_FLAG + N3_INTERNATIONAL_MSG_FLAG + 
                       N3_MSG_FLAG + USE_STAT_NM + CNTRB_LVL + HANDSET + PROJ_USE_HANDSET + 
                       ACTV_CLASS_NM + FEW_CALL_INDIC + NO_CALL_INDIC + 
                       BILL_AMT + BILL_EXCL_PNLTY_AMT + BILL_PRE_CHRG_AMT + RATE_PLAN_RNG + 
                       MONTHLY_RATE_V + TOTAL_MONTHLY_RATE + MONTHLY_FEE_V + TOTAL_MONTHLY_FEE + 
                       CURRENT_LEVEL + L3_PSUSPEND_FLAG + L3_SUSPEND_FLAG + 
                       AUTO_TRANS_FLAG + L3_AVG_LEVEL_DOWN_CNT + L3_MODI_MAX_LEVEL + 
                       L3_AVG_MODI_CNT + L3_MODI_FLAG + L3_AVG_LEVEL_UP_CNT + L3_MAX_LEVEL + 
                       L3_MAX_LEVEL_MONTH_DIFF + L3_AVG_USED_QUOTA + L3_SERV_FLAG + 
                       N3_MONTH_PRCH_CNT_PER_MONTH + N3_MONTH_PRCH_AMOUNT_PER_MONTH + 
                       LAST_MONTH_PRCH_AMOUNT + N3_BUY_AMT_PER_QUOTA + LATEST_FAIL_PRICE_PER_QUOTA + 
                       L3_AVG_AMT_NITILE_OVER_50_FLAG + N3_FAIL_NORM_TIME_CNT_PER_MONTH + 
                       N3_NORM_TIME_AMOUNT_PER_MONTH + N3_FAIL_NORM_TIME_AMOUNT_PER_MONTH + 
                       N3_REFUND_NORM_TIME_CNT_PER_MONTH + N3_FAIL_PEAK_TIME_CNT_PER_MONTH + 
                       N3_FAIL_PEAK_TIME_AMOUNT_PER_MONTH + N3_PEAK_TIME_AMOUNT_PER_MONTH + 
                       N3_PEAK_TIME_NTILE_10_AMT_PER_MONTH_FLAG, data)

summary(reduced_model2)
anova(reduced_model2, reduced_model1)


## variables selection & enhance the model
# 1. some variables with Y/N have no value (L3_AVG_AMT_NITILE_OVER_50_FLAG, N3_PEAK_TIME_NTILE_10_AMT_PER_MONTH_FLAG)
# 2. some variables are not significant (FT_CUST_TRIAL, N3_MSG_FLAG, TOTAL_MONTHLY_RATE 
#                       N3_FAIL_NORM_TIME_AMOUNT_PER_MONTH, N3_REFUND_NORM_TIME_CNT_PER_MONTH) (remove)

reduced_model3 <- lm(formula = PAY ~ FLAG + CUR_AGE + TNR + GNDR + 
                       MAIN_MSISDN + NP_OUT_GRP + N3_PAID_CHANNEL_CATE + N3_PAID_CNT_PER_MONTH + 
                       N3_OVER_PAY_FLAG + N3_INTERNATION_CALL_FLAG + N3_VAS_FEE_PER_MON + 
                       N3_MSG_FEE_PER_MON + N3_VAS_APPLY_FLAG + N3_INTERNATIONAL_MSG_FLAG + 
                       USE_STAT_NM + CNTRB_LVL + HANDSET + PROJ_USE_HANDSET + 
                       ACTV_CLASS_NM + FEW_CALL_INDIC + NO_CALL_INDIC + 
                       BILL_AMT + BILL_EXCL_PNLTY_AMT + BILL_PRE_CHRG_AMT + RATE_PLAN_RNG + 
                       MONTHLY_RATE_V + TOTAL_MONTHLY_RATE + MONTHLY_FEE_V + 
                       CURRENT_LEVEL + L3_PSUSPEND_FLAG + L3_SUSPEND_FLAG + 
                       AUTO_TRANS_FLAG + L3_AVG_LEVEL_DOWN_CNT + L3_MODI_MAX_LEVEL + 
                       L3_AVG_MODI_CNT + L3_MODI_FLAG + L3_AVG_LEVEL_UP_CNT + L3_MAX_LEVEL + 
                       L3_MAX_LEVEL_MONTH_DIFF + L3_AVG_USED_QUOTA + L3_SERV_FLAG + 
                       N3_MONTH_PRCH_CNT_PER_MONTH + N3_MONTH_PRCH_AMOUNT_PER_MONTH + 
                       LAST_MONTH_PRCH_AMOUNT + N3_BUY_AMT_PER_QUOTA + LATEST_FAIL_PRICE_PER_QUOTA + 
                       L3_AVG_AMT_NITILE_OVER_50_FLAG + N3_FAIL_NORM_TIME_CNT_PER_MONTH + 
                       N3_FAIL_NORM_TIME_AMOUNT_PER_MONTH + N3_FAIL_PEAK_TIME_CNT_PER_MONTH + 
                       N3_FAIL_PEAK_TIME_AMOUNT_PER_MONTH + N3_PEAK_TIME_AMOUNT_PER_MONTH + 
                       N3_PEAK_TIME_NTILE_10_AMT_PER_MONTH_FLAG, data)

summary(reduced_model3)
anova(reduced_model3, reduced_model2)

## step function again
step(reduced_model3)

reduced_model4 <- lm(formula = PAY ~ FLAG + CUR_AGE + TNR + GNDR + MAIN_MSISDN + 
                       NP_OUT_GRP + N3_PAID_CHANNEL_CATE + N3_PAID_CNT_PER_MONTH + 
                       N3_OVER_PAY_FLAG + N3_INTERNATION_CALL_FLAG + N3_VAS_FEE_PER_MON + 
                       N3_MSG_FEE_PER_MON + N3_VAS_APPLY_FLAG + N3_INTERNATIONAL_MSG_FLAG + 
                       USE_STAT_NM + CNTRB_LVL + HANDSET + PROJ_USE_HANDSET + ACTV_CLASS_NM + 
                       FEW_CALL_INDIC + NO_CALL_INDIC + BILL_AMT + BILL_EXCL_PNLTY_AMT + 
                       BILL_PRE_CHRG_AMT + RATE_PLAN_RNG + TOTAL_MONTHLY_RATE + 
                       CURRENT_LEVEL + L3_PSUSPEND_FLAG + L3_SUSPEND_FLAG + AUTO_TRANS_FLAG + 
                       L3_AVG_LEVEL_DOWN_CNT + L3_MODI_MAX_LEVEL + L3_AVG_MODI_CNT + 
                       L3_MODI_FLAG + L3_AVG_LEVEL_UP_CNT + L3_MAX_LEVEL + L3_MAX_LEVEL_MONTH_DIFF + 
                       L3_AVG_USED_QUOTA + L3_SERV_FLAG + N3_MONTH_PRCH_CNT_PER_MONTH + 
                       N3_MONTH_PRCH_AMOUNT_PER_MONTH + LAST_MONTH_PRCH_AMOUNT + 
                       N3_BUY_AMT_PER_QUOTA + LATEST_FAIL_PRICE_PER_QUOTA + L3_AVG_AMT_NITILE_OVER_50_FLAG + 
                       N3_FAIL_NORM_TIME_CNT_PER_MONTH + N3_FAIL_NORM_TIME_AMOUNT_PER_MONTH + 
                       N3_FAIL_PEAK_TIME_CNT_PER_MONTH + N3_FAIL_PEAK_TIME_AMOUNT_PER_MONTH + 
                       N3_PEAK_TIME_AMOUNT_PER_MONTH + N3_PEAK_TIME_NTILE_10_AMT_PER_MONTH_FLAG, data)

summary(reduced_model4)


# checking outlier & leverage
res1 <- rstandard(reduced_model4)	# studentized residuals
hist(res1)

lev1 <- hatvalues(reduced_model4)
plot(lev1, xlab="ID Number", ylab="Leverage") 
lev1[lev1>0.3]




plot(reduced_model4)








### Building model (method 2)
## apply original model, focus on 3 month
model_PAY <- lm(PAY ~ CURRENT_LEVEL+N3_OVER_PAY_FLAG+N3_BUY_AMT_PER_QUOTA+LAST_MONTH_PRCH_CNT+
                  LAST_MONTH_PRCH_AMOUNT+N3_VAS_APPLY_FLAG, data_model1)
summary(model_PAY)
plot(PAY, CURRENT_LEVEL)

# checking outlier & leverage
res1 <- rstandard(model_PAY)	# studentized residuals
hist(res1, breaks=20)

lev1 <- hatvalues(model_PAY)
plot(lev1, xlab="ID Number", ylab="Leverage") 



model_PAY_test <- lm(PAY ~ L3_MODI_FLAG+L3_AVG_USED_QUOTA+N3_MONTH_PRCH_CNT_PER_MONTH+
                       N3_MONTH_PRCH_AMOUNT_PER_MONTH+USE_STAT_NM+CNTRB_LVL+TOTAL_MONTHLY_RATE+
                       TOTAL_MONTHLY_FEE+FT_CUST_TRIAL+MAIN_MSISDN+TNR+GNDR+CUR_AGE+CURRENT_LEVEL+
                       N3_OVER_PAY_FLAG+N3_BUY_AMT_PER_QUOTA+LAST_MONTH_PRCH_CNT+LAST_MONTH_PRCH_AMOUNT+
                       N3_VAS_APPLY_FLAG)
summary(model_PAY_test)
step(model_PAY_test)


model_PAY_test_adjust <- lm(PAY ~ HANDSET + L3_MODI_FLAG + L3_AVG_USED_QUOTA + N3_MONTH_PRCH_AMOUNT_PER_MONTH + 
                              CNTRB_LVL + TOTAL_MONTHLY_RATE + TOTAL_MONTHLY_FEE + 
                              FT_CUST_TRIAL + MAIN_MSISDN + TNR + CUR_AGE + N3_OVER_PAY_FLAG + 
                              N3_BUY_AMT_PER_QUOTA + LAST_MONTH_PRCH_CNT + LAST_MONTH_PRCH_AMOUNT + 
                              N3_VAS_APPLY_FLAG)
summary(model_PAY_test_adjust)
plot(model_PAY_test_adjust)

