data <- read.csv("University HW\\109-1 Business Analytics\\project\\model3\\Model3.csv", header = TRUE, fileEncoding = "big5")
attach(data)

library(car)

## data cleaning for model 3

# remove PAY == NA
data <- subset(data,PAY!="NA")
# replace all left NA to 0
for(i in c(30:288)){ 
  na.rows <- is.na(data[, i])
  data[na.rows,i] <- 0
}

# relevel to ref = "N"
data$G_N3_BUY_TOP5_CNT_OR_NOT <- as.factor(data$G_N3_BUY_TOP5_CNT_OR_NOT)
data$G_N3_BUY_TOP5_CNT_OR_NOT <- relevel(data$G_N3_BUY_TOP5_CNT_OR_NOT, ref="N")
data$G_N3_BUY_GAME_APP_OR_NOT <- as.factor(data$G_N3_BUY_GAME_APP_OR_NOT)
data$G_N3_BUY_GAME_APP_OR_NOT <- relevel(data$G_N3_BUY_GAME_APP_OR_NOT, ref="N")
data$G_N3_BUY_DATE_APP_OR_NOT <- as.factor(data$G_N3_BUY_DATE_APP_OR_NOT)
data$G_N3_BUY_DATE_APP_OR_NOT <- relevel(data$G_N3_BUY_DATE_APP_OR_NOT, ref="N")
data$G_N3_BUY_BUTTOM50_DCON_OR_NOT <- as.factor(data$G_N3_BUY_BUTTOM50_DCON_OR_NOT)
data$G_N3_BUY_BUTTOM50_DCON_OR_NOT <- relevel(data$G_N3_BUY_BUTTOM50_DCON_OR_NOT, ref="N")
data$G_N3_BUY_BUTTOM50_CNT_OR_NOT <- as.factor(data$G_N3_BUY_BUTTOM50_CNT_OR_NOT)
data$G_N3_BUY_BUTTOM50_CNT_OR_NOT <- relevel(data$G_N3_BUY_BUTTOM50_CNT_OR_NOT, ref="N")
data$G_N3_BUY_BUTTOM50_AVG_PRICE_OR_NOT <- as.factor(data$G_N3_BUY_BUTTOM50_AVG_PRICE_OR_NOT)
data$G_N3_BUY_BUTTOM50_AVG_PRICE_OR_NOT <- relevel(data$G_N3_BUY_BUTTOM50_AVG_PRICE_OR_NOT, ref="N")
data$G_N3_BUY_TOP5_DCON_OR_NOT <- as.factor(data$G_N3_BUY_TOP5_DCON_OR_NOT)
data$G_N3_BUY_TOP5_DCON_OR_NOT <- relevel(data$G_N3_BUY_TOP5_DCON_OR_NOT, ref="N")
data$G_N3_BUY_TOP5_AVG_PRICE_OR_NOT <- as.factor(data$G_N3_BUY_TOP5_AVG_PRICE_OR_NOT)
data$G_N3_BUY_TOP5_AVG_PRICE_OR_NOT <- relevel(data$G_N3_BUY_TOP5_AVG_PRICE_OR_NOT, ref="N")
data$N3_PEAK_TIME_NTILE_10_AMT_PER_MONTH_FLAG <- as.factor(data$N3_PEAK_TIME_NTILE_10_AMT_PER_MONTH_FLAG)
data$N3_PEAK_TIME_NTILE_10_AMT_PER_MONTH_FLAG <- relevel(data$N3_PEAK_TIME_NTILE_10_AMT_PER_MONTH_FLAG, ref="N")
data$L3_AVG_AMT_NITILE_OVER_50_FLAG <- as.factor(data$L3_AVG_AMT_NITILE_OVER_50_FLAG)
data$L3_AVG_AMT_NITILE_OVER_50_FLAG <- relevel(data$L3_AVG_AMT_NITILE_OVER_50_FLAG, ref="N")

data$PROJ_USE_HANDSET <- as.factor(data$PROJ_USE_HANDSET)
data$HANDSET <- as.factor(data$HANDSET)
data$HANDSET <- relevel(data$HANDSET, ref="2")

# use the variables from model 1 (linear model fitted before)
fit_glm1 <- glm(M ~ FLAG + CUR_AGE + TNR + GNDR + MAIN_MSISDN + 
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
                  N3_PEAK_TIME_AMOUNT_PER_MONTH + N3_PEAK_TIME_NTILE_10_AMT_PER_MONTH_FLAG + N3_NORM_TIME_CNT_PER_MONTH +
                  + G_N3_BUY_BUTTOM50_CNT_CNT + G_N3_BUY_TOP5_CNT_CNT
                  + G_N3_BUY_BUTTOM50_CNT_OR_NOT + G_N3_BUY_TOP5_CNT_OR_NOT
                  + G_N3_BUY_GAME_APP_OR_NOT + G_N3_BUY_DATE_APP_OR_NOT
                  + G_N3_BUY_BUTTOM50_CNT_AVG_AMT + G_N3_BUY_TOP5_CNT_AVG_AMT
                  + G_N3_BUY_BUTTOM50_DCON_OR_NOT + G_N3_BUY_BUTTOM50_AVG_PRICE_OR_NOT
                  + G_N3_BUY_TOP5_DCON_OR_NOT + G_N3_BUY_TOP5_AVG_PRICE_OR_NOT
                  , binomial, data)

summary(fit_glm1)
anova(fit_glm1)

# picking the variables that good at dropping in deviance 
fit_glm2 <- glm(M ~ TNR + N3_PAID_CHANNEL_CATE + CNTRB_LVL + PROJ_USE_HANDSET
                + BILL_AMT + BILL_EXCL_PNLTY_AMT + BILL_PRE_CHRG_AMT + CURRENT_LEVEL
                + L3_AVG_USED_QUOTA + MAIN_MSISDN
                , binomial, data)
summary(fit_glm2)
anova(fit_glm2)


# consider google
fit_glm_g <- glm(M ~ G_N3_BUY_DATE_APP_AVG_AMT + G_N3_BUY_LIVE_STREAM_AVG_AMT + G_N3_BUY_GAME_APP_AVG_AMT
                 , binomial, data)
summary(fit_glm_g)


fit_glm3 <- glm(M ~ TNR + N3_PAID_CHANNEL_CATE + CNTRB_LVL + PROJ_USE_HANDSET
                + BILL_AMT + BILL_EXCL_PNLTY_AMT + BILL_PRE_CHRG_AMT + CURRENT_LEVEL
                + L3_AVG_USED_QUOTA + MAIN_MSISDN
                + G_N3_BUY_BUTTOM50_CNT_CNT + G_N3_BUY_TOP5_CNT_CNT
                + G_N3_BUY_BUTTOM50_CNT_OR_NOT + G_N3_BUY_TOP5_CNT_OR_NOT
                + G_N3_BUY_GAME_APP_OR_NOT + G_N3_BUY_DATE_APP_OR_NOT
                + G_N3_BUY_BUTTOM50_CNT_AVG_AMT + G_N3_BUY_TOP5_CNT_AVG_AMT
                + G_N3_BUY_BUTTOM50_DCON_OR_NOT + G_N3_BUY_BUTTOM50_AVG_PRICE_OR_NOT
                + G_N3_BUY_TOP5_DCON_OR_NOT + G_N3_BUY_TOP5_AVG_PRICE_OR_NOT
                , binomial, data)
summary(fit_glm3)
anova(fit_glm3)


# change to linear 
fit_lm2 <- lm(PAY ~ TNR + N3_PAID_CHANNEL_CATE + CNTRB_LVL + PROJ_USE_HANDSET
                + BILL_AMT + BILL_EXCL_PNLTY_AMT + BILL_PRE_CHRG_AMT + CURRENT_LEVEL
                + L3_AVG_USED_QUOTA
                + G_N3_BUY_DATE_APP_AVG_AMT + G_N3_BUY_LIVE_STREAM_AVG_AMT + G_N3_BUY_GAME_APP_AVG_AMT, data)
summary(fit_lm2)


vif(fit_glm2)

