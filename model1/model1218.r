data1218 <- read.csv("University HW\\109-1 Business Analytics\\project\\¸ê®Æ¶°_ver1\\model1new.csv", header = TRUE, fileEncoding = "big5")
attach(data1218)

addNA(N3_PEAK_TIME_REFUND_NTILE_50_AMT_PER_MONTH_FLAG)
N3_PEAK_TIME_NTILE_50_AMT_PER_MONTH_FLAG <- as.factor(N3_PEAK_TIME_NTILE_50_AMT_PER_MONTH_FLAG)
N3_PEAK_TIME_NTILE_50_AMT_PER_MONTH_FLAG <- relevel(N3_PEAK_TIME_NTILE_50_AMT_PER_MONTH_FLAG, ref="N")

addNA(L3_AVG_AMT_NITILE_OVER_50_FLAG)
L3_AVG_AMT_NITILE_OVER_50_FLAG <- as.factor(L3_AVG_AMT_NITILE_OVER_50_FLAG)
L3_AVG_AMT_NITILE_OVER_50_FLAG <- relevel(L3_AVG_AMT_NITILE_OVER_50_FLAG, ref="N")

# replace all left NA to 0
for(i in c(1:176)){ 
  na.rows <- is.na(data1218[, i])
  data1218[na.rows,i] <- 0
}

HANDSET <- factor(HANDSET)

model1218 <- lm(PAY ~ CUR_AGE + TNR + MAIN_MSISDN + NP_OUT_GRP + HANDSET
                + N3_PAID_CHANNEL_CATE + N3_VAS_FEE_PER_MON
                + N3_MSG_FEE_PER_MON + RATE_PLAN_RNG 
                + L3_AVG_MODI_CNT + L3_AVG_USED_QUOTA
                + N3_MONTH_PRCH_CNT_PER_MONTH + N3_MONTH_PRCH_AMOUNT_PER_MONTH
                + L3_AVG_AMT_NITILE_OVER_50_FLAG 
                + CNTRB_LVL + TOTAL_MONTHLY_FEE
                + N3_PEAK_TIME_REFUND_NTILE_50_AMT_PER_MONTH_FLAG)
summary(model1218)

plot(model1218)
plot(fitted(model1218), residuals(model1218), xlab="Fitted", ylab="Residuals") ; abline(h=0)
hist(residuals(model1218))
qqnorm(residuals(model1218), ylab="Residuals") ; qqline(residuals(model1218))

