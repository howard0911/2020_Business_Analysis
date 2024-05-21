library(rfm)
library(magrittr)
library(dplyr) 
library(ggplot2)
data1229 <- read.csv("/Users/yenjohan/Desktop/model3.csv", header = TRUE, fileEncoding = "big5")
attach(data1229)

rfm_table_order()

rfm_result <- rfm_table_order(CONTRACT_ID, CONTRACT_ID, LAST_MONTH_PRCH_CNT/N3_MONTH_PRCH_CNT_PER_MONTH, LAST_MONTH_PRCH_CNT, PAY)

library(ggplot2)
# plot data

data1229$Total_Score <- as.numeric(paste(data1229$R,data1229$F,data1229$R, sep = ""))
data1229$Total_Score
data1229 %>%
  mutate(R = as.factor(R),
         F = as.factor(F),
         M = as.factor(M)) %>%
  group_by(R, F) %>% 
  summarise(value = n()) %>% 
  ungroup() %>% 
  ggplot(aes(R, F)) +
  geom_tile(aes(fill = value), col = "black") + 
  geom_text(aes(label = round(value, 1))) +
  scale_fill_gradient2() +
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(family = "黑體-繁 中黑", size = 14),
        legend.background = element_rect(fill = "gray96",colour = "gray88"),
        plot.background = element_rect(fill = "gray99"),
        plot.title = element_text(hjust = 0.5,size = 16), #標題置中
        strip.background = element_rect(fill = "lightgray", colour = NA)
  )