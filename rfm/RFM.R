#載入和清洗

rfm <- read.csv("Model_RFM_de.csv", header=TRUE, sep = ",",fileEncoding ="Big5")
head(rfm)

rfm1 <- filter(rfm, rfm$M == 1)
head(rfm1)

rfm2 <- filter(rfm, rfm$M == 2)
head(rfm2)

rfm3 <- filter(rfm, rfm$M == 3)
head(rfm3)


#長條圖

howard_theme <- function(base_size = 12, base_family = "sans"){
    theme_minimal(base_size = base_size, base_family = base_family) +
        theme(
            axis.text.x = element_text(size=10, angle = 65, vjust = 1, hjust=1),
            axis.text.y = element_text(size=10),
            axis.title = element_text(size = 10),
            panel.grid.major = element_line(color = "grey"),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "aliceblue"),
            strip.background = element_rect(fill = "navy", color = "navy", size = 1),
            strip.text = element_text(face = "bold", size = 10, color = "white"),
            legend.position = "right",
            legend.justification = "bottom",
            legend.background = element_blank(),
            legend.text=element_text(size=15),
            panel.border = element_rect(color = "grey", fill = NA, size = 0.05),
            title = element_text(size = 15),
            plot.caption=element_text(size = 10)
        )
}

ggplot(rfm, aes(x=F)) +
    #theme_bw() +
    scale_x_continuous(breaks=c(1:10)) +
    geom_histogram(alpha=0.6, binwidth=1) +
    ggtitle("消費頻率與訂單數量分佈圖")+
    xlab("消費頻率") + 
    ylab("訂單數量") +howard_theme()+
    theme(plot.title = element_text(color="black", size=15),
          axis.title.x = element_text(color="blue", size=10),
          text=element_text(family="黑體-繁 中黑"),
          axis.title.y = element_text(color="#993333", size=10))

ggplot(rfm, aes(x=G_LAST_BUY_DAYS)) +
    theme_bw() +
    geom_histogram(alpha=0.6, binwidth=1) +
    scale_x_continuous(breaks=c(0:91))+
    ggtitle("最近一次（天）的消費與購買量分佈圖")+
    xlab("距離上次購買的天數") + 
    ylab("訂單數量") +#howard_theme()+
    theme(plot.title = element_text(color="black", size=15),
          axis.title.x = element_text(color="blue", size=10),
          axis.title.y = element_text(color="#993333", size=10),
          panel.background = element_rect(fill = "aliceblue"),
          text=element_text(family="黑體-繁 中黑"),
          strip.background = element_rect(fill = "navy", color = "navy", size = 1),
          strip.text = element_text(face = "bold", size = 10, color = "white"))


#新欄位編碼
rfm.segm <- rfm %>%
    mutate(F=ifelse(between(F, 1, 1), '1',
                    ifelse(between(F, 2, 2), '2',
                           ifelse(between(F, 3, 3), '3', '4')))) %>%
    
    
    # 切割近因畫出邊界
    mutate(G_LAST_BUY_DAYS=ifelse(between(G_LAST_BUY_DAYS, 1, 6),  '0-6 天',
                    ifelse(between(G_LAST_BUY_DAYS, 7, 13), '7-13 天',
                           ifelse(between(G_LAST_BUY_DAYS, 14, 20), '14-20 天','>20 天')))) %>%
    
    mutate(cart=paste(ifelse(G_N3_BUY_LIVE_STREAM_CNT!=0, '、直播軟體', ''),
                        ifelse(G_N3_BUY_DATE_APP_CNT!=0, '、交友軟體', ''),
                            ifelse(G_N3_BUY_GAME_APP_CNT!=0, '、遊戲軟體', ''),sep='')) %>%
    
    arrange(CONTRACT_ID)

rfm.segm$cart = str_split_fixed(rfm.segm$cart, '、', 2)[,2]

?str_split_fixed
#all
rfm.segm$F <- factor(rfm.segm$F, levels=c( '4', '3', '2', '1'))
rfm.segm$G_LAST_BUY_DAYS <- factor(rfm.segm$G_LAST_BUY_DAYS, levels=c('>20 天', '14-20 天', '7-13 天', '0-6 天'))


lcg <- rfm.segm %>%
    group_by(G_LAST_BUY_DAYS, F) %>%
    summarise(quantity=n()) %>%
    mutate(client='顧客人數') %>%
    ungroup()
########數字
lcg.matrix= as.data.frame.matrix(table(rfm.segm$F, rfm.segm$G_LAST_BUY_DAYS))
lcg.matrix$F = row.names(lcg.matrix) 
lcg.matrix

lcg.adv <- lcg %>%
    mutate(G_LAST_BUY_DAYS = ifelse(G_LAST_BUY_DAYS %in% c('14-20 天', '>20 天'), "not recent", "recent"),
           F = ifelse(F %in% c('4', '3'), "frequent", "infrequent"),
           customer.type = interaction(G_LAST_BUY_DAYS, F))

ggplot(lcg.adv, aes(x=client, y=quantity, fill=customer.type)) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    geom_rect(aes(fill = customer.type), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.1) +
    facet_grid(F ~ G_LAST_BUY_DAYS) +
    geom_bar(stat='identity', alpha=0.7) +
    ggtitle("RF分析圖_所有用戶") +
    xlab("最近一次消費天數") + ylab("購買頻率")+ 
    theme(plot.title = element_text(color="black", size=15 ),
          axis.title.x = element_text(color="blue", size=10, face="bold"),
          axis.title.y = element_text(color="#993333", size=10, face="bold"),
          text=element_text(family="黑體-繁 中黑"))+
    guides(fill=guide_legend(title="客群顏色指示表"))+
    scale_fill_discrete(name="Experimental\nCondition",breaks = c('not recent.frequent','recent.frequent','not recent.infrequent','recent.infrequent'), labels = c('先前活躍用戶','現活躍用戶','先前嚐鮮用戶','現嚐鮮用戶'))


#性別


as.character(rfm.segm$GNDR)
rfm.segm$GNDR[rfm.segm$GNDR == 0] <- "女"
rfm.segm$GNDR[rfm.segm$GNDR == 1] <- "男"
#標號方式
lcg.sub <- rfm.segm %>%
    group_by(GNDR, G_LAST_BUY_DAYS, F, cart) %>%
    summarise(quantity=n()) %>%
    mutate(client='顧客人數') %>%
    ungroup()

lcg.sub$GNDR = factor(lcg.sub$GNDR, levels = c('女', '男'))
ggplot(lcg.sub, aes(x=client, y=quantity, fill=GNDR)) +
    theme_bw() +
    scale_fill_brewer(palette='Set1') +
    theme(panel.grid = element_blank())+
    geom_bar(stat='identity', position='fill' , alpha=0.6) +
    facet_grid(F ~ G_LAST_BUY_DAYS) +
    ggtitle("RF分析圖_所有用戶（性別）") +
    xlab("最近一次消費天數") + ylab("購買頻率")+ 
    theme(plot.title = element_text(color="black", size=15),
          axis.title.x = element_text(color="blue", size=10, face="bold"),
          axis.title.y = element_text(color="#993333", size=10, face="bold"),
          text=element_text(family="黑體-繁 中黑"))+
    guides(fill=guide_legend(title="顧客性別"))

#商品

rfm.segm$cart
ggplot(lcg.sub, aes(x=GNDR, y=quantity, fill=cart)) +
    theme_bw() +
    scale_fill_brewer(palette='Set1') +
    theme(panel.grid = element_blank())+
    geom_bar(stat='identity', position='fill' , alpha=0.6) +
    facet_grid(F ~ G_LAST_BUY_DAYS) +
    ggtitle("RF分析圖_所有用戶(商品分類)") +
    xlab("最近一次消費天數") + ylab("購買頻率")+ 
    theme(plot.title = element_text(color="black", size=15),
          axis.title.x = element_text(color="blue", size=10, face="bold"),
          axis.title.y = element_text(color="#993333", size=10, face="bold"),
          text=element_text(family="黑體-繁 中黑"))+
    guides(fill=guide_legend(title="商品顏色指示表"))


## M == 1

#新欄位編碼
rfm1.segm <- rfm1 %>%
    mutate(F=ifelse(between(F, 1, 1), '1',
                    ifelse(between(F, 2, 2), '2',
                           ifelse(between(F, 3, 3), '3', '4')))) %>%
    
    
    # 切割近因畫出邊界
    mutate(G_LAST_BUY_DAYS=ifelse(between(G_LAST_BUY_DAYS, 1, 6),  '0-6 天',
                                  ifelse(between(G_LAST_BUY_DAYS, 7, 13), '7-13 天',
                                         ifelse(between(G_LAST_BUY_DAYS, 14, 20), '14-20 天','>20 天')))) %>%
    
    mutate(cart=paste(ifelse(G_N3_BUY_LIVE_STREAM_CNT!=0, '、直播軟體', ''),
                      ifelse(G_N3_BUY_DATE_APP_CNT!=0, '、交友軟體', ''),
                      ifelse(G_N3_BUY_GAME_APP_CNT!=0, '、遊戲軟體', ''),sep='')) %>%
    
    arrange(CONTRACT_ID)

rfm1.segm$cart = str_split_fixed(rfm1.segm$cart, '、', 2)[,2]

#all
rfm1.segm$F <- factor(rfm1.segm$F, levels=c( '4', '3', '2', '1'))
rfm1.segm$G_LAST_BUY_DAYS <- factor(rfm1.segm$G_LAST_BUY_DAYS, levels=c('>20 天', '14-20 天', '7-13 天', '0-6 天'))


lcg <- rfm1.segm %>%
    group_by(G_LAST_BUY_DAYS, F) %>%
    summarise(quantity=n()) %>%
    mutate(client='顧客人數') %>%
    ungroup()

lcg.matrix= as.data.frame.matrix(table(rfm1.segm$F, rfm1.segm$G_LAST_BUY_DAYS))
lcg.matrix$F = row.names(lcg.matrix) 
lcg.matrix

lcg.adv <- lcg %>%
    mutate(G_LAST_BUY_DAYS = ifelse(G_LAST_BUY_DAYS %in% c('14-20 天', '>20 天'), "not recent", "recent"),
           F = ifelse(F %in% c('4', '3'), "frequent", "infrequent"),
           customer.type = interaction(G_LAST_BUY_DAYS, F))

ggplot(lcg.adv, aes(x=client, y=quantity, fill=customer.type)) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    geom_rect(aes(fill = customer.type), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.1) +
    facet_grid(F ~ G_LAST_BUY_DAYS) +
    geom_bar(stat='identity', alpha=0.7) +
    ggtitle("RF分析圖_壞用戶") +
    xlab("最近一次消費天數") + ylab("購買頻率")+ 
    theme(plot.title = element_text(color="black", size=15 ),
          axis.title.x = element_text(color="blue", size=10, face="bold"),
          axis.title.y = element_text(color="#993333", size=10, face="bold"),
          text=element_text(family="黑體-繁 中黑"))+
    guides(fill=guide_legend(title="客群顏色指示表"))+
    scale_fill_discrete(name="Experimental\nCondition",breaks = c('not recent.frequent','recent.frequent','not recent.infrequent','recent.infrequent'), labels = c('先前活躍用戶','現活躍用戶','先前嚐鮮用戶','現嚐鮮用戶'))


#性別


as.character(rfm1.segm$GNDR)
rfm1.segm$GNDR[rfm1.segm$GNDR == 0] <- "女"
rfm1.segm$GNDR[rfm1.segm$GNDR == 1] <- "男"
#標號方式
lcg.sub <- rfm1.segm %>%
    group_by(GNDR, G_LAST_BUY_DAYS, F, cart) %>%
    summarise(quantity=n()) %>%
    mutate(client='顧客人數') %>%
    ungroup()

lcg.sub$GNDR = factor(lcg.sub$GNDR, levels = c('女', '男'))
ggplot(lcg.sub, aes(x=client, y=quantity, fill=GNDR)) +
    theme_bw() +
    scale_fill_brewer(palette='Set1') +
    theme(panel.grid = element_blank())+
    geom_bar(stat='identity', position='fill' , alpha=0.6) +
    facet_grid(F ~ G_LAST_BUY_DAYS) +
    ggtitle("RF分析圖_壞用戶（性別）") +
    xlab("最近一次消費天數") + ylab("購買頻率")+ 
    theme(plot.title = element_text(color="black", size=15),
          axis.title.x = element_text(color="blue", size=10, face="bold"),
          axis.title.y = element_text(color="#993333", size=10, face="bold"),
          text=element_text(family="黑體-繁 中黑"))+
    guides(fill=guide_legend(title="顧客性別"))

#商品


ggplot(lcg.sub, aes(x=GNDR, y=quantity, fill=cart)) +
    theme_bw() +
    scale_fill_brewer(palette='Set1') +
    theme(panel.grid = element_blank())+
    geom_bar(stat='identity', position='fill' , alpha=0.6) +
    facet_grid(F ~ G_LAST_BUY_DAYS) +
    ggtitle("RF分析圖_壞用戶(商品分類)") +
    xlab("最近一次消費天數") + ylab("購買頻率")+ 
    theme(plot.title = element_text(color="black", size=15),
          axis.title.x = element_text(color="blue", size=10, face="bold"),
          axis.title.y = element_text(color="#993333", size=10, face="bold"),
          text=element_text(family="黑體-繁 中黑"))+
    guides(fill=guide_legend(title="商品顏色指示表"))

## M == 2

#新欄位編碼
rfm2.segm <- rfm2 %>%
    mutate(F=ifelse(between(F, 1, 1), '1',
                    ifelse(between(F, 2, 2), '2',
                           ifelse(between(F, 3, 3), '3', '4')))) %>%
    
    
    # 切割近因畫出邊界
    mutate(G_LAST_BUY_DAYS=ifelse(between(G_LAST_BUY_DAYS, 1, 6),  '0-6 天',
                                  ifelse(between(G_LAST_BUY_DAYS, 7, 13), '7-13 天',
                                         ifelse(between(G_LAST_BUY_DAYS, 14, 20), '14-20 天','>20 天')))) %>%
    
    mutate(cart=paste(ifelse(G_N3_BUY_LIVE_STREAM_CNT!=0, '、直播軟體', ''),
                      ifelse(G_N3_BUY_DATE_APP_CNT!=0, '、交友軟體', ''),
                      ifelse(G_N3_BUY_GAME_APP_CNT!=0, '、遊戲軟體', ''),sep='')) %>%
    
    arrange(CONTRACT_ID)

rfm2.segm$cart = str_split_fixed(rfm2.segm$cart, '、', 2)[,2]

#all
rfm2.segm$F <- factor(rfm2.segm$F, levels=c( '4', '3', '2', '1'))
rfm2.segm$G_LAST_BUY_DAYS <- factor(rfm2.segm$G_LAST_BUY_DAYS, levels=c('>20 天', '14-20 天', '7-13 天', '0-6 天'))


lcg <- rfm2.segm %>%
    group_by(G_LAST_BUY_DAYS, F) %>%
    summarise(quantity=n()) %>%
    mutate(client='顧客人數') %>%
    ungroup()


lcg.matrix= as.data.frame.matrix(table(rfm2.segm$F, rfm2.segm$G_LAST_BUY_DAYS))
lcg.matrix$F = row.names(lcg.matrix) 
lcg.matrix

lcg.adv <- lcg %>%
    mutate(G_LAST_BUY_DAYS = ifelse(G_LAST_BUY_DAYS %in% c('14-20 天', '>20 天'), "not recent", "recent"),
           F = ifelse(F %in% c('4', '3'), "frequent", "infrequent"),
           customer.type = interaction(G_LAST_BUY_DAYS, F))

ggplot(lcg.adv, aes(x=client, y=quantity, fill=customer.type)) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    geom_rect(aes(fill = customer.type), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.1) +
    facet_grid(F ~ G_LAST_BUY_DAYS) +
    geom_bar(stat='identity', alpha=0.7) +
    ggtitle("RF分析圖_低潛力用戶") +
    xlab("最近一次消費天數") + ylab("購買頻率")+ 
    theme(plot.title = element_text(color="black", size=15 ),
          axis.title.x = element_text(color="blue", size=10, face="bold"),
          axis.title.y = element_text(color="#993333", size=10, face="bold"),
          text=element_text(family="黑體-繁 中黑"))+
    guides(fill=guide_legend(title="客群顏色指示表"))+
    scale_fill_discrete(name="Experimental\nCondition",breaks = c('not recent.frequent','recent.frequent','not recent.infrequent','recent.infrequent'), labels = c('先前活躍用戶','現活躍用戶','先前嚐鮮用戶','現嚐鮮用戶'))


#性別


as.character(rfm2.segm$GNDR)
rfm2.segm$GNDR[rfm2.segm$GNDR == 0] <- "女"
rfm2.segm$GNDR[rfm2.segm$GNDR == 1] <- "男"
#標號方式
lcg.sub <- rfm2.segm %>%
    group_by(GNDR, G_LAST_BUY_DAYS, F, cart) %>%
    summarise(quantity=n()) %>%
    mutate(client='顧客人數') %>%
    ungroup()

lcg.sub$GNDR = factor(lcg.sub$GNDR, levels = c('女', '男'))
ggplot(lcg.sub, aes(x=client, y=quantity, fill=GNDR)) +
    theme_bw() +
    scale_fill_brewer(palette='Set1') +
    theme(panel.grid = element_blank())+
    geom_bar(stat='identity', position='fill' , alpha=0.6) +
    facet_grid(F ~ G_LAST_BUY_DAYS) +
    ggtitle("RF分析圖_低潛力用戶（性別）") +
    xlab("最近一次消費天數") + ylab("購買頻率")+ 
    theme(plot.title = element_text(color="black", size=15),
          axis.title.x = element_text(color="blue", size=10, face="bold"),
          axis.title.y = element_text(color="#993333", size=10, face="bold"),
          text=element_text(family="黑體-繁 中黑"))+
    guides(fill=guide_legend(title="顧客性別"))

#商品


ggplot(lcg.sub, aes(x=GNDR, y=quantity, fill=cart)) +
    theme_bw() +
    scale_fill_brewer(palette='Set1') +
    theme(panel.grid = element_blank())+
    geom_bar(stat='identity', position='fill' , alpha=0.6) +
    facet_grid(F ~ G_LAST_BUY_DAYS) +
    ggtitle("RF分析圖_低潛力用戶(商品分類)") +
    xlab("最近一次消費天數") + ylab("購買頻率")+ 
    theme(plot.title = element_text(color="black", size=15),
          axis.title.x = element_text(color="blue", size=10, face="bold"),
          axis.title.y = element_text(color="#993333", size=10, face="bold"),
          text=element_text(family="黑體-繁 中黑"))+
    guides(fill=guide_legend(title="商品顏色指示表"))

## M == 3

#新欄位編碼
rfm3.segm <- rfm3 %>%
    mutate(F=ifelse(between(F, 1, 1), '1',
                    ifelse(between(F, 2, 2), '2',
                           ifelse(between(F, 3, 3), '3', '4')))) %>%
    
    
    # 切割近因畫出邊界
    mutate(G_LAST_BUY_DAYS=ifelse(between(G_LAST_BUY_DAYS, 1, 6),  '0-6 天',
                                  ifelse(between(G_LAST_BUY_DAYS, 7, 13), '7-13 天',
                                         ifelse(between(G_LAST_BUY_DAYS, 14, 20), '14-20 天','>20 天')))) %>%
    
    mutate(cart=paste(ifelse(G_N3_BUY_LIVE_STREAM_CNT!=0, '、直播軟體', ''),
                      ifelse(G_N3_BUY_DATE_APP_CNT!=0, '、交友軟體', ''),
                      ifelse(G_N3_BUY_GAME_APP_CNT!=0, '、遊戲軟體', ''),sep='')) %>%
    
    arrange(CONTRACT_ID)

rfm3.segm$cart = str_split_fixed(rfm3.segm$cart, '、', 2)[,2]

#all
rfm3.segm$F <- factor(rfm3.segm$F, levels=c( '4', '3', '2', '1'))
rfm3.segm$G_LAST_BUY_DAYS <- factor(rfm3.segm$G_LAST_BUY_DAYS, levels=c('>20 天', '14-20 天', '7-13 天', '0-6 天'))


lcg <- rfm3.segm %>%
    group_by(G_LAST_BUY_DAYS, F) %>%
    summarise(quantity=n()) %>%
    mutate(client='顧客人數') %>%
    ungroup()


lcg.matrix= as.data.frame.matrix(table(rfm3.segm$F, rfm3.segm$G_LAST_BUY_DAYS))
lcg.matrix$F = row.names(lcg.matrix) 
lcg.matrix

lcg.adv <- lcg %>%
    mutate(G_LAST_BUY_DAYS = ifelse(G_LAST_BUY_DAYS %in% c('14-20 天', '>20 天'), "not recent", "recent"),
           F = ifelse(F %in% c('4', '3'), "frequent", "infrequent"),
           customer.type = interaction(G_LAST_BUY_DAYS, F))

ggplot(lcg.adv, aes(x=client, y=quantity, fill=customer.type)) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    geom_rect(aes(fill = customer.type), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.1) +
    facet_grid(F ~ G_LAST_BUY_DAYS) +
    geom_bar(stat='identity', alpha=0.7) +
    ggtitle("RF分析圖_高潛力用戶") +
    xlab("最近一次消費天數") + ylab("購買頻率")+ 
    theme(plot.title = element_text(color="black", size=15 ),
          axis.title.x = element_text(color="blue", size=10, face="bold"),
          axis.title.y = element_text(color="#993333", size=10, face="bold"),
          text=element_text(family="黑體-繁 中黑"))+
    guides(fill=guide_legend(title="客群顏色指示表"))+
    scale_fill_discrete(name="Experimental\nCondition",breaks = c('not recent.frequent','recent.frequent','not recent.infrequent','recent.infrequent'), labels = c('先前活躍用戶','現活躍用戶','先前嚐鮮用戶','現嚐鮮用戶'))


#性別


as.character(rfm3.segm$GNDR)
rfm3.segm$GNDR[rfm3.segm$GNDR == 0] <- "女"
rfm3.segm$GNDR[rfm3.segm$GNDR == 1] <- "男"
#標號方式
lcg.sub <- rfm3.segm %>%
    group_by(GNDR, G_LAST_BUY_DAYS, F, cart) %>%
    summarise(quantity=n()) %>%
    mutate(client='顧客人數') %>%
    ungroup()

lcg.sub$GNDR = factor(lcg.sub$GNDR, levels = c('女', '男'))
ggplot(lcg.sub, aes(x=client, y=quantity, fill=GNDR)) +
    theme_bw() +
    scale_fill_brewer(palette='Set1') +
    theme(panel.grid = element_blank())+
    geom_bar(stat='identity', position='fill' , alpha=0.6) +
    facet_grid(F ~ G_LAST_BUY_DAYS) +
    ggtitle("RF分析圖_高潛力用戶（性別）") +
    xlab("最近一次消費天數") + ylab("購買頻率")+ 
    theme(plot.title = element_text(color="black", size=15),
          axis.title.x = element_text(color="blue", size=10, face="bold"),
          axis.title.y = element_text(color="#993333", size=10, face="bold"),
          text=element_text(family="黑體-繁 中黑"))+
    guides(fill=guide_legend(title="顧客性別"))

#商品


ggplot(lcg.sub, aes(x=GNDR, y=quantity, fill=cart)) +
    theme_bw() +
    scale_fill_brewer(palette='Set1') +
    theme(panel.grid = element_blank())+
    geom_bar(stat='identity', position='fill' , alpha=0.6) +
    facet_grid(F ~ G_LAST_BUY_DAYS) +
    ggtitle("RF分析圖_高潛力用戶(商品分類)") +
    xlab("最近一次消費天數") + ylab("購買頻率")+ 
    theme(plot.title = element_text(color="black", size=15),
          axis.title.x = element_text(color="blue", size=10, face="bold"),
          axis.title.y = element_text(color="#993333", size=10, face="bold"),
          text=element_text(family="黑體-繁 中黑"))+
    guides(fill=guide_legend(title="商品顏色指示表"))
