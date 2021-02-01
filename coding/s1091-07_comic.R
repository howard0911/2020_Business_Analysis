COMIC <- read.table("comic_origin.txt", header=TRUE, sep="\t")

COMIC_noop <- read.table("comic_noOP.txt", header=TRUE, sep="\t")
attach(COMIC_NOOP)

畫風 <- factor(畫風)
漫畫類別 <- factor(漫畫類別)
出版社 <- factor(出版社)
連載刊物 <- factor(連載刊物)
目標客群 <- factor(目標客群)

COMIC_NOOPa <- COMIC_NOOP[-18,]
COMIC_NOOPa <- COMIC_NOOPa[-18,]
COMIC_NOOPa <- COMIC_NOOPa[-56,]

plot(COMIC)
fit <- lm(銷售量~作家性別+目標客群+同作者代表作+戰鬥元素+當季動畫
             +真人化+這本漫畫真厲害+店員推薦+雜誌發行量+CP值+已銷售月份長度
             +腐度*動畫化+同人創作熱度+漫畫類別*畫風, data=COMIC_NOOPa)
summary(fit)



