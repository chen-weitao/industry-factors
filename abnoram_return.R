library(readr)
library(dplyr)
all_month_data <- read.csv("E:/all_month_data.csv")
all_month_data$X <- substr(all_month_data$V1,17,19)
s1 <- all_month_data %>%
  group_by(X,type) %>%
  summarise(w_avg = weighted.mean(m_r, mv))
s2 <- all_month_data %>%
  group_by(X,type) %>%
  summarise(w_avg = weighted.mean(excess_m_r, mv))
s1$w_avg <- s1$w_avg*100
s <- cbind(s1,s2[,3])

#合并三因子
factor_m <- read_xlsx("C:/Users/User/Desktop/因子/三因子模型指标(月)202136394(仅供浙江工商大学使用)/STK_MKT_THRFACMONTH.xlsx")
factor_m <- factor_m[3:nrow(factor_m),]
row <- which(grepl('P9715', factor_m$MarkettypeID))
factor_P9715 <- factor_m[row,2]
factor_P9715 <- cbind(factor_P9715,factor_m[row,3])
factor_P9715 <- cbind(factor_P9715,factor_m[row,5])
factor_P9715 <- cbind(factor_P9715,factor_m[row,7])
for (i in (1:nrow(s))){
  row <- which(grepl(s[i,2], factor_P9715$TradingMonth))
  print(i)
  s[i,'f1'] <- factor_P9715[row,2]
  s[i,'f2'] <- factor_P9715[row,3]
  s[i,'f3'] <- factor_P9715[row,4]
}
names(s) <- c('industry','date','m_r','e_m_r','f1','f2','f3')
s$f1 <- as.numeric(s$f1)*100
s$f2 <- as.numeric(s$f2)*100
s$f3 <- as.numeric(s$f3)*100
write.csv(x = s, file = "s.csv")

# 拆分
s_group <- split(s, s$industry)
# 计算
s_group_fit <- lapply(s_group, lm, formula = e_m_r ~ f1 + f2 + f3)
s_group_fit_coef <- lapply(s_group_fit, coef)
# 合并
dat <- do.call("rbind", s_group_fit_coef)

abnormal_return <- as.data.frame(dat)
write.csv(x = abnormal_return, file = "abnormal_return.csv")

Statistics <- read.csv("E:/Statistics.csv")
Statistics$a_r <- abnormal_return$`(Intercept)`
colMeans(Statistics[,c(3,4,5,6,7,8,9)])
