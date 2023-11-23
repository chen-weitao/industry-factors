require(foreign)
require(plm)
require(lmtest)
library(dplyr)
require("magrittr")
# # 增加日期序号
# load("E:/finished_data_all.Rdata")
# grouped_data <- split(deleted_data_new, deleted_data_new$type)
# group_num <- length(names(grouped_data))
# for (i in (1:(group_num))) {
#   # for (i in (5:5)) {
#   print(i)
#   group <- grouped_data[[names(grouped_data)[i]]]
#   data_row <- which(grepl(group[1,'type'], deleted_data_new$type))
#   deleted_data_new[data_row,'date_order'] <- i
# }
# 
# # 增加公司序号
# grouped_data <- split(deleted_data_new, deleted_data_new$V1)
# group_num <- length(names(grouped_data))
# for (i in (1:(group_num))) {
#   # for (i in (5:5)) {
#   print(i)
#   group <- grouped_data[[names(grouped_data)[i]]]
#   data_row <- which(grepl(group[1,'V1'], deleted_data_new$V1))
#   deleted_data_new[data_row,'V1_order'] <- i
# }
# 
# save(deleted_data_new,file='E:/finished_data_all_new.Rdata') 


#载入数据
load('E:/finished_data_all_new.Rdata')
deleted_data_new$p12m <- deleted_data_new$p12m*100
deleted_data_new$across_p12m <- deleted_data_new$across_p12m*100
deleted_data_new$ind_p12m <- deleted_data_new$ind_p12m*100
deleted_data_new$p12m_within <- deleted_data_new$p12m_within*100
deleted_data_new$p12m_across <- deleted_data_new$p12m_across*100
deleted_data_new$dummy_p12m <- deleted_data_new$dummy_p12m*100
# deleted_data_new_test <- deleted_data_new[600000:603664,]
# fpmg <- pmg(excess_m_r~mv_within, deleted_data_new_test, index=c("type","V1"))

# deleted_data_new <-  deleted_data_new[50000:nrow(deleted_data_new),]
deleted_data_new <- deleted_data_new[order(deleted_data_new$date_order),]
rownames(deleted_data_new) <- 1:nrow(deleted_data_new)
# 从1998年1月开始
deleted_data_new <- deleted_data_new[10450:nrow(deleted_data_new),]
# deleted_data_new <- deleted_data_new[45450:nrow(deleted_data_new),]

# # 剔除 货币金融服务（J66） 其他金融业（J69）
# deleted_data_new <- deleted_data_new %>%
#   filter(X != 'J66')
# deleted_data_new <- deleted_data_new %>%
#   filter(X != 'J69')
# row <- which(grepl('J66', deleted_data_new$X))
# deleted_data_new[row,]

# # 只保留一月
# deleted_data_new$month <- substr(deleted_data_new$type,6,7)
# deleted_data_new <- deleted_data_new %>%
#   filter(month == '01')

# # 不保留一月
# deleted_data_new$month <- substr(deleted_data_new$type,6,7)
# deleted_data_new <- deleted_data_new %>%
#   filter(month != '01')

# # 上市月份不足13月的数据
# data_filter1 <- deleted_data_new %>%
#   group_by(V1_order) %>%
#   mutate(flag = (if (length(V1_order) < 13 ) {1} else {0})) %>%
#   ungroup() %>%
#   filter(flag == 1)

# 上市月份大于12月的数据
data_filter2 <- deleted_data_new %>%
  group_by(V1_order) %>%
  mutate(flag = (if (length(V1_order) < 13 ) {1} else {0})) %>%
  ungroup() %>%
  filter(flag == 0)

# # 删除月份少于12的
# data_filter3 <- data_filter2 %>%
#   group_by(date_order) %>%
#   mutate(flag = (if (length(date_order) < 13 ) {1} else {0})) %>%
#   ungroup() %>%
#   filter(flag == 0)
# 
# data_filter3 <- data_filter3[order(data_filter3$date_order),]
# data_filter3 <- data_filter3[6703:nrow(data_filter3),]
data_filter_last <- data_filter2
# Fama-MacBeth回归
fpmg <- pmg(excess_m_r~mv_within + dummy_mv + bm_within + dummy_bm + p12m_within + dummy_p12m, data_filter_last, index=c("type","V1"))

# fpmg <- pmg(excess_m_r~ mv+bm+p12m+dummy_mv+dummy_bm+dummy_p12m, data_filter_last, index=c("type","V1"))
# fpmg <- pmg(excess_m_r~ excess_m_r+m_r, data_filter_last, index=c("type","V1"))

fpmg.coefficients <- fpmg$coefficients
coeftest(fpmg)

the.years <- unique(data_filter_last$date_order)
# a.formula <- data_filter_last$excess_m_r ~ data_filter_last$mv+data_filter_last$bm+data_filter_last$p12m+data_filter_last$dummy_mv+data_filter_last$dummy_bm+data_filter_last$dummy_p12m
a.formula <-excess_m_r~mv_within + dummy_mv + bm_within + dummy_bm + p12m_within + dummy_p12m

first.step <-  lapply(the.years, function(a.year) {
  temp.data <- data_filter_last[data_filter_last$date_order == a.year, ]
  an.lm <- lm(a.formula, data = temp.data)
  the.coefficients <- an.lm$coef
  the.results <- as.data.frame(cbind(a.year, t(the.coefficients)))
  the.results
}) 
first.step.df <- do.call('rbind', first.step)
first.step.df$mv_dif <- first.step.df$mv_within+first.step.df$dummy_mv
first.step.df$bm_dif <- first.step.df$bm_within+first.step.df$dummy_bm
first.step.df$p12m_dif <- first.step.df$p12m_within+first.step.df$dummy_p12m

second.step.coefficients <- apply(first.step.df[, -1], 2, mean)
# second.step.coefficients

# identical(fpmg.coefficients, second.step.coefficients)

# 观测值个数的0.25次方 
# lag_num <- nrow(first.step.df)^(1/4)
# lag_num <- 4*(nrow(first.step.df)/100)^(2/9)
lag_num <- 12



library(sandwich)
second.step.NW.sigma.sq <- apply(first.step.df[, -1], 2, 
                                 function(x) sqrt(NeweyWest(lm(x ~ 1), 
                                                            lag = lag_num, prewhite = FALSE)['(Intercept)',       
                                                                                             '(Intercept)']))
# second.step.NW.sigma.sq

# t.statistics.NW.lag.1 <- second.step.coefficients / second.step.NW.sigma.sq
# 
# t.statistics.NW.lag.1


# 计算p值

intercept <- coeftest(lm(first.step.df$'(Intercept)' ~ 1), vcov = NeweyWest(lm(first.step.df$'(Intercept)' ~ 1), lag = lag_num, prewhite = FALSE))

# mv
mv_within <- coeftest(lm(first.step.df$mv_within ~ 1), vcov = NeweyWest(lm(first.step.df$bm_within ~ 1), lag = lag_num, prewhite = FALSE))
p_zhi <- rbind(intercept,mv_within)
dummy_mv <- coeftest(lm(first.step.df$dummy_mv ~ 1), vcov = NeweyWest(lm(first.step.df$bm_within ~ 1), lag = lag_num, prewhite = FALSE))
p_zhi <- rbind(p_zhi,dummy_mv)
mv_dif <- coeftest(lm(first.step.df$mv_dif ~ 1), vcov = NeweyWest(lm(first.step.df$bm_within ~ 1), lag = lag_num, prewhite = FALSE))
p_zhi <- rbind(p_zhi,mv_dif)

# bm
bm_within <- coeftest(lm(first.step.df$bm_within ~ 1), vcov = NeweyWest(lm(first.step.df$bm_within ~ 1), lag = lag_num, prewhite = FALSE))
p_zhi <- rbind(p_zhi,bm_within)
dummy_bm <- coeftest(lm(first.step.df$dummy_bm ~ 1), vcov = NeweyWest(lm(first.step.df$bm_within ~ 1), lag = lag_num, prewhite = FALSE))
p_zhi <- rbind(p_zhi,dummy_bm)
bm_dif <- coeftest(lm(first.step.df$bm_dif ~ 1), vcov = NeweyWest(lm(first.step.df$bm_within ~ 1), lag = lag_num, prewhite = FALSE))
p_zhi <- rbind(p_zhi,bm_dif)

# p12m
p12m_within <- coeftest(lm(first.step.df$p12m_within ~ 1), vcov = NeweyWest(lm(first.step.df$bm_within ~ 1), lag = lag_num, prewhite = FALSE))
p_zhi <- rbind(p_zhi,p12m_within)
dummy_p12m <- coeftest(lm(first.step.df$dummy_p12m ~ 1), vcov = NeweyWest(lm(first.step.df$bm_within ~ 1), lag = lag_num, prewhite = FALSE))
p_zhi <- rbind(p_zhi,dummy_p12m)
p12m_dif <- coeftest(lm(first.step.df$p12m_dif ~ 1), vcov = NeweyWest(lm(first.step.df$bm_within ~ 1), lag = lag_num, prewhite = FALSE))
p_zhi <- rbind(p_zhi,p12m_dif)

p_zhi_df <- as.data.frame(p_zhi)
row.names(p_zhi_df) <- c('intercept','mv_within','mv_across','mv_dif','bm_within','bm_across','bm_dif','p12m_within','p12m_across','p12m_dif')

p_zhi_df <- t(p_zhi_df)

# 
# # 计算差异
# mv_dif_li <- as.data.frame(first.step.df$mv_within-first.step.df$mv_across)
# lag_mv <- nrow(mv_dif_li)^(1/4)
# mv_dif <- apply(mv_dif_li, 2, function(x) sqrt(NeweyWest(lm(x ~ 1), lag =lag_mv , prewhite = FALSE)['(Intercept)',       
#                                                                                              '(Intercept)']))
# 
# t.test(first.step.df$mv_within,first.step.df$mv_across)
# t.test(first.step.df$bm_within,first.step.df$bm_across)
# t.test(first.step.df$bm_within,first.step.df$bm_across)
