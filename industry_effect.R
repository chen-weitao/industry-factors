library(readr)
library(dplyr)

# all_month_data <- read.csv("E:/all_month_data.csv")
# all_month_data$X <- substr(all_month_data$V1,17,19)
# grouped_data <- split(all_month_data, all_month_data$X)
# deleted_data <- data.frame()
# 
# # 剔除行业内公司数小于等于3个的行业
# group_num <- length(names(grouped_data))
# for (i in (1:(group_num))) {
# # for (i in (1:3)) {
#   print(i)
#   group_i <- grouped_data[[names(grouped_data)[i]]]
#   group_j <- split(group_i, group_i$V1)
#   time_num <- length(names(group_j))
#   print(time_num)
#   if (time_num<=4){
#     next;
#   }
#   deleted_data <- rbind(deleted_data,group_i)
# }
# 
# save(deleted_data,file='E:/industry_effect_data.Rdata')


# # 先删除存续期不足13个月的公司，因为要算前12个月的平均收益率
# load('E:/industry_effect_data.Rdata')
# grouped_data <- split(deleted_data, deleted_data$V1)
# deleted_data_new <- data.frame()
# group_num <- length(names(grouped_data))
# for (i in (1:(group_num))) {
# # for (i in (1:5)) {
#   print(i)
#   group_i <- grouped_data[[names(grouped_data)[i]]]
#   time_num <- nrow(group_i)
#   if (time_num<13){
#     next;
#   }
#   deleted_data_new <- rbind(deleted_data_new,group_i)
# }
# save(deleted_data_new,file='E:/industry_effect_data_new.Rdata')


# # 计算过去12个月的平均收益率
# load('E:/industry_effect_data_new.Rdata')
# re <- deleted_data_new[,c('V1','type','m_r')]
# grouped_data <- split(re, re$V1)
# p12m <- c()
# group_num <- length(names(grouped_data))
# 
# for (i in (1:(group_num))) {
# # for (i in (1:5)) {
#   print(i)
#   group_i <- grouped_data[[names(grouped_data)[i]]]
#   time_num <- nrow(group_i)
#   p12m <- c(p12m,'na','na','na','na','na','na','na','na','na','na','na','na')
#   for (j in (13:time_num)){
#     time_num <- length(names(group_i))
#     p_re <- mean(group_i[(j-12):j,'m_r']) 
#     p12m <- c(p12m,p_re)
#   }
# }
# p12m_df <- as.data.frame(p12m)
# p12m_matrix <- as.matrix (p12m)
## 绝对不能向df中添加df，应该使用as.matrix()。我是用的解决方法是先把它
## 转化为矩阵（as.matrix()）再进行赋值，为什么不用as.vector()转化为向量呢，
## 这个我也试过，貌似数据框不能直接转化为向量，但在R语言中矩阵就是向量，
## 所以效果是等价的。还有一种数据结构是数组

# deleted_data_new$p12m <- p12m_df
# save(deleted_data_new,file='E:/industry_effect_deleted_data_new.Rdata')

# # 删除空值
# load('E:/industry_effect_deleted_data_new.Rdata')
# # 将最后一列的df格式转为正常情况
# deleted_data_new[,12] <- deleted_data_new[,12]
# deleted_data_new <- deleted_data_new[!(deleted_data_new$p12m == 'na'),]
# # 替换最小和最大0.05之外的数据
# small <- quantile(deleted_data_new$bm,0.05)
# large <- quantile(deleted_data_new$bm,0.95)
# deleted_data_new[which(deleted_data_new$bm<small),'bm'] <- small
# deleted_data_new[which(deleted_data_new$bm>large),'bm'] <- large
# save(deleted_data_new,file='E:/industry_effect_deleted_data_new_new.Rdata')


# 计算某月行业内和行业间的中值，这样直接合并很慢，先算平均值到c()，再匹配到原数据中
load('E:/industry_effect_deleted_data_new_new.Rdata')
deleted_data_new$p12m <- as.numeric(deleted_data_new$p12m)
grouped_data <- split(deleted_data_new, deleted_data_new$type)
group_num <- length(names(grouped_data))
winthin_data <- c()
across_data <- c()
for (i in (1:(group_num))) {
  # for (i in (5:5)) {
  print(i)
  ind_mv_c <- c()
  ind_bm_c <- c()
  ind_p12m_c <- c()
  group_i <- grouped_data[[names(grouped_data)[i]]]
  group_j <- split(group_i, group_i$X)
  industry_num <- length(names(group_j))
  # print(industry_num)
  for (j in (1:industry_num)){
    industry <- group_j[[names(group_j)[j]]]
    ind_median_mv <- median(industry$mv)
    ind_median_bm <- median(industry$bm)
    ind_median_p12m <- median(industry$p12m)
    ind_mv_c <- c(ind_mv_c,ind_median_mv)
    ind_bm_c <- c(ind_bm_c,ind_median_bm)
    ind_p12m_c <- c(ind_p12m_c,ind_median_p12m)
    winthin_data <- c(winthin_data,industry[1,'X'],industry[1,'type'],ind_median_mv,ind_median_bm,ind_median_p12m)
  }
  across_mv <- median(ind_mv_c) 
  across_bm <- median(ind_bm_c) 
  across_p12m <- median(ind_p12m_c) 
  across_data <- c(across_data,group_i[1,'type'],across_mv,across_bm,across_p12m)
}

winthin <- t(matrix(data=winthin_data,  nrow=5, byrow=FALSE, dimnames=NULL))
winthin <- as.data.frame(winthin)
across <- t(matrix(data=across_data,  nrow=4, byrow=FALSE, dimnames=NULL))
across <- as.data.frame(across)


# 1992年4月和5月的行业个数都是1个，所以不包括
for (i in (1:(group_num))) {
  # for (i in (5:5)) {
  print(i)
  group_i <- grouped_data[[names(grouped_data)[i]]]
  data_row <- which(grepl(group_i[1,'type'], deleted_data_new$type))
  across_row <- which(grepl(group_i[1,'type'], across$V1))
  deleted_data_new[data_row,'across_mv'] <- across[across_row,'V2']
  deleted_data_new[data_row,'across_bm'] <- across[across_row,'V3']
  deleted_data_new[data_row,'across_p12m'] <- across[across_row,'V4']
  group_j <- split(group_i, group_i$X)
  industry_num <- length(names(group_j))
  Nnindcd <- deleted_data_new[data_row,'X']
  Nnindcd <- as.data.frame(Nnindcd)
  Nnindcd <- cbind(Nnindcd,data_row)
  # print(industry_num)
  for (j in (1:industry_num)){
    industry <- group_j[[names(group_j)[j]]]
    cbind(Nnindcd,data_row)
    # 直接grep返回索引列的值
    row_2 <- which(grepl(industry[1,'X'], Nnindcd$Nnindcd))
    row_3 <- Nnindcd[row_2,'data_row']
    ind_median_mv <- median(industry$mv)
    ind_median_bm <- median(industry$bm)
    ind_median_p12m <- median(industry$p12m)
    # print(industry[1,'X'])
    # print(all_median_mv)
    # print(row_2)

    deleted_data_new[row_3,'ind_mv'] <- ind_median_mv
    deleted_data_new[row_3,'ind_bm'] <- ind_median_bm
    deleted_data_new[row_3,'ind_p12m'] <- ind_median_p12m
  }
}
deleted_data_new$across_mv <- as.numeric(deleted_data_new$across_mv)
deleted_data_new$across_bm <- as.numeric(deleted_data_new$across_bm)
deleted_data_new$across_p12m <- as.numeric(deleted_data_new$across_p12m)

deleted_data_new$mv_within <- deleted_data_new$mv-deleted_data_new$ind_mv
deleted_data_new$mv_across <- deleted_data_new$mv-deleted_data_new$across_mv

deleted_data_new$bm_within <- deleted_data_new$bm-deleted_data_new$ind_bm
deleted_data_new$bm_across <- deleted_data_new$bm-deleted_data_new$across_bm

deleted_data_new$p12m_within <- deleted_data_new$p12m-deleted_data_new$ind_p12m
deleted_data_new$p12m_across <- deleted_data_new$p12m-deleted_data_new$across_p12m

deleted_data_new$dummy_mv <- deleted_data_new$mv_within
deleted_data_new$dummy_bm <- deleted_data_new$bm_within
deleted_data_new$dummy_p12m <- deleted_data_new$p12m_within

deleted_data_new$dummy_mv <- ifelse(deleted_data_new$dummy_mv > 0, deleted_data_new$dummy_mv, 0)
deleted_data_new$dummy_bm <- ifelse(deleted_data_new$dummy_bm > 0, deleted_data_new$dummy_bm, 0)
deleted_data_new$dummy_p12m <- ifelse(deleted_data_new$dummy_p12m > 0, deleted_data_new$dummy_p12m, 0)

save(deleted_data_new,file='E:/finished_data_all.Rdata') 







