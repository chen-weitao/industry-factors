library(readr)
library(dplyr)

load("E:/returns_2_part1.Rdata")
part_1 <- s
load("E:/returns_2_part2.Rdata")
part_2 <- s
all <- rbind(part_1,part_2)
names(all) <- c('V1','type','m_r')
all$m_r <- as.numeric(all$m_r)
all$X <- substr(all$V1,17,19)
all$code <- substr(all$V1,3,8)
grouped_data <- split(all, all$X)
grouped_data_all <- split(all, all$code)

corr_ind <- c()

# 计算行业内相关系数
for (group_industry in names(grouped_data)) {
  corr <- c()
  group_ind <- grouped_data[[group_industry]]
  # group_ind <- grouped_data[[names(grouped_data)[24]]]
  # print(names(grouped_data)[24])
  group_code <- split(group_ind, group_ind$V1)
  print(group_industry)
  if(length(names(group_code))==1){
    next;
  }
  # for (i in (1:3)) {
  for (i in (1:(length(names(group_code))-1))) {
    group_i <- group_code[[names(group_code)[i]]]
    m_r_i <- group_i[,c('type','m_r')]
    tem <- i+1
    # for (j in (tem:length(names(group_code)))) {
    for (j in (tem:length(names(group_code)))) {
      group_j <- group_code[[names(group_code)[j]]]
      m_r_j <- group_j[,c('type','m_r')]
      m_r_ij <- inner_join(m_r_i,m_r_j,by="type")
      if (nrow(m_r_ij)==1){
        # print('----------------------------------------')
        # print(m_r_ij)
        break;
      }
      if (is.na(cor(m_r_ij$m_r.x,m_r_ij$m_r.y))){
        # print('----------------------------------------')
        # print(m_r_ij)
        break;
      }
            # print(cor(m_r_ij$m_r.x,m_r_ij$m_r.y))
      corr <- c(corr,cor(m_r_ij$m_r.x,m_r_ij$m_r.y))
    }
  }
  corr_ind <- c(corr_ind,group_industry,mean(corr))
}
s <- t(matrix(data=corr_ind,  nrow=2, byrow=FALSE, dimnames=NULL))
s <- as.data.frame(s)

write.csv(x = s, file = "E:/corr_ind_2.csv")
# 
# corr <- c()
# group_num <- length(names(grouped_data_all))
# # 计算行业内相关系数
# for (i in (1:(group_num-1))) {
#   print(i)
#   group_i <- grouped_data_all[[names(grouped_data_all)[i]]]
#   m_r_i <- group_i[,c('type','m_r')]
#   for (j in ((i+1):group_num)){
#     # print(j)
#     group_j <- grouped_data_all[[names(grouped_data_all)[j]]]
#     m_r_j <- group_j[,c('type','m_r')]
#     m_r_ij <- inner_join(m_r_i,m_r_j,by="type")
#     cor <- cor(m_r_ij$m_r.x,m_r_ij$m_r.y)
#     if (is.na(cor)){
#       # print('----------------------------------------')
#       # print(m_r_ij)
#       break;
#     }
#     corr <- c(corr,cor)
#   }
# }
# # mean(corr)
# 
# # write.csv(x = s, file = "E:/corr_1.csv")



# # 对每个子集进行回归分析
# for (group in names(grouped_data)) {
#   group_data <- grouped_data[[group]]
#   group_mean <- mean(group_data$m_r)
#   grouped <- split(group_data, group_data$V1)
#   for (g in names(grouped)) {
#     gro <- grouped[[g]]
#     sigma_i <- sqrt(sum((gro$m_r-mean(gro$m_r))^2)/(nrow(gro)-1-1))
#   }
# }
# save(corr,file='E:/corr.Rdata')


