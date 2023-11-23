library(readr)
library(dplyr)
all_month_data <- read.csv("E:/all_month_data.csv")
all_month_data$X <- substr(all_month_data$V1,17,19)
grouped_data <- split(all_month_data, all_month_data$V1)

# k也需要手动修改
for (k_tem in (7:12)){
  k <- k_tem
  print(k)
  re_2 <- c()
  group_num <- length(names(grouped_data))
  # 计算行业内相关系数
  for (i in (1:(group_num))) {
    # for (i in (1:1)) {
    # print(i)
    group_i <- grouped_data[[names(grouped_data)[i]]]
    m_r_i <- group_i[,c('V1','type','m_r')]
    time_num <- nrow(m_r_i)
    num <- time_num-k
    if (num<0){
      next;
    }
    for (j in (1:num)){ 
      # print(j)
      re <- 1
      # 下一行需要自己手动修改
      for (tem in (1:k)){
        re <- re*(1+m_r_i[j+tem-1,3])
      }
      re <- re-1
      re_2 <- c(re_2,m_r_i[j,1],m_r_i[j,2],re)
    }
  }
  s <- t(matrix(data=re_2,  nrow=3, byrow=FALSE, dimnames=NULL))
  s <- as.data.frame(s)
  # 保存结果也要手动修改
  name <- paste('E:/returns_',k,'.Rdata')
  save(s,file=name)
}
