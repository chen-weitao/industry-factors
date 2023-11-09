library(readr)
library(dplyr)
all_month_data <- read.csv("E:/all_month_data.csv")
all_month_data$X <- substr(all_month_data$V1,17,19)
all <- all_month_data[,c('X','V1','type','m_r','mv')]
grouped_data <- split(all, all$X)

re_all <- c()
group_num <- length(names(grouped_data))

for (i in (1:(group_num))) {
# for (i in (1:3)) {
  print(i)
  group_i <- grouped_data[[names(grouped_data)[i]]]
  group_j <- split(group_i, group_i$type)
  time_num <- length(names(group_j))
  for (j in (1:time_num)){ 
    # print(j)
    group <-  group_j[[names(group_j)[j]]]
    re <- weighted.mean(group$m_r, group$mv)
    re_all <- c(re_all,group[1,1],group[1,2],group[1,3],re)
  }
}
s <- t(matrix(data=re_all,  nrow=4, byrow=FALSE, dimnames=NULL))
s <- as.data.frame(s)
save(s,file='E:/returns_industry_portfolios.Rdata')


