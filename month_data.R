setwd(dir="E:/")
load('E:/deleted_data.Rdata')
new_data$month <- substr(new_data$V2,1,7)

sz <- as.numeric(new_data$V11)
new_data$sz <- sz
sz_mean <- aggregate(new_data$sz, by=list(type=new_data$V1,new_data$month),mean)

pb <- as.numeric(new_data$V14)
new_data$bm <- 1/pb                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
bm_mean <- aggregate(new_data$bm, by=list(type=new_data$V1,new_data$month),mean)
statistics <- cbind(sz_mean,bm_mean)

names(statistics) <- c("type", "date", "mv", "temp", "temp", "bm")
statistics <- statistics[,-grep("temp",colnames(statistics))]
write.csv(x = statistics, file = "Statistics_month.csv")

#合并各种月度数据
library(readr)
Statistics_month <- read.csv("E:/Statistics_month.csv")
excess_m_r <- read.csv("E:/excess_m_r.csv")
#excess_risk_free.csv这个文件不小心被覆盖了，需要在monthly_excess_returns.R中重新跑
excess_risk_free <- read.csv("E:/excess_risk_free.csv")

all_month_data <- cbind(excess_m_r[,c(2,3,4,5,6)],excess_risk_free[,c(5,6,7)],Statistics_month[,c(4,5)])
write.csv(x = all_month_data, file = "all_month_data.csv")
