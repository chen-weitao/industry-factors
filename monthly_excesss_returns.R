library(dplyr)
library(readxl)

# load('E:/deleted_data.Rdata')
# monthly_returns=new_data[,c(1,2,3,4,11)]
# save(monthly_returns,file='E:/monthly_returns.Rdata')
# load('E:/monthly_returns.Rdata')
# monthly_returns$V6 <- substr(monthly_returns$V1,3,8)
# monthly_returns$V7 <- substr(monthly_returns$V1,17,19)
# monthly_returns$V8 <- substr(monthly_returns$V2,1,7)
# # group <- group_by(monthly_returns, V1, V8)
# # 先分组后apply
# start <- aggregate(
#   monthly_returns$V3,
#   by=list(monthly_returns$V1, monthly_returns$V8),
#   FUN=first
# )
# close <- aggregate(
#   monthly_returns$V4,
#   by=list(monthly_returns$V1, monthly_returns$V8),
#   FUN=last
# )
# s_c <- cbind(start$Group.1,start$Group.2,start$x,close$x)
# s_c <- as.data.frame(s_c)
# s_c <- s_c[order(s_c$V1),]
# s_c$V5 <- c(0,s_c[1:nrow(s_c)-1,4])
# s_c$monthly_return <- (as.numeric(s_c$V4)-as.numeric(s_c$V3))/
#   as.numeric(s_c$V3)
# save(s_c,file='E:/s_c.Rdata')

load('E:/s_c.Rdata')
s_c$monthly_return_next <- (as.numeric(s_c$V4)-as.numeric(s_c$V5))/
  as.numeric(s_c$V5)
risk_free <- read_xlsx("C:/Users/User/Desktop/利率/无风险利率文件表160914914(仅供浙江工商大学使用)/BND_Exchange.xlsx")
risk_free <- risk_free[3:nrow(risk_free),]
risk_free$yyyymm <- substr(risk_free$Clsdt,1,7)
risk_free <- as.data.frame(risk_free)
colnames(s_c)[2] <- 'type'
monthly_risk_free <- aggregate(as.numeric(risk_free$Nrrmtdt), by=list(type=risk_free$yyyymm),mean)
new_s_c <- left_join(s_c,monthly_risk_free,by="type")
new_s_c$excess_monthly_return <- new_s_c$monthly_return*100-new_s_c$x
new_s_c$excess_monthly_return_next <- new_s_c$monthly_return_next*100-new_s_c$x
new_s_c$excess_m_r <- c(new_s_c[1,9],new_s_c[2:nrow(new_s_c),10])
excess_risk_free <- new_s_c[order(new_s_c$V1),c('V1','type','excess_m_r')]

# 算上月末收盘价和这月末收盘价，或者IPO发行价