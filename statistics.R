setwd(dir="E:/")
# load('merge_all.Rdata')
# # 会修改原数据，所以要备份
# new_data=new_data[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,15)]
# industry_code <- substr(new_data$code,17,19)
# new_data$v15 <- industry_code
# names(new_data)<- paste0("V", 1:ncol(new_data))
# # null_data <- grep('--', new_data$V7, value = T)
# new_data=subset(new_data, !grepl("--", V7))
# new_data=subset(new_data, !grepl("--", V14))
# save(new_data,file='E:/deleted_data.Rdata') 
load('E:/deleted_data.Rdata')
zdf <- as.numeric(new_data$V7)
zdf_mean <- aggregate(zdf, by=list(type=new_data$V15),mean)
zdf_sd <- aggregate(zdf, by=list(type=new_data$V15),sd)
statistics <- cbind(zdf_mean,zdf_sd)

sz <- as.numeric(new_data$V11)
new_data$lnsz <- log(sz)
sz_mean <- aggregate(new_data$lnsz, by=list(type=new_data$V15),mean)
statistics <- cbind(statistics,sz_mean)
pb <- as.numeric(new_data$V14)
new_data$bm <- 1/pb                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
bm_mean <- aggregate(new_data$bm, by=list(type=new_data$V15),mean)
statistics <- cbind(statistics,bm_mean)
new_data$month <- substr(new_data$V2,1,7)
firms_number <- aggregate(new_data$V3, by=list(new_data$V15,new_data$month,new_data$V1),length)
firms_number_m <- aggregate(firms_number$Group.1,by=list(firms_number$Group.1,firms_number$Group.2),length)
firms_number_mean <- aggregate(firms_number_m$x,by=list(firms_number_m$Group.1),mean)
firms_number_sd <- aggregate(firms_number_m$x,by=list(firms_number_m$Group.1),sd)
statistics <- cbind(statistics,firms_number_mean)
statistics <- cbind(statistics,firms_number_sd)
names(statistics) <- c("industry", "zdf_mean", "temp", "zdf_sd", "temp", "lnsz_mean", 
                       "temp", "bm_mean",'temp','firms_mean','temp','firms_sd')
statistics <- statistics[,-grep("temp",colnames(statistics))]
write.csv(x = statistics, file = "Statistics.csv")
