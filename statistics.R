setwd(dir="E:/")
load('merge_all.Rdata')
# 会修改原数据，所以要备份
new_data <- new.data
industry_code <- substr(new_data$code,17,19)
new_data$v14 <- industry_code
names(new_data)<- paste0("V", 1:ncol(new_data))
# null_data <- grep('--', new_data$V7, value = T)
new_data=subset(new_data, !grepl("--", V7))
save(new_data,file='E:/deleted_data.Rdata') 
zdf <- as.numeric(new_data$V7)
zdf_mean <- aggregate(zdf, by=list(type=new_data$V14),mean)
zdf_sd <- aggregate(zdf, by=list(type=new_data$V14),sd)
statistics <- cbind(zdf_mean,zdf_sd)
names(statistics) <- c("industry", "mean", "temp", "sd")
statistics <- statistics[,-grep("temp",colnames(statistics))]
write.csv(x = statistics, file = "Statistics.csv")

sz <- as.numeric(new_data$V11)
new_data$lnsz <- log(sz)
sz_mean <- aggregate(new_data$v15, by=list(type=new_data$V14),mean)
pb <- as.numeric(new_data$V13)
new_data$bm <- 1/pb
bm_mean <- aggregate(new_data$bm, by=list(type=new_data$V14),mean)
