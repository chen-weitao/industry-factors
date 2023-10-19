setwd(dir="E:/")
load('merge_all.Rdata')
industry_code <- substr(new.data$code,17,19)
new.data$v15 <- industry_code
data <- new.data
names(data)<- paste0("V", 1:ncol(data))
zdf <- as.numeric(data$V7)
zdf_mean <- aggregate(zdf, by=list(type=data$V15),mean)


Results <- grep('N77', data$V15, value = T)
aaaa=subset(data, grepl("N77", V15))

Results <- grep('--', data$V7, value = T)
aaaav=subset(data, !grepl("--", V7)) 

save(aaaav,file='E:/delete_one_N77.Rdata') 
test=aaaav
c=as.numeric(test$V7)
b=aggregate(c, by=list(type=test$V15),mean)
c=aggregate(c, by=list(type=test$V15),sd)
d=cbind(b,c)

names(d) <- c("industry", "mean", "temp", "sd")
d=d[,-grep("temp",colnames(d))]
write.csv(x = d,file = "Statistics.csv")

