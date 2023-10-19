library(readxl)
library(readr)
library(plyr)
# 开始计时
start_time <- Sys.time()
# 读取文件
Industry <- read_xlsx('C:/Users/user/Desktop/公司文件/TRD_Co.xlsx')
Industry <- Industry[3:nrow(Industry),]
# daily_stock <- read_csv('C:/Users/user/Desktop/WINDdailyStock/000001.SZ.CSV',locale=locale(encoding="GBK"))
#设立工作目录，读取文件名列表
# files_li <- list.files("C:/Users/user/Desktop/WINDdailyStock")
setwd(dir="C:/Users/user/Desktop/WINDdailyStock")
files_li <- list.files()
# 命令构建路径变量dir（方便更改），也可以不构建，后面示例
dir <- paste("./",files_li,sep="")                 
# 文件个数
len <- length(files_li)
code <- substr(files_li,1,6)
# Industry[row,12]
#定义不变量
# lis <- c(0,10,20,30,40,100)
lis <- c(0,1000,2000,3000,4000,len)
col <- c(3,5,8,9,10,12,14,15,16,17,18,19,20)

for (j in 1:5){
  start <- lis[j]+1
  # print('----------------')
  # print(lis[j])
  # print(lis[j+1])
  # print('----------------')
  print(start)
  # print(dir[start])
  row <- which(grepl(code[start], Industry$Stkcd))
  Nnindcd <- paste(Industry[row,11],Industry[row,12],sep="-")
  tem <- paste(dir[start],Nnindcd,sep="-")
  # 读入第一个文件内容
  merge.data <- read.csv(file = dir[start],header=T,sep=",", fileEncoding = "GBK")
  merge.data <- merge.data[,col]
  merge.data <- cbind(tem, merge.data)
  merge.data <- rename(merge.data, c("tem" = "code"))
  # 循环从第二个文件开始读入所有文件，并组合到merge.data变量中
  for (i in (lis[j]+2):lis[j+1]){
    print(i)
    # print(dir[i])
    row <- which(grepl(code[i], Industry$Stkcd))
    Nnindcd <- paste(Industry[row,11],Industry[row,12],sep="-")
    tem <- paste(dir[i],Nnindcd)
    new.data <- read.csv(file = dir[i], header=T, sep=",", fileEncoding = "GBK")
    new.data <- new.data[,col]
    new.data <- cbind(tem, new.data)
    new.data <- rename(new.data, c("tem" ="code"))
    merge.data <- rbind(merge.data,new.data)
  }
  merge.data <- merge.data[,-ncol(merge.data)]
  filename <- paste('E:/part_data',j,'.Rdata', sep = "", collapse = NULL)
  # 输出组合后的文件merge_all.csv到input文件
  # write.csv(merge.data,file = "E:/merge_all.csv",row.names=FALSE, fileEncoding = "GBK")
  save(merge.data,file=filename) 
}
# 结束计时
end_time <- Sys.time()
end_time - start_time 
# load('E:/part_data/1.Rdata')
# print(dir)