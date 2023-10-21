library(dplyr)
#设立工作目录，读取文件名列表
setwd(dir="E:/part_data")
files_li <- list.files()
# 命令构建路径变量dir（方便更改），也可以不构建，后面示例
dir <- paste("./",files_li,sep="")                 
# 文件个数
len <- length(files_li)
# 第一个
load(dir[1])
new_data <-  merge.data
# new.data <- arrange(data_first, '日期')
# 再合并后面几个
for (i in 2:len){
  load(dir[i])
  data_i=merge.data
  # data_sorted <- arrange(data_i, '日期')
  new.data <- rbind(new.data,data_i)
}
save(new.data,file='E:/merge_all.Rdata') 