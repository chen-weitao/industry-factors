library(rhdf5)
library(hdf5r)
library(datasets)
# 创建
h5createFile("new1.h5")
h5createGroup("new1.h5","group1_mat")
h5write(aa, "new1.h5", "group1_mat/df")
h5ls("new1.h5")
# 读取
setwd(dir="C:/Users/user/Desktop")
h5_file= H5Fopen("new1.h5")
h5_file$group1_mat$df