library(ggplot2) 
library(ggrepel)
load("E:/returns_industry_portfolios.Rdata")
return_ind <- s[,c('V1','V3','V4')]
return_ind$V4 <- as.numeric(return_ind$V4)
s <- spread(return_ind,key=V1,value=V4)
num <- ncol(s)
a <- s[1:380,2:num]
pca1 <- prcomp(a,center = TRUE,scale. = TRUE)
df1 <- pca1$x
library(tidyr)
df <- data.frame(id=rownames(mtcars),cyl=mtcars$cyl,disp=mtcars$disp)
a=spread(df,key=cyl,value=disp)





iris_input <- iris # 使用R自带iris数据集（150*5，行为样本，列为特征） 
rownames(iris_input) <- paste("sample",1:nrow(iris_input),sep = "") # 设置样本名 
head(iris_input) # 查看数据集前几行 
#         Sepal.Length Sepal.Width Petal.Length Petal.Width Species 
# sample1          5.1         3.5          1.4         0.2  setosa 
# sample2          4.9         3.0          1.4         0.2  setosa 
# sample3          4.7         3.2          1.3         0.2  setosa

iris_scale <- scale(iris_input[,-ncol(iris_input)]) # 标准化原始数据
head(iris_scale)
#         Sepal.Length Sepal.Width Petal.Length Petal.Width
# sample1   -0.8976739  1.01560199    -1.335752   -1.311052
# sample2   -1.1392005 -0.13153881    -1.335752   -1.311052
# sample3   -1.3807271  0.32731751    -1.392399   -1.311052

cor_mat <- cor(iris_scale) # 计算相关系数矩阵
#              Sepal.Length Sepal.Width Petal.Length Petal.Width
# Sepal.Length    1.0000000  -0.1175698    0.8717538   0.8179411
# Sepal.Width    -0.1175698   1.0000000   -0.4284401  -0.3661259
# Petal.Length    0.8717538  -0.4284401    1.0000000   0.9628654

rs_mat <- eigen(cor_mat) # 特征分解
# rs_mat
# eigen() decomposition
# $values
# [1] 2.91849782 0.91403047 0.14675688 0.02071484
# 
# $vectors
# [,1]        [,2]       [,3]       [,4]
# [1,]  0.5210659 -0.37741762  0.7195664  0.2612863
# [2,] -0.2693474 -0.92329566 -0.2443818 -0.1235096
# [3,]  0.5804131 -0.02449161 -0.1421264 -0.8014492
# [4,]  0.5648565 -0.06694199 -0.6342727  0.5235971

val <- rs_mat$values # 提取特征值,即各主成分的方差
standard_deviation <- sqrt(val) # 换算成标准差
# [1] 1.7083611 0.9560494 0.3830886 0.1439265

proportion_of_variance <- val/sum(val) # 计算方差贡献率
# [1] 0.729624454 0.228507618 0.036689219 0.005178709

cumulative_proportion <- cumsum(proportion_of_variance) # 计算累积贡献率
# [1] 0.7296245 0.9581321 0.9948213 1.0000000

load_mat <- as.matrix(rs_mat$vectors) # 提取特征向量,即载荷矩阵(loadings)
PC <- iris_scale %*% load_mat # 计算主成分得分
colnames(PC) <- paste("PC",1:ncol(PC),sep = "")
df2 <- as.data.frame(PC) # 转换成数据框，否则直接用于绘图会报错
head(df2)
#               PC1        PC2         PC3          PC4
# sample1 -2.257141 -0.4784238  0.12727962  0.024087508
# sample2 -2.074013  0.6718827  0.23382552  0.102662845
# sample3 -2.356335  0.3407664 -0.04405390  0.028282305

# # 提取主成分的方差贡献率,生成坐标轴标题
# xlab2 <- paste0("PC1(",round(proportion_of_variance[1]*100,2),"%)")
# ylab2 <- paste0("PC2(",round(proportion_of_variance[2]*100,2),"%)")

# # 绘制PCA得分图
# p.pca2 <- ggplot(data = df2,aes(x = PC1,y = PC2,color = iris_input$Species))+
#   geom_point(size = 3)+
#   theme_bw()+
#   labs(x = xlab2,y = ylab2,color = "Group",title = "Plot of PCA score")+
#   stat_ellipse(aes(fill = iris_input$Species),
#                type = "norm",geom = "polygon",alpha = 0.2,color = NA)+
#   guides(fill = "none")+
#   theme(plot.title = element_text(hjust = 0.5,size = 15),
#         axis.text = element_text(size = 11),axis.title = element_text(size = 13),
#         legend.text = element_text(size = 11),legend.title = element_text(size = 13),
#         plot.margin = unit(c(0.4,0.4,0.4,0.4),'cm'))
# ggsave(p.pca2,filename = "PCA.pdf")