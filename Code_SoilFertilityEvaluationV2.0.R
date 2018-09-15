# 程序名称：土壤肥力综合评价初步研究 算法
# 版本：V2.0，2017.9.5修订 
# 作者：Guoqiang Li
# E-Mail: agri521#gmail.com
# 说明：算法摘自“土壤肥力综合评价初步研究”，浙江大学学报，1999，25（4）：378-382

# System Options
## setwd工作路径，修改为CSV文件和源代码所在文件夹，建议文件名为英文
## 统一修改工作路径为"E:/R Studio/Project/SoilFertilityEvaluation/"
setwd("E:/R Studio/Project/SoilFertilityEvaluation/")
rm(list=ls(all=TRUE))
options(digits = 4)

# Read Data From CSV File
mydata <- read.csv("SoilFertilityZhejiang.csv", sep=",", header=TRUE,skip = 1)

# Statistical Analysis For Soil Fertility Data
## Descriptive statistics, five values of totalN, availableN, availableP, availableK, Orgaincmydata[,-1]
mydata.mean <- unlist(lapply(mydata[,-1],mean))
mydata.min <- unlist(lapply(mydata[,-1],min))
mydata.max <- unlist(lapply(mydata[,-1],max))
mydata.sd <- unlist(lapply(mydata[,-1],sd))
mydata.cv <- unlist(lapply(mydata[,-1],function(x) sd(x)/mean(x)*100))

mydata.result <- rbind(mydata.mean,mydata.min,mydata.max,mydata.sd,mydata.cv)
row.names(mydata.result) <- c("Mean","Min","Max","SD","CV")

## 调用自定属函数
source("UserDefinedFunction.R")

## 有机质
# 定义隶属值向量
organic.matter <- fun_Membership(mydata$organic,xmin=10,xmax=20)
## 全氮
total.nitrogen <- fun_Membership(mydata$totalN,xmin=0.5,xmax=1.5)
## 碱解氮
available.nitrogen <- fun_Membership(mydata$availN,xmin=50,xmax=150)
## 全磷
total.phosphorus <- fun_Membership(mydata$totalP,xmin=0.2,xmax=0.45)
## 速效磷
available.phosphorous <- fun_Membership(mydata$availP,xmin=3,xmax=20)
## 缓效钾
slowly.available.k <- fun_Membership(mydata$availK,xmin=150,xmax=500)
## 速效钾
available.potassium <- fun_Membership(mydata$availK2,xmin=50,xmax=180)

# 单项指标权重的确定
# 求单项肥力指标之间的相关系数，然后计算单项肥力相关系数/所有肥力指标相关系数均值
indexWeight <- fun_Weight(mydata[,2:8])

# 土壤养分肥力综合指标计算
#IFI <- organic.matter*indexWeight$organic + total.nitrogen*indexWeight$totalN  + available.nitrogen*indexWeight$availN + total.phosphorus*indexWeight$totalP   + available.phosphorous*indexWeight$availP + slowly.available.k*indexWeight$availK + available.potassium*indexWeight$availK2 
IFI <- with(indexWeight,(organic.matter*organic + total.nitrogen*totalN  + available.nitrogen*availN + total.phosphorus*totalP + available.phosphorous*availP + slowly.available.k*availK + available.potassium*availK2))
IFI






















