# 程序名称：土壤肥力综合评价初步研究 算法
# 版本：V1.0，2011.1.6修订
# 作者：Guoqiang Li
# E-Mail: agri521#gmail.com
# 说明：算法摘自“土壤肥力综合评价初步研究”，浙江大学学报，1999，25（4）：378-382

# 读取数据，保存到mydata
mydata <- read.csv(textConnection("id,x1,x2,x3,x4,x5,x6,x7
                                  2-01,15.8,1,118,0.4,2,280,104
                                  2-02,11.6,0.82,92,0.27,1,277,102
                                  2-03,16.1,1.08,124,0.42,17,418,245
                                  2-04,6.8,0.56,61,0.2,4,319,165
                                  2-06,16.3,0.98,98,0.31,16,691,313
                                  2-07,10.7,0.69,88,0.26,9,259,76
                                  2-08,12.2,0.78,93,0.28,26,403,174
                                  2-09,14.8,0.94,93,0.36,5,256,68
                                  2-10,13.4,0.85,94,0.21,4,364,160
                                  2-11,13.1,0.78,87,0.25,11,444,244
                                  2-12,12.2,0.75,74,0.3,20,194,59
                                  2-14,13.3,0.77,79,0.23,2,378,202
                                  2-15,18.9,1.16,97,0.45,23,414,190
                                  2-17,13.9,0.96,131,0.38,19,651,316
                                  2-18,17.9,1.09,123,0.28,4,287,141
                                  2-19,8.6,0.57,73,0.35,13,243,96
                                  3-01,19.8,1.17,151,0.28,2,235,101
                                  3-02,15.9,0.87,135,0.35,18,560,255
                                  3-03,27.2,1.56,177,0.34,5,359,126
                                  3-04,20.1,1.16,191,0.33,13,306,75
                                  3-05,11.5,0.7,120,0.28,26,348,198
                                  3-06,11.8,0.76,156,0.28,22,248,80
                                  3-07,23.7,1.31,131,0.4,47,412,143
                                  3-08,13.14,0.75,96,0.22,1,202,57
                                  3-09,23.2,1.34,166,0.4,10,373,146
                                  3-10,19.4,1.08,119,0.36,6,210,40
                                  3-11,16.1,0.92,126,0.32,10,270,84
                                  3-12,13,0.76,96,0.34,5,280,139
                                  3-13,13.1,0.98,330,0.28,19,331,168
                                  3-15,19,1.15,128,0.3,2,293,82
                                  3-16,6.6,0.4,59,0.14,8,123,34
                                  3-17,12.6,0.77,155,0.33,42,380,204
                                  3-18,17.1,0.98,130,0.35,7,320,144
                                  3-19,8.3,0.63,77,0.27,5,368,57
                                  3-20,11.6,0.66,139,0.27,7,295,68
                                  3-21,15.1,0.93,166,0.3,19,220,72
                                  3-22,9.7,0.5,71,0.32,5,155,59
                                  3-23,9.1,0.51,70,0.33,20,124,51"),sep=",")

## 肥力指标划分和评价值计算
# 为各土壤养分建立相应的隶属度函数，计算其隶属值
# 均采用S型作物效应曲线，根据文献获得转折点取值(a,b)

## 有机质
x <- mydata$x1
a <- 10   
b <- 20
# 定义隶属值向量
organic.matter <- rep(0,length(x))

for( i in 1:length(x)){
  if ( x[i] < b && x[i] >=a ){
    organic.matter[i] <- 0.9*(x[i] - a)/(b - a)+0.1
  }
  if(x[i] >= b){
    organic.matter[i] <- 1
  }
  if(x[i] < a){
    organic.matter[i] <- 0.1
  }
}

## 全氮
x <- mydata$x2
a <- 0.5   
b <- 1.5
total.nitrogen <- rep(0,length(x))

for( i in 1:length(x)){
  if ( x[i] < b && x[i] >=a ){
    total.nitrogen[i] <- 0.9*(x[i] - a)/(b - a)+0.1
  }
  if(x[i] >= b){
    total.nitrogen[i] <- 1
  }
  if(x[i] < a){
    total.nitrogen[i] <- 0.1
  }
}

## 碱解氮
x <- mydata$x3
a <- 50   
b <- 150
available.nitrogen <- rep(0,length(x))

for( i in 1:length(x)){
  if ( x[i] < b && x[i] >=a ){
    available.nitrogen[i] <- 0.9*(x[i] - a)/(b - a)+0.1
  }
  if(x[i] >= b){
    available.nitrogen[i] <- 1
  }
  if(x[i] < a){
    available.nitrogen[i] <- 0.1
  }
}

## 全磷
x <- mydata$x4
a <- 0.2 
b <- 0.45
total.phosphorus <- rep(0,length(x))

for( i in 1:length(x)){
  if ( x[i] < b && x[i] >=a ){
    total.phosphorus[i] <- 0.9*(x[i] - a)/(b - a)+0.1
  }
  if(x[i] >= b){
    total.phosphorus[i] <- 1
  }
  if(x[i] < a){
    total.phosphorus[i] <- 0.1
  }
}

## 速效磷
x <- mydata$x5
a <- 3 
b <- 20
available.phosphorous <- rep(0,length(x))

for( i in 1:length(x)){
  if ( x[i] < b && x[i] >=a ){
    available.phosphorous[i] <- 0.9*(x[i] - a)/(b - a)+0.1
  }
  if(x[i] >= b){
    available.phosphorous[i] <- 1
  }
  if(x[i] < a){
    available.phosphorous[i] <- 0.1
  }
}

## 缓效钾
x <- mydata$x6
a <- 150
b <- 500
slowly.available.k <- rep(0,length(x))

for( i in 1:length(x)){
  if ( x[i] < b && x[i] >=a ){
    slowly.available.k[i] <- 0.9*(x[i] - a)/(b - a)+0.1
  }
  if(x[i] >= b){
    slowly.available.k[i] <- 1
  }
  if(x[i] < a){
    slowly.available.k[i] <- 0.1
  }
}

## 速效钾
x <- mydata$x7
a <- 50
b <- 180
available.potassium <- rep(0,length(x))

for( i in 1:length(x)){
  if ( x[i] < b && x[i] >=a ){
    available.potassium[i] <- 0.9*(x[i] - a)/(b - a)+0.1
  }
  if(x[i] >= b){
    available.potassium[i] <- 1
  }
  if(x[i] < a){
    available.potassium[i] <- 0.1
  }
}

# 单项指标权重的确定
# 求单项肥力指标之间的相关系数，然后计算单项肥力相关系数/所有肥力指标相关系数均值

mydata.cor <- mydata[,-1]

# 相关系数
m.cor <- cor(mydata.cor)
m.cor <- as.data.frame(m.cor)
corpar.mean <- (sapply(m.cor,sum)-1)/(length(m.cor)-1)
corpar <- as.vector(corpar.mean)
# 权重
x1.weightvalue <- corpar[1]/sum(corpar)*100
x2.weightvalue <- corpar[2]/sum(corpar)*100
x3.weightvalue <- corpar[3]/sum(corpar)*100
x4.weightvalue <- corpar[4]/sum(corpar)*100
x5.weightvalue <- corpar[5]/sum(corpar)*100
x6.weightvalue <- corpar[6]/sum(corpar)*100
x7.weightvalue <- corpar[7]/sum(corpar)*100

# 土壤养分肥力综合指标计算

IFI <- rep(0,length(mydata$x1))
for(i in 1:length(mydata$x1)){
  IFI[i] <- organic.matter[i]*x1.weightvalue + total.nitrogen[i]*x2.weightvalue + available.nitrogen[i]*x3.weightvalue + total.phosphorus[i]*x4.weightvalue + available.phosphorous[i]*x5.weightvalue + slowly.available.k[i]*x6.weightvalue + available.potassium[i]*x7.weightvalue 
}