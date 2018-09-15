# 程序名称：土壤肥力综合评价初步研究 算法
# 版本：V3.0，2017.9.5修订 
# 作者：Guoqiang Li
# E-Mail: agri521#gmail.com
# 说明：算法摘自“土壤肥力综合评价初步研究”，浙江大学学报，1999，25（4）：378-382

## 隶属函数定义
fun_Membership <- function(x,xmin,xmax){
  result <- rep(0,length(x))
  for( i in 1:length(x)){
    if ( x[i] < xmax && x[i] >=xmin ){
      result[i] <- 0.9*(x[i] - xmin)/(xmax - xmin)+0.1
    }
    else if(x[i] >= xmax){
      result[i] <- 1
    }
    else if(x[i] < xmin){
      result[i] <- 0.1
    }
  }
  return(result)
}

# 单项肥力权重确定
fun_Weight <- function(dataForCor){
  mydata.cor <- dataForCor
  # 相关系数
  m.cor <- cor(mydata.cor)
  ## m.cor是矩阵
  ## 相关系数平均数
  cor.sum <- apply(m.cor,1,sum)
  cor.mean <- (cor.sum-1)/(nrow(m.cor)-1)
  
  # 权重
  ## 数据转置后，转换为data.frame
  indexWeight <- as.data.frame(t(cor.mean/sum(cor.mean)*100))
  return(indexWeight)
}