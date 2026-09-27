# 程序名称：土壤肥力综合评价初步研究 算法
# 版本：V3.1，2026.9.26修订（修复边界与权重逻辑问题）
# 作者：Guoqiang Li @ HNAAS
# E-Mail: hnagri@qq.com
# 说明：算法摘自“土壤肥力综合评价初步研究”，浙江大学学报，1999，25（4）：378-382
#       本次修订：1. 隶属函数增加参数合法性检查并向量化
#                 2. 权重计算取相关系数绝对值，避免负权重
#                 3. 增加变量数与常数列检查

## 隶属函数定义（戒上型）
fun_Membership <- function(x, xmin, xmax) {
  if (!is.numeric(x) || !is.numeric(xmin) || !is.numeric(xmax)) {
    stop("x, xmin, xmax 必须为数值型")
  }
  if (length(xmin) != 1 || length(xmax) != 1) {
    stop("xmin 和 xmax 必须为标量")
  }
  if (xmax <= xmin) {
    stop("xmax 必须大于 xmin，否则会出现除零错误")
  }
  
  # 向量化计算，边界连续
  # x >= xmax → 1
  # xmin <= x < xmax → 0.9*(x-xmin)/(xmax-xmin) + 0.1
  # x < xmin → 0.1
  result <- ifelse(x >= xmax, 1,
                   ifelse(x < xmin, 0.1,
                          0.9 * (x - xmin) / (xmax - xmin) + 0.1))
  return(result)
}

# 单项肥力权重确定（相关系数法）
fun_Weight <- function(dataForCor) {
  if (!is.data.frame(dataForCor) && !is.matrix(dataForCor)) {
    stop("dataForCor 必须为 data.frame 或 matrix")
  }
  
  # 转为 data.frame 并检查数值列
  mydata.cor <- as.data.frame(dataForCor)
  if (!all(sapply(mydata.cor, is.numeric))) {
    stop("所有列必须为数值型")
  }
  
  n <- ncol(mydata.cor)
  if (n < 2) {
    stop("至少需要 2 个变量才能计算相关系数权重")
  }
  
  # 检查常数列（方差为 0 会导致 cor 产生 NA）
  vars <- apply(mydata.cor, 2, var, na.rm = TRUE)
  if (any(vars == 0 | is.na(vars))) {
    const_cols <- names(mydata.cor)[vars == 0 | is.na(vars)]
    stop(paste("存在常数列或全NA列，无法计算相关：", paste(const_cols, collapse = ", ")))
  }
  
  # 相关系数矩阵（取绝对值，避免负权重）
  m.cor <- abs(cor(mydata.cor, use = "pairwise.complete.obs"))
  
  # 每行平均相关系数（排除自相关）
  cor.sum <- apply(m.cor, 1, sum)
  cor.mean <- (cor.sum - 1) / (n - 1)
  
  # 归一化权重（百分比）
  weight <- cor.mean / sum(cor.mean) * 100
  
  # 返回带列名的单行 data.frame
  indexWeight <- as.data.frame(t(weight))
  colnames(indexWeight) <- colnames(mydata.cor)
  rownames(indexWeight) <- "weight"
  
  return(indexWeight)
}