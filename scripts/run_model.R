# ============================================================
# 土壤肥料综合评价
# Version: 1.0.0  
# Date  ：2026-09-27   
# Author: Guoqiang Li
# ============================================================

rm(list = ls())
## 设置工作目录 -------------------------------------------------
# 将工作目录切换至项目根目录，确保后续相对路径可正确定位

setwd("E:/GitHub/SoilFertilityEvaluation/")
source("R/fun_main.R")    # 加载主程序


data_file <- "data/SoilFertilityData.csv"
output_file <- "output/SoilResult.csv"
## 自定义参数运行评价流程 ---------------------------------------
# 参数说明：
#   data_file     : 输入数据文件名（CSV 格式，含各评价指标）
#   output_stats  : 是否输出描述性统计结果
#   output_weight : 是否输出各指标权重
#   export_result : 是否将评价结果导出为文件
#   output_file   : 评价结果输出文件名
result <- fun_main (
  data_file     = data_file,
  output_stats  = TRUE,
  output_weight = TRUE,
  export_result = TRUE,
  output_file   = output_file
)
