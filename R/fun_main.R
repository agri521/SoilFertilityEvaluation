# ============================================================
# 土壤肥料综合评价
# Sesame Climate Suitability Model
#
# Project: Sesame_climate_suitibility
# Version: 1.0.0  
# Date  ：2026-09-27   
# Author: Guoqiang Li
# ============================================================


# ------------------------------------------------------------
# 1. 项目初始化
# ------------------------------------------------------------

# ------------------------------------------------------------
# 2. 加载程序模块
# ------------------------------------------------------------
source("R/fun_membership.R")    # 加载自定义函数


# ------------------------------------------------------------
# 3. 读取模型参数
# ------------------------------------------------------------
source("config/config.R")              # 加载配置文件



# ------------------------------------------------------------------------------
# 2. 主函数：run_evaluation
# ------------------------------------------------------------------------------

#' 土壤肥力综合评价主函数
#'
#' @param data_file 输入数据文件路径（CSV）
#' @param membership_params 隶属函数参数列表
#' @param output_stats 是否输出描述性统计（默认 TRUE）
#' @param output_weight 是否输出权重结果（默认 TRUE）
#' @param export_result 是否导出结果到 CSV（默认 TRUE）
#' @param output_file 输出文件路径
#' @return 包含综合指数和中间结果的数据框
#'
#' @examples
#' result <- run_evaluation(
#'   data_file = "SoilFertilityZhejiang.csv",
#'   membership_params = MEMBERSHIP_PARAMS
#' )

fun_main <- function(
    data_file = CONFIG_DATA_FILE,
    membership_params = MEMBERSHIP_PARAMS,
    output_stats = CONFIG_OUTPUT_STATS,
    output_weight = CONFIG_OUTPUT_WEIGHT,
    export_result = CONFIG_EXPORT_RESULT,
    output_file = CONFIG_OUTPUT_FILE
) {
  
  # ==========================================================================
  # 步骤 1: 数据读取与验证
  # ==========================================================================
  
  message("[1/5] 读取数据文件：", data_file)
  
  # 检查文件是否存在
  if (!file.exists(data_file)) {
    stop("错误：数据文件不存在 - ", data_file)
  }
  
  # 读取数据
  mydata <- read.csv(
    file = data_file,
    sep = ",",
    header = TRUE,
    skip = 0,
    stringsAsFactors = FALSE
  )
  
  # 验证必要列是否存在
  required_cols <- names(membership_params)
  missing_cols <- setdiff(required_cols, names(mydata))
  
  if (length(missing_cols) > 0) {
    stop("错误：数据文件缺少以下列：", paste(missing_cols, collapse = ", "))
  }
  
  message("    成功读取 ", nrow(mydata), " 条记录，", ncol(mydata), " 个字段")
  
  # ==========================================================================
  # 步骤 2: 描述性统计分析
  # ==========================================================================
  
  message("[2/5] 计算描述性统计...")
  
  # 提取肥力指标列（排除第 1 列）
  fertility_data <- mydata[, -1]
  
  # 计算统计量
  stats_result <- data.frame(
    Mean = unlist(lapply(fertility_data, mean)),
    Min  = unlist(lapply(fertility_data, min)),
    Max  = unlist(lapply(fertility_data, max)),
    SD   = unlist(lapply(fertility_data, sd)),
    CV   = unlist(lapply(fertility_data, function(x) sd(x) / mean(x) * 100))
  )
  
  # 输出统计结果
  if (output_stats) {
    message("\n=== 土壤肥力指标描述性统计 ===")
    print(round(stats_result, 4))
  }
  
  # ==========================================================================
  # 步骤 3: 计算各指标隶属度
  # ==========================================================================
  
  message("[3/5] 计算隶属度...")
  
  # 存储隶属度结果
  membership_results <- list()
  
  # 遍历每个指标计算隶属度
  for (col_name in names(membership_params)) {
    param <- membership_params[[col_name]]
    
    membership_results[[col_name]] <- fun_Membership(
      x = mydata[[col_name]],
      xmin = param$xmin,
      xmax = param$xmax
    )
    
    message(sprintf(
      "    %-10s: xmin=%-6g, xmax=%-6g (%s)",
      param$name, param$xmin, param$xmax, param$unit
    ))
  }
  
  # ==========================================================================
  # 步骤 4: 计算指标权重
  # ==========================================================================
  
  message("[4/5] 计算指标权重...")
  
  indexWeight <- fun_Weight(fertility_data)
  
  # 输出权重结果
  if (output_weight) {
    message("\n=== 单项肥力指标权重 ===")
    print(round(indexWeight, 4))
  }
  
  # ==========================================================================
  # 步骤 5: 计算综合肥力指数 (IFI)
  # ==========================================================================
  
  message("[5/5] 计算综合肥力指数...")
  
  # 构建 IFI 计算公式
  ifi_formula <- paste0(
    "membership_results$", names(membership_params), " * indexWeight$", names(membership_params),
    collapse = " + "
  )
  
  # 计算 IFI
  IFI <- eval(parse(text = ifi_formula))
  
  # 输出 IFI 统计
  message("\n=== 土壤肥力综合指数 (IFI) ===")
  message(sprintf("    样本数：  %d", length(IFI)))
  message(sprintf("    平均值：  %.4f", mean(IFI)))
  message(sprintf("    最小值：  %.4f", min(IFI)))
  message(sprintf("    最大值：  %.4f", max(IFI)))
  message(sprintf("    标准差：  %.4f", sd(IFI)))
  
  # ==========================================================================
  # 结果整合与导出
  # ==========================================================================
  
  # 将隶属度和 IFI 添加到原始数据
  result_data <- mydata
  
  # 添加隶属度列
  for (col_name in names(membership_params)) {
    new_col_name <- paste0(col_name, "_membership")
    result_data[[new_col_name]] <- membership_results[[col_name]]
  }
  
  # 添加综合指数列
  result_data$IFI <- IFI
  
  # 添加肥力等级评价（可选）
  result_data$IFI_Level <- cut(
    result_data$IFI,
    breaks = IFI_LEVELS$breaks,
    labels = IFI_LEVELS$labels,
    right = TRUE
  )
  
  # 导出结果
  if (export_result) {
    message("\n导出结果到：", output_file)
    write.csv(result_data, file = output_file, row.names = FALSE, fileEncoding = "UTF-8")
    message("    导出完成！")
  }
  
  # 返回结果
  message("\n=== 评价完成 ===")
  
  return(list(
    data = result_data,           # 完整结果数据框
    statistics = stats_result,    # 描述性统计
    weights = indexWeight,        # 指标权重
    membership = membership_results,  # 各指标隶属度
    IFI = IFI                     # 综合指数向量
  ))
}



# ==============================================================================
# 结束
# ==============================================================================