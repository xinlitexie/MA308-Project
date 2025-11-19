library(tidyverse)
library(lubridate)
library(ggplot2)
library(corrplot)
library(naniar)

###读取数据
data <- read_csv("网易云音乐歌单分析/data.csv")

###获取数据基础信息
names(data)
str(data)
cat("数据维度：", dim(data), "\n")
colSums(is.na(data))
gg_miss_var(data) + labs(title = "变量缺失值分布")

###填充缺失值，因为只有introduction部分有缺失值，所以用"[无简介]"替代原本null内容，在后续文本分析过程中，检测热词可以来判断无简介是否也可以受到欢迎。
data$introduction[is.na(data$introduction) | data$introduction == ""] <- "[无简介]"

###转换分类变量为因子，将identity中四种身份分开为四列，以factor类型确定其是否为无、达人、认证、音乐人
split_identity_semicolon <- function(data, identity_col = "identity") {
  data %>%
    mutate(
      # 确保账号身份是字符型
      !!identity_col := as.character(!!sym(identity_col)),
      
      # 创建三个新的二分类变量 - 使用分号分隔
      无 = ifelse(str_detect(!!sym(identity_col), "无"), "是", "否"),
      达人 = ifelse(str_detect(!!sym(identity_col), "达人"), "是", "否"),
      认证 = ifelse(str_detect(!!sym(identity_col), "认证"), "是", "否"),
      音乐人 = ifelse(str_detect(!!sym(identity_col), "音乐人"), "是", "否"),
      
      # 转换为因子类型
      达人 = factor(达人, levels = c("是", "否")),
      认证 = factor(认证, levels = c("是", "否")),
      音乐人 = factor(音乐人, levels = c("是", "否"))
    )
}
data <- split_identity_semicolon(data, "identity")

###将所有只由是否组成的数据转变为factor类型
# 更精确地识别只包含"是"和"否"的列
convert_strict_binary <- function(data) {
  # 遍历每一列
  for(col_name in names(data)) {
    # 检查列是否为字符型或因子型
    if(is.character(data[[col_name]]) || is.factor(data[[col_name]])) {
      # 获取唯一值（排除NA）
      unique_vals <- unique(na.omit(data[[col_name]]))
      
      # 严格检查是否只包含"是"和"否"
      if(length(unique_vals) == 2 && 
         all(sort(unique_vals) == c("否", "是"))) {
        
        # 转换为因子，确保水平顺序为"是","否"
        data[[col_name]] <- factor(data[[col_name]], levels = c("是", "否"))
        
        cat("已将列", col_name, "转换为因子类型\n")
      }
    }
  }
  return(data)
}
data <- convert_strict_binary(data)

###删除处理后的列
remove_columns_tidy <- function(data, col_names) {
  # 检查哪些列存在
  existing_cols <- col_names[col_names %in% names(data)]
  missing_cols <- col_names[!col_names %in% names(data)]
  
  if(length(missing_cols) > 0) {
    warning("以下列不存在于数据中: ", paste(missing_cols, collapse = ", "))
  }
  
  if(length(existing_cols) > 0) {
    # 使用select和减号删除列
    data <- data %>% select(-all_of(existing_cols))
    cat("已删除列:", paste(existing_cols, collapse = ", "), "\n")
  }
  
  return(data)
}
delete_col_names = c("identity")
data <- remove_columns_tidy(data, delete_col_names)

###导出处理后的数据
write_csv(data, "data_processed.csv")
