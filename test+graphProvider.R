library(tidyverse)
library(ggplot2)
library(cluster)
library(lubridate)
library(naniar)
library(corrplot)

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
convert_binary <- function(data) {
  # 遍历每一列
  for(col_name in names(data)) {
    if(is.character(data[[col_name]]) || is.factor(data[[col_name]])) {
      unique_vals <- unique(data[[col_name]])
      if(length(unique_vals) == 2 && all(sort(unique_vals) == c("否", "是"))) {
        data[[col_name]] <- factor(data[[col_name]], levels = c("是", "否"))
        cat("已将列", col_name, "转换为因子类型\n")
      }
    }
  }
  return(data)
}
data <- convert_binary(data)

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

###依照play_count数据，将原本数据集分为三类：low，medium，high，方便之后对其中数据进行分析，得到哪些关键数据的高值/低值使得其进入不同类数据集
summary(data$play_count)
data$log_play_count = log(data$play_count)
kmeans_function <- function(data, col_names, k) {
  # 检查哪些列存在
  existing_cols <- col_names[col_names %in% names(data)]
  missing_cols <- col_names[!col_names %in% names(data)]
  
  if(length(missing_cols) > 0) {
    warning("以下列不存在于数据中: ", paste(missing_cols, collapse = ", "))
  }
  
  if(length(existing_cols) > 0) {
    ###对数化处理播放量的长尾特性
    log_col <- log(data[[col_names]] + 1)
    ###k-聚类算法应用
    kmeans_result <- kmeans(log_col, centers = k)
    print(kmeans_result)
    ###寻找聚类点中心，从而完成划分
    cluster_centers <- kmeans_result$centers
    print(cluster_centers)
    sorted_centers <- sort(cluster_centers)
    ###分为low，medium和high三类
    low_cluster <- which(cluster_centers == sorted_centers[1])
    medium_cluster <- which(cluster_centers == sorted_centers[2])
    high_cluster <- which(cluster_centers == sorted_centers[3])
    data$cluster <- kmeans_result$cluster
    low_play_data <- data[data$cluster == low_cluster, ]
    medium_play_data <- data[data$cluster == medium_cluster, ]
    high_play_data <- data[data$cluster == high_cluster, ]
    write.csv(low_play_data, "low_play_data.csv")
    write.csv(medium_play_data, "medium_play_data.csv")
    write.csv(high_play_data, "high_play_data.csv")
  }
  
  return(list(low = low_play_data, medium = medium_play_data, high = high_play_data, kmeans_result = kmeans_result))
}

result <- kmeans_function(data,"play_count", k = 3)
###注意到解释方差比例达到了81.3%，故而可以认为这个聚类算法十分有效
###同时，其按照播放量划分出按照播放量低中高的三个数据包

###根据不同数据量的包来提取热词

###导出处理后的数据
write_csv(data, "data_processed.csv")

