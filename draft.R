library(tidyverse)
library(ggplot2)
library(cluster)
library(lubridate)
library(naniar)
library(corrplot)
library(jiebaR)
library(wordcloud)
library(scatterplot3d)
library(MASS)
library(scales)
library(dplyr)
library(stringr)
library(randomForest)
library(broom)
library(car)
library(moments)
library(nnet)
library(caret)
library(reshape2)

data <- read_csv("网易云音乐歌单分析/data.csv")

# ==== 1、清洗数据 ====

###获取数据基础信息
names(data)
str(data)
cat("数据维度：", dim(data), "\n")
colSums(is.na(data))
gg_miss_var(data) + labs(title = "变量缺失值分布")

###填充缺失值，因为只有introduction部分有缺失值，所以用"[无简介]"替代原本null内容，在后续文本分析过程中，检测热词可以来判断无简介是否也可以受到欢迎。
data$introduction[is.na(data$introduction) | data$introduction == ""] <- "[无简介]"

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

data_cleaned = data
# ==== 2、描述性分布和热图 ====

### 提取numeric和factor变量，并进行简单数据统计分析
numeric_vars <- names(data_cleaned)[sapply(data_cleaned, is.numeric)]
length(numeric_vars)
factor_vars <- names(data_cleaned)[sapply(data_cleaned, is.factor)]
length(factor_vars)
desc_stats <- (data_cleaned[, numeric_vars])
summary(desc_stats)
###绘制统计数据图片
par(mfrow = c(2, 1), mar = c(2, 3, 1, 2) + 0.1)

for (var in numeric_vars) {
  current_data = data_cleaned[[var]]
  kernels <- c("rectangular", "triangular", "epanechnikov", "gaussian")
  colors <- c("red", "blue", "green", "yellow")
  hist(current_data, freq = FALSE, main = paste("直方图与核密度估计:", var), xlab = var, ylab = "密度", col = "lightgray", border = "black", ylim = c(0, max(hist(current_data, plot = FALSE)$density) * 1.3))
  
  bandwidths = 1
  for (i in seq_along(kernels)) {
    kernel_type <- kernels[i]
    color <- colors[i]
    kde <- density(current_data, kernel = kernel_type, bandwidths = bandwidths, adjust = 1)
    lines(kde, col = color, lty = 1, lwd = 2)
  }
  
  legend("topright", legend = kernels, col = colors, lty = 1, lwd = 2, title = "核函数", bty = "n")
  
  boxplot(current_data, main = paste("箱线图:", var), ylab = var, col = "lightblue")
}
par(mfrow = c(1, 1))

###对于factor变量的基础统计
num_vars <- length(factor_vars)
par(mfrow = c(1, 3), mar = c(4, 4, 3, 2))

for (var in factor_vars) {
  freq_data <- table(data_cleaned[[var]])
  prop_data <- prop.table(freq_data)
  
  categories <- names(freq_data)
  colors <- c("#1f77b4", "#ff7f0e")  # 两个颜色
  
  barplot(freq_data, 
          main = paste("频数分布:", var),
          xlab = var,
          ylab = "频数",
          col = colors[1:length(categories)],
          border = "black",
          ylim = c(0, max(freq_data) * 1.2))
  
  text(x = seq_along(freq_data), 
       y = freq_data,
       label = freq_data,
       pos = 3,  # 在条形上方
       cex = 0.8,
       col = "black")
  
  barplot(prop_data * 100, 
          main = paste("百分比分布:", var),
          xlab = var,
          ylab = "百分比 (%)",
          col = colors[1:length(categories)],
          border = "black",
          ylim = c(0, 100))
  
  text(x = seq_along(prop_data), 
       y = prop_data * 100,
       label = paste0(round(prop_data * 100, 1), "%"),
       pos = 3,
       cex = 0.8,
       col = "black")
  
  pie(freq_data,
      main = paste("饼图:", var),
      col = colors[1:length(categories)],
      labels = paste0(categories, "\n", 
                      freq_data, " (", 
                      round(prop_data * 100, 1), "%)"),
      cex = 0.9)
  
  legend("topright",
         legend = categories,
         fill = colors[1:length(categories)],
         title = paste("水平:", var))
}
par(mfrow = c(1, 1))

###设置中文字体
windowsFonts(SimHei = windowsFont("SimHei"))

###定义停用词列表
stopwords_custom <- c("br", "介绍", "一个", "没有", "可以", "一种", "就是", "因为", "一些", "这个", "这些", "不是", "所以", "什么", "那些")

###清洗文本中所有的标点
text_clean <- function (texts) {
  all_text <- paste(texts, collapse = " ")
  all_text <- str_replace_all(all_text, "[，。]", " ")
  return(all_text)
}

###分析提取热词并绘图，可以先不设置停用词，然后根据出来的结果设置停用词
text_analysis <- function(texts) {
  text_cleaned <- text_clean(texts)
  
  ###初始化分词器
  cutter <- worker()
  words <- cutter[text_cleaned]
  words_filtered <- words[
    nchar(words) > 1 &
      !words %in% stopwords_custom
  ]
  word_freq <- table(words_filtered) %>%
    as.data.frame() %>%
    arrange(desc(Freq)) %>%
    head(50)
  colnames(word_freq) <- c("Word", "Frequency")
  
  ###绘制词云图
  wordcloud(words = word_freq$Word,
            freq = word_freq$Frequency, 
            min.freq = 5,
            max.words = 100,
            random.order = FALSE,
            colors = rainbow(10),
            family = "SimHei")
  
  ###返回高频词以及高频词的频率
  return(word_freq)
}
name_analysis <- text_analysis(data_cleaned$name)
introduction_analysis <- text_analysis(data_cleaned$introduction)


###是否需要log变换
skewness(data_cleaned$play_count)


# ====模块1：探究播放量、收藏量、分享量、评论量以及作者粉丝数之间的线性相关关系。 ====
result_vars <- data_cleaned %>%
  dplyr::select(play_count, collect_count, share_count, comment_count, fans)
# 计算相关系数矩阵
cor_matrix <- cor(result_vars, use = "complete.obs")
# 绘制热力图
corrplot(cor_matrix, 
         method = "color", 
         type = "upper", 
         addCoef.col = "black", # 显示相关系数数值
         tl.col = "black",      # 标签颜色
         tl.srt = 45,           # 标签旋转角度
         title = "Key Metrics Correlation Matrix", 
         mar = c(0,0,1,0))







#====模块2：高价值标签挖掘====
tag_analysis_top <- data_cleaned %>%
  dplyr::select(topics, play_count) %>%
  mutate(topics = str_remove_all(topics, "\\[|\\]|'|\"| ")) %>%
  separate_rows(topics, sep = ",") %>%
  # 按标签分组统计
  group_by(topics) %>%
  summarise(
    avg_play = mean(play_count),
    count = n()
  ) %>%
  # 按平均播放量降序排列
  arrange(desc(avg_play)) %>%
  slice_head(n = 10)

tag_analysis_bottom <- data_cleaned %>%
  dplyr::select(topics, play_count) %>%
  mutate(topics = str_remove_all(topics, "\\[|\\]|'|\"| ")) %>%
  separate_rows(topics, sep = ",") %>%
  # 按标签分组统计
  group_by(topics) %>%
  summarise(
    avg_play = mean(play_count),
    count = n()
  ) %>%
  # 按平均播放量升序排列
  arrange(avg_play) %>%
  slice_head(n = 10)

tag_analysis_avoid_extreme_top <- data_cleaned %>%
  dplyr::select(topics, play_count) %>%
  mutate(topics = str_remove_all(topics, "\\[|\\]|'|\"| ")) %>%
  separate_rows(topics, sep = ",") %>%
  # 按标签分组统计
  group_by(topics) %>%
  summarise(
    avg_play = mean(play_count),
    count = n()
  ) %>%
  filter(count > 10) %>%
  # 按平均播放量降序排列
  arrange(desc(avg_play)) %>%
  slice_head(n = 10)

tag_analysis_avoid_extreme_bottom <- data_cleaned %>%
  dplyr::select(topics, play_count) %>%
  mutate(topics = str_remove_all(topics, "\\[|\\]|'|\"| ")) %>%
  separate_rows(topics, sep = ",") %>%
  # 按标签分组统计
  group_by(topics) %>%
  summarise(
    avg_play = mean(play_count),
    count = n()
  ) %>%
  filter(count > 10) %>%
  # 按平均播放量升序排列
  arrange(avg_play) %>%
  slice_head(n = 10)

tag_freq <- data_cleaned %>%
  dplyr::select(topics, play_count) %>%
  mutate(topics = str_remove_all(topics, "\\[|\\]|'|\"| ")) %>%
  separate_rows(topics, sep = ",") %>%
  # 按标签分组统计
  group_by(topics) %>%
  summarise(
    count = n()
  ) %>%
  arrange(desc(count)) %>%
  slice_head(n = 10)
# 绘图
ggplot(tag_analysis_top, aes(x = reorder(topics, avg_play), y = avg_play)) +
  geom_col(fill = "steelblue") +
  coord_flip() + # 翻转坐标轴便于阅读标签
  labs(
    title = "Top 10 Tags by Average Play Count",
    x = "Tags",
    y = "Average Play Count"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))

ggplot(tag_analysis_bottom, aes(x = reorder(topics, avg_play), y = avg_play)) +
  geom_col(fill = "steelblue") +
  coord_flip() + # 翻转坐标轴便于阅读标签
  labs(
    title = "Bottom 10 Tags by Average Play Count",
    x = "Tags",
    y = "Average Play Count"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))

ggplot(tag_analysis_avoid_extreme_top, aes(x = reorder(topics, avg_play), y = avg_play)) +
  geom_col(fill = "steelblue") +
  coord_flip() + # 翻转坐标轴便于阅读标签
  labs(
    title = "Top 10 Tags by Average Play Count(Happen more than ten times)",
    x = "Tags",
    y = "Average Play Count"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))

ggplot(tag_analysis_avoid_extreme_bottom, aes(x = reorder(topics, avg_play), y = avg_play)) +
  geom_col(fill = "steelblue") +
  coord_flip() + # 翻转坐标轴便于阅读标签
  labs(
    title = "Bottom 10 Tags by Average Play Count(Happen more than ten times)",
    x = "Tags",
    y = "Average Play Count"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))

ggplot(tag_freq, aes(x = reorder(topics, count), y = count)) +
  geom_col(fill = "steelblue") +
  coord_flip() + # 翻转坐标轴便于阅读标签
  labs(
    title = "Tag frequency",
    x = "Tags",
    y = "Top 10 Tags by Tag Frequency"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))



#====模块3：随机森林进一步分析tag impact====
###提取并集标签
all_tags <- unique(c(
  tag_analysis_avoid_extreme_top$topics,
  tag_analysis_avoid_extreme_bottom$topics,
  tag_freq$topics
))

cat("共提取", length(all_tags), "个不重复标签\n")

###清洗topic量
model_data_long <- data_cleaned %>%
  dplyr::select(play_count, topics) %>%
  mutate(
    topics_clean = str_remove_all(topics, "\\[|\\]|'|\"| "),
    log_play = log1p(play_count)
  ) %>%
  separate_rows(topics_clean, sep = ",") %>%
  filter(topics_clean != "")

###获取唯一歌曲记录
unique_songs <- model_data_long %>% distinct(play_count, log_play)
tag_matrix <- matrix(0L, 
                     nrow = nrow(unique_songs), 
                     ncol = length(all_tags))
colnames(tag_matrix) <- paste0("tag_", make.names(all_tags))
for(i in seq_len(nrow(unique_songs))) {
  song_play_count <- unique_songs$play_count[i]
  song_tags <- model_data_long$topics_clean[model_data_long$play_count == song_play_count]
  
  for(tag in song_tags) {
    if(tag %in% all_tags) {
      col_idx <- match(make.names(tag), make.names(all_tags))
      tag_matrix[i, col_idx] <- 1L
    }
  }
}
model_data_wide <- cbind(unique_songs, as.data.frame(tag_matrix))

###线性回归
feature_cols <- colnames(tag_matrix)
formula_str <- paste("log_play ~", paste(feature_cols, collapse = " + "))
model_lm <- lm(as.formula(formula_str), data = model_data_wide)

###提取系数
model_all_tags <- tidy(model_lm, conf.int = TRUE) %>%
  filter(term %in% feature_cols) %>%
  mutate(
    tag_name = all_tags[match(str_remove(term, "tag_"), make.names(all_tags))],
    significance = ifelse(p.value < 0.05, "Significant", "Not Significant")
  )

###绘制森林图
# 绘制基本图
# 添加颜色和透明度
ggplot(model_all_tags, aes(x = estimate, y = reorder(tag_name, estimate), color = ifelse(estimate > 0, "Positive", "Negative"), alpha = significance)) +
  geom_point() +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("Positive" = "#E41A1C", "Negative" = "#377EB8")) +
  scale_alpha_manual(values = c("Significant" = 1.0, "Not Significant" = 0.4)) +
  labs(title = "All Tags Impact on Play Count (Pure Tag Effects)",
       subtitle = "Linear Regression Coefficients with 95% CI | Red = Positive, Blue = Negative",
       x = "Effect Size (Impact on Log Play Count)",
       y = NULL,
       color = "Impact Direction",
       alpha = "Significance") +
  theme_minimal()






#====模块4：检测播放数异常值点和杠杆点====

data_cleaned <- data_cleaned %>%
  # 使用log1p处理0值
  mutate(log_play = log1p(play_count)) %>%
  filter(play_count > 0, collect_count >= 0, share_count >= 0, comment_count >= 0) %>%
  na.omit()

###构建model
diagnostic_formula <- log_play ~ collect_count + share_count + comment_count
diag_model <- lm(diagnostic_formula, data = data_cleaned)
summary(diag_model)

###计算诊断统计量
par(mar = c(6, 6, 5, 3) + 0.1, mgp = c(3.5, 1, 0))
cooks_dist <- cooks.distance(diag_model)
leverage <- hatvalues(diag_model)
std_resid <- rstandard(diag_model)
p_influence <- influencePlot(
  diag_model,
  main = "影响力诊断图 (Influence Plot)",
  sub = "圆圈大小 ∝ Cook's距离 | 红色点需特别关注",
  xlab = "标准化残差 (Standardized Residuals)",
  ylab = "杠杆值 (Leverage)",
  col = c("steelblue", "orange", "red"),
  pch = c(16, 18),
  lwd = 1.5,                 # 减小线宽
  mar = c(5, 5, 4, 2) + 0.1, # 增加边距（下、左、上、右）
  mgp = c(2.5, 0.7, 0),      # 调整坐标轴标签位置
  cex.lab = 1.0,             # 坐标轴标签大小
  cex.main = 1.3,            # 标题大小
  cex.sub = 0.9,             # 副标题大小
  cex.axis = 0.9,            # 坐标轴刻度大小
  las = 1                    # 刻度标签水平（避免重叠）
)
p_influence

threshold_cook <- 4 / nrow(data_cleaned)
threshold_leverage <- 3 * length(coef(diag_model)) / nrow(data_cleaned)

###识别异常值
outlier_mask <- cooks_dist > threshold_cook
leverage_mask <- leverage > threshold_leverage

cat("识别出异常值:", sum(outlier_mask), "个\n")

data_normal <- data_cleaned %>% filter(!outlier_mask) %>% dplyr::select(log_play, play_count)
data_outliers <- data_cleaned %>% filter(outlier_mask) %>% dplyr::select(log_play, play_count) %>% mutate(cluster = NA)








#====模块5：聚类分析（需要去掉异常值点），之后再补充进去====

###提取正常值

data_normal <- data_cleaned %>% filter(!outlier_mask) %>% dplyr::select(log_play, play_count)
data_outliers <- data_cleaned %>% filter(outlier_mask) %>% dplyr::select(log_play, play_count) %>% mutate(cluster = NA)

###肘部算法

set.seed(1234)
wss = numeric(10)
for (i in 1:10) {
  km = kmeans(data_normal$play_count, centers = i, nstart = 25)
  wss[i] = km$tot.withinss
}
plot(1:10, wss, type = "b", xlab = "Number of groups", ylab = "Within groups sum of squares")

kmeans_function <- function(data, col_names, k) {
  # 检查哪些列存在
  existing_cols <- col_names[col_names %in% names(data)]
  missing_cols <- col_names[!col_names %in% names(data)]
  
  if(length(missing_cols) > 0) {
    warning("以下列不存在于数据中: ", paste(missing_cols, collapse = ", "))
  }
  
  if(length(existing_cols) > 0) {
    data_subset <- data[, existing_cols, drop = FALSE]
    
    # 执行k-means聚类
    set.seed(123)  # 设置随机种子保证结果可重复
    kmeans_result <- kmeans(data_subset, centers = k, nstart = 25)
    
    print(kmeans_result)
    cluster_centers <- kmeans_result$centers
    cat("\n聚类中心:\n")
    print(cluster_centers)
    
    # 排序聚类中心（按第一个特征的均值排序）
    sorted_centers <- cluster_centers[order(cluster_centers[, 1]), , drop = FALSE]
    cat("\n按第一个特征排序后的聚类中心:\n")
    print(sorted_centers)
    
    # 将聚类结果添加到原始数据中
    data$cluster <- kmeans_result$cluster
  } else {
    stop("没有找到任何有效的列进行聚类分析。")
  }
  
  return(list(
    kmeans_result = kmeans_result,
    data_with_clusters = data,
    cluster_centers = cluster_centers,
    sorted_centers = sorted_centers
  ))
}

###导出结果
result1 <- kmeans_function(data_normal, "play_count", k = 4)
result2 <- kmeans_function(data_cleaned, "play_count", k = 4)

###可视化操作
data_clustered1 <- result1$data_with_clusters
ggplot(data_clustered1, aes(x = factor(cluster), y = play_count, fill = factor(cluster))) +
  geom_boxplot() +
  labs(title = "不同聚类的 play_count 分布",
       x = "聚类",
       y = "播放次数",
       fill = "聚类") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()

data_clustered2 <- result2$data_with_clusters
ggplot(data_clustered2, aes(x = factor(cluster), y = play_count, fill = factor(cluster))) +
  geom_boxplot() +
  labs(title = "不同聚类的 play_count 分布",
       x = "聚类",
       y = "播放次数",
       fill = "聚类") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()
###注意到解释方差比例达到了89.53%，故而可以认为这个聚类算法十分有效
explained_variance <- result1$kmeans_result$betweenss / result1$kmeans_result$totss
cat("解释方差比例: ", round(explained_variance * 100, 2), "%\n")



# 计算每个聚类的边界范围
calculate_cluster_bounds <- function(data_with_clusters, col_name) {
  # 检查列是否存在
  if (!col_name %in% names(data_with_clusters)) {
    stop("指定的列名不存在于数据中。")
  }
  
  # 提取聚类标签和数据列
  clusters <- unique(data_with_clusters$cluster)
  bounds <- data.frame(cluster = integer(), min_value = numeric(), max_value = numeric())
  
  for (cluster in clusters) {
    cluster_data <- data_with_clusters[data_with_clusters$cluster == cluster, ]
    min_value <- min(cluster_data[[col_name]])
    max_value <- max(cluster_data[[col_name]])
    
    bounds <- rbind(bounds, data.frame(cluster = cluster, min_value = min_value, max_value = max_value))
  }
  
  return(bounds)
}

# 计算聚类边界范围
cluster_bounds <- calculate_cluster_bounds(data_clustered1, "play_count")

# 输出每个聚类的边界范围
cat("\n=== 每个聚类的边界范围 ===\n")
print(cluster_bounds)




#====模块6：日均播放量(改成kmeans更好，但没写出来就用中位数)====


reference_date <- max(data_cleaned$create_time)

data_velocity <- data_cleaned %>%
  mutate(
    # 计算歌单存活天数
    days_alive = as.numeric(difftime(reference_date, create_time, units = "days")),
    # 计算日均播放量 (Velocity)
    plays_per_day = play_count / days_alive
  ) %>%
  arrange(desc(plays_per_day))

# 看看是谁在“爆发”：取出日均播放量最高的 Top 10
top_velocity <- head(data_velocity %>% dplyr::select(name, plays_per_day, days_alive, fans), 10)
print(top_velocity)

median_days <- median(data_velocity$days_alive)
median_plays <- median(data_velocity$plays_per_day)
data_velocity$quadrant <- with(data_velocity, {
  case_when(
    days_alive <= median_days & plays_per_day > median_plays ~ "左上：新生优秀",
    days_alive <= median_days & plays_per_day <= median_plays ~ "左下：新生不优秀",
    days_alive > median_days & plays_per_day > median_plays ~ "右上：老但优秀",
    days_alive > median_days & plays_per_day <= median_plays ~ "右下：老不优秀"
  ) %>% factor(levels = c("左上：新生优秀", "左下：新生不优秀", "右上：老但优秀", "右下：老不优秀"))
})


ggplot(data_velocity, aes(x = days_alive, y = plays_per_day, color = quadrant)) +
  geom_point(alpha = 0.7, size = 2.5) +
  
  # 分界线
  geom_vline(xintercept = median_days, linetype = "dashed", color = "gray50", size = 1) +
  geom_hline(yintercept = median_plays, linetype = "dashed", color = "gray50", size = 1) +
  
  scale_y_log10(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  
  labs(
    title = "歌单Velocity四象限分析",
    subtitle = paste("分界线: 天数=", round(median_days), "天, 播放量=", round(median_plays)),
    x = "存活天数", y = "日均播放量",
    color = "象限类型"
  ) +
  
  scale_color_manual(values = c(
    "左上：新生优秀" = "#2ECC71",
    "左下：新生不优秀" = "#E74C3C",
    "右上：老但优秀" = "#3498DB",
    "右下：老不优秀" = "#95A5A6"
  )) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )









#====模块7：歌单信息的影响====
data_morph <- data_cleaned %>%
  dplyr::select(play_count, length_name, length_intro, number_songs, number_hot_singers) %>%
  filter(play_count > 0) %>%
  na.omit()



###公式：log播放量 ~ 标题长度 + 简介长度 + 歌曲总数 + 热门歌手数
model_morph <- lm(log(play_count) ~ length_name + length_intro + number_songs + number_hot_singers, data = data_morph)
print("--- Regression Summary: Length & Size Effects ---")
print(summary(model_morph))
stepAIC(model_morph, direction = "backward")
model_morph_changed <- lm(log(play_count) ~ length_name + length_intro + number_hot_singers, data = data_morph)
anova(model_morph_changed, model_morph)





#====模块8：作者信息的影响====
data_author <- data_cleaned %>%
  dplyr::select(play_count, fans, grade, playlists, talent, verification, musician) %>%
  filter(play_count > 0) %>%
  na.omit()


boxcox(lm(play_count ~ fans * grade * playlists + talent * verification * musician, data = data_author), 
       lambda = seq(-2, 2, length.out = 100))

model_full <- lm(
  log(play_count) ~ fans * grade * playlists + talent * verification * musician,
  data = data_author
)

summary(model_full)
model_changed <- stepAIC(model_full, direction = "backward")

summary(model_changed)
anova(model_changed, model_full)



###模块9：身份的作用(用permutation test来做检测)
data_id <- data_cleaned %>%
  mutate(
    is_talent = ifelse(talent == "是", 1, 0),
    is_verified = ifelse(verification == "是", 1, 0),
    is_musician = ifelse(musician == "是", 1, 0)
  )

data_id <- data_id %>%
  mutate(
    identity_4level = case_when(
      is_verified == 1 ~ "Verification",
      is_talent == 1 ~ "Talent",
      is_musician == 1 ~ "Musician",
      TRUE ~ "None"
    ) %>% factor(levels = c("None", "Talent", "Musician", "Verification"))
  )

comparison_pairs <- list(
  c("None", "Talent"),
  c("None", "Musician"),
  c("None", "Verification"),
  c("Musician", "Talent"),
  c("Talent", "Verification"),
  c("Musician", "Verification")
)

# H0: group2均值 = group1均值
# H1: group2均值 > group1均值（单侧）
run_permutation_test <- function(data, group_col, value_col, pair, n_perm = 10000) {
  group1_data <- data[[value_col]][data[[group_col]] == pair[1]] %>% na.omit()
  group2_data <- data[[value_col]][data[[group_col]] == pair[2]] %>% na.omit()
  
  # 检查样本量
  if(length(group1_data) < 2 | length(group2_data) < 2) {
    return(NULL)
  }
  
  # 观察到的均值差异
  observed_diff <- mean(group2_data) - mean(group1_data)
  
  # 合并数据
  combined <- c(group1_data, group2_data)
  n2 <- length(group2_data)
  
  # 置换：随机打乱分组标签，计算均值差异
  perm_diffs <- replicate(n_perm, {
    perm_group2 <- sample(combined, size = n2)
    perm_group1 <- sample(combined, size = length(group1_data))
    mean(perm_group2) - mean(perm_group1)
  })
  
  # 单侧p值：perm_diffs ≥ observed_diff的比例
  p_value <- mean(perm_diffs >= observed_diff)
  
  # Bootstrap CI（稳健标准误）
  boot_diffs <- replicate(2000, {
    mean(sample(group2_data, replace = TRUE)) - mean(sample(group1_data, replace = TRUE))
  })
  
  list(
    comparison = paste(pair[2], "vs", pair[1]),
    mean_diff = observed_diff,
    p_value = p_value,
    ci_lower = quantile(boot_diffs, 0.025),
    ci_upper = quantile(boot_diffs, 0.975),
    group1_n = length(group1_data),
    group2_n = length(group2_data)
  )
}

results <- lapply(comparison_pairs, function(pair) {
  run_permutation_test(data_id, "identity_4level", "play_count", pair)
}) %>% bind_rows()

results_final <- results %>%
  mutate(
    p_adj = p.adjust(p_value, method = "bonferroni"),
    sig = case_when(
      p_adj < 0.001 ~ "***",
      p_adj < 0.01 ~ "**",
      p_adj < 0.05 ~ "*",
      TRUE ~ "ns"
    )
  )

cat("=== 置换检验结果（均值差异 + Bonferroni校正）===\n")
print(results_final)

ggplot(results_final, aes(x = comparison, y = mean_diff, fill = sig)) +
  geom_col(alpha = 0.8) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_text(aes(label = sig), color = "red", size = 5, vjust = -1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_fill_manual(values = c("***" = "red", "**" = "red", "*" = "red", "ns" = "gray80")) +
  labs(
    title = "身份价值差异：置换检验（Permutation Test）",
    subtitle = "均值差异 + 95% Bootstrap CI | 红色=显著",
    x = "比较组",
    y = "均值差异",
    fill = "显著性"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###???
identity_stats <- data.frame(
  Identity = c("Verification (认证用户)", "Talent (达人)", "Musician (音乐人)", "none(无)"),
  Avg_Play = c(
    mean(data_id$play_count[data_id$is_verified == 1], na.rm=TRUE),
    mean(data_id$play_count[data_id$is_talent == 1], na.rm=TRUE),
    mean(data_id$play_count[data_id$is_musician == 1], na.rm=TRUE),
    mean(data_id$play_count[data_id$is_musician == 0 & data_id$is_verified == 0 & data_id$is_talent == 0], na.rm=TRUE)
  )
)


ggplot(identity_stats, aes(x = reorder(Identity, -Avg_Play), y = Avg_Play, fill = Identity)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = comma(Avg_Play, accuracy = 1)), vjust = -0.5, size = 4) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = c("black", "gray50", "#E74C3C", "#F1C40F")) +
  labs(
    title = "创作者身份价值评估 (Identity ROI)",
    x = "身份类型",
    y = "平均播放量"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14))












#====模块9：时序分析====
###最佳发布时间分析

# 1. 数据准备
data_time_week <- data_cleaned %>%
  mutate(
    create_dt = as_datetime(create_time),
    # 将周几转化为有序因子 (周一到周日)
    day_of_week = factor(wday(create_dt, label = TRUE, week_start = 1)) 
  ) %>%
  group_by(day_of_week) %>%
  summarise(avg_play = mean(play_count, na.rm = TRUE))

# 2. 绘图：独立时间柱状图
ggplot(data_time_week, aes(x = day_of_week, y = avg_play, fill = day_of_week)) +
  geom_col(alpha = 0.8) +
  # 在柱子上添加具体数值标签
  geom_text(aes(label = comma(avg_play, accuracy = 1)), vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) + # 让Y轴上方留白
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "C类分析: 不同发布时间的流量差异 (Time Analysis)",
    subtitle = "数据洞察：周日发布的歌单平均播放量最高 (周末效应)",
    x = "发布时间 (周几)",
    y = "平均播放量 (Average Play Count)"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14))


# 1. 数据准备（修正版）
data_time_month <- data_cleaned %>%
  mutate(
    create_dt = as_datetime(create_time),
    # 修正点1：直接使用 lubridate 的 label=TRUE，它会自动生成有序因子
    # 只要不强制指定 levels = month.abb，就不会有中英文冲突
    month_of_year = month(create_dt, label = TRUE, abbr = TRUE) 
  ) %>%
  # 修正点2：在 group_by 之前最好检查一下是否有 NA，虽然这步不是必须的
  filter(!is.na(month_of_year)) %>%
  group_by(month_of_year) %>%
  summarise(avg_play = mean(play_count, na.rm = TRUE)) %>%
  ungroup()

# 找出播放量最高的月份（用于动态副标题）
top_month_data <- data_time_month %>%
  slice_max(order_by = avg_play, n = 1) 

top_month_name <- top_month_data$month_of_year
top_month_val  <- top_month_data$avg_play

# 2. 绘图（修正版）
ggplot(data_time_month, aes(x = month_of_year, y = avg_play)) +
  # 修正点3：将 fill 映射给 avg_play (数值)，而不是月份 (分类)
  # 这样可以避开 "Blues" 只有9种颜色的限制，且逻辑更佳：颜色越深播放量越高
  geom_col(aes(fill = avg_play), alpha = 0.9) +
  
  # 在柱子上添加具体数值标签
  geom_text(aes(label = comma(avg_play, accuracy = 1)), vjust = -0.5, size = 3.5) +
  
  # 修正点4：使用 distiller 处理连续数值的蓝色渐变 (direction=1 保证数值大颜色深)
  scale_fill_distiller(palette = "Blues", direction = 1) +
  
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  
  labs(
    title = "C类分析: 不同发布月份的流量差异 (Monthly Analysis)",
    subtitle = paste0("数据洞察：", top_month_name, " 发布的歌单平均播放量最高 (", comma(top_month_val), ")"),
    x = "发布时间 (月份)",
    y = "平均播放量 (Average Play Count)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none", # 隐藏图例，因为高度已经代表了数值
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(size = 10) 
  )



# 1. 数据准备（优化版）
data_time_month <- data_cleaned %>%
  mutate(
    create_dt = as_datetime(create_time),
    # 确保月份顺序：先获取数字月份，再转因子
    month_num = month(create_dt),
    month_of_year = factor(month(create_dt, label = TRUE), 
                           levels = month.abb)  # 保证1-12月顺序
  ) %>%
  group_by(month_of_year, month_num) %>%
  summarise(avg_play = mean(play_count, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(month_num)  # 按月份排序

# 找出播放量最高的月份（用于动态副标题）
top_month <- data_time_month %>%
  slice_max(order_by = avg_play, n = 1) %>%
  pull(month_of_year)

# 2. 绘图（优化版）
ggplot(data_time_month, aes(x = month_of_year, y = avg_play, fill = month_of_year)) +
  geom_col(alpha = 0.8) +
  # 在柱子上添加具体数值标签
  geom_text(aes(label = comma(avg_play, accuracy = 1)), vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "C类分析: 不同发布月份的流量差异 (Monthly Analysis)",
    subtitle = paste("数据洞察：", top_month, "发布的歌单平均播放量最高 (季节性效应)"),
    x = "发布时间 (月份)",
    y = "平均播放量 (Average Play Count)"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1))  # 月份标签倾斜，避免重叠







# ====模块10：热词检测 (Text Analysis - Impact on Play Count)====


# 定义一个函数来计算特定词汇列表对播放量的影响
analyze_word_impact <- function(data, text_col, top_words_df, top_n = 20) {
  # 取频数最高的前N个词
  target_words <- head(top_words_df$Word, top_n)
  
  impact_list <- list()
  
  for(word in target_words) {
    has_word <- str_detect(data[[text_col]], fixed(word))
    avg_play <- mean(data$play_count[has_word], na.rm = TRUE)
    count <- sum(has_word, na.rm = TRUE)
    
    ### 当样本量足够时才统计(>5)
    if(count > 5) {
      impact_list[[word]] <- data.frame(
        word = word,
        avg_play = avg_play,
        count = count
      )
    }
  }
  
  result_df <- do.call(rbind, impact_list)
  return(result_df)
}

# 1. 分析歌单名称(name)中的热词影响
# 也就是利用你之前算好的 name_analysis 变量
name_impact <- analyze_word_impact(data_cleaned, "name", name_analysis, top_n = 30)

# 2. 分析简介(introduction)中的热词影响
intro_word_impact <- analyze_word_impact(data_cleaned, "introduction", introduction_analysis, top_n = 30)

# 3. 绘图：展示对播放量提升最大的歌单名热词
if(!is.null(name_impact) && nrow(name_impact) > 0) {
  name_impact_sorted <- name_impact %>% arrange(desc(avg_play)) %>% head(15)
  
  p_name_hot <- ggplot(name_impact_sorted, aes(x = avg_play, y = reorder(word, avg_play))) +
    geom_col(fill = "#E67E22", alpha = 0.8) +
    geom_text(aes(label = comma(avg_play, accuracy = 1)), hjust = -0.1, size = 3) +
    scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.3))) +
    labs(
      title = "Module 11: 歌单标题热词价值分析",
      subtitle = "包含这些词汇的歌单具有更高的平均播放量",
      x = "平均播放量",
      y = "标题关键词"
    ) +
    theme_minimal()
  print(p_name_hot)
}

# 4. 绘图：展示对播放量提升最大的简介热词
if(!is.null(intro_word_impact) && nrow(intro_word_impact) > 0) {
  intro_impact_sorted <- intro_word_impact %>% arrange(desc(avg_play)) %>% head(15)
  
  p_intro_hot <- ggplot(intro_impact_sorted, aes(x = avg_play, y = reorder(word, avg_play))) +
    geom_col(fill = "#16A085", alpha = 0.8) +
    geom_text(aes(label = comma(avg_play, accuracy = 1)), hjust = -0.1, size = 3) +
    scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.3))) +
    labs(
      title = "Module 11: 简介内容热词价值分析",
      subtitle = "简介中包含这些词汇往往意味着更高的流量",
      x = "平均播放量",
      y = "简介关键词"
    ) +
    theme_minimal()
  print(p_intro_hot)
}


# ====模块11：神经网络预测以及error矩阵 (Neural Network Prediction)====



cat("\n=== 开始 Module 12: PCA 与 神经网络预测 ===\n")

# 1. 数据准备：选择用于预测的数值型变量
# 排除掉只有0/1的dummy变量，选择连续变量进行PCA
# 注意：不能把 play_count 放进 PCA
nn_data <- data_cleaned %>%
  dplyr::select(
    play_count, 
    collect_count, share_count, comment_count, # 中间层/强相关变量
    fans, grade, playlists,                # 作者特征
    length_name, length_intro, number_songs, number_hot_singers # 歌单特征
  ) %>%
  na.omit()

# 2. PCA分析 (提取主要特征值)
# 针对作者特征和歌单特征进行降维，作为“底层”输入
pca_features <- nn_data %>% 
  dplyr::select(fans, grade, playlists, length_name, length_intro, number_songs, number_hot_singers)

# 进行PCA，并进行标准化
pca_res <- prcomp(pca_features, scale. = TRUE)

# 查看解释方差，决定保留几个主成分（比如保留累计贡献>85%的，或者前3-4个）
summary(pca_res)

# 提取前4个主成分作为输入特征
pca_scores <- as.data.frame(pca_res$x[, 1:4])
names(pca_scores) <- c("PC1", "PC2", "PC3", "PC4")

# 3. 构建神经网络数据集
# 输入层 = PCA特征 (PC1-PC4) + 互动数据 (收藏/分享/评论)
# 输出层 = play_count (做对数变换 log1p 以加速收敛)
model_data <- cbind(
  dplyr::select(nn_data, play_count, collect_count, share_count, comment_count), 
  pca_scores
)

# 归一化/标准化数据 (神经网络对尺度非常敏感)
# 使用 caret 的 preProcess
preproc_values <- preProcess(model_data, method = c("center", "scale"))
model_data_scaled <- predict(preproc_values, model_data)

# 4. 划分训练集和测试集
set.seed(123)
train_index <- createDataPartition(model_data_scaled$play_count, p = 0.7, list = FALSE)
train_set <- model_data_scaled[train_index, ]
test_set <- model_data_scaled[-train_index, ]

# 5. 训练神经网络
# size = 隐层节点数, decay = 权重衰减(防止过拟合), linout = TRUE (因为是回归问题，输出要是线性的)
# 公式：播放量 由 互动数据 和 PCA特征 共同决定
nn_formula <- play_count ~ collect_count + share_count + comment_count + PC1 + PC2 + PC3 + PC4

cat("正在训练神经网络...\n")
# maxit 增加迭代次数以确保收敛
nn_model <- nnet(nn_formula, data = train_set, size = 10, decay = 0.01, linout = TRUE, maxit = 500, trace = FALSE)

# 6. 预测与评估
predictions_scaled <- predict(nn_model, test_set)

# 反归一化（还原真实的播放量数值，用于计算误差）
# 这里需要手动反归一化，或者简单地看相关性。
# 为了简单起见，我们计算标准化后的 RMSE，或者看预测值与真实值的相关性

# 获取标准化参数
play_count_mean <- mean(model_data$play_count)
play_count_sd <- sd(model_data$play_count)

# 反标准化预测值和真实值
predictions_raw <- (predictions_scaled * play_count_sd) + play_count_mean
test_set$play_count_raw <- (test_set$play_count * play_count_sd) + play_count_mean

rmse_val <- sqrt(mean((predictions_raw - test_set$play_count_raw)^2))
mae_val <- mean(abs(predictions_raw - test_set$play_count_raw))
r2_val <- cor(predictions_raw, test_set$play_count_raw)^2

cat("\n=== 神经网络模型评估 (原始数据) ===\n")
cat("RMSE (均方根误差):", rmse_val, "\n")
cat("MAE  (平均绝对误差):", mae_val, "\n")
cat("R-squared (拟合优度):", r2_val, "\n")

# 7. 绘制预测值 vs 真实值 (Error Matrix 可视化)
pred_df <- data.frame(
  Actual = test_set$play_count_raw,
  Predicted = predictions_raw
)

ggplot(pred_df, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5, color = "purple") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Module 12: Neural Network Prediction Accuracy",
    subtitle = paste("R-squared =", round(r2_val, 3), "| Red Line = Perfect Prediction"),
    x = "Actual Play Count",
    y = "Predicted Play Count"
  ) +
  theme_minimal()

# 8. 生成误差矩阵 (Error Matrix / Confusion Matrix)
# 计算分箱边界
breaks <- c(0, 793134.5, 2269060.5, 4575695, 32119005)

# 将真实值和预测值进行分箱
actual_class <- cut(test_set$play_count_raw, breaks = breaks, labels = c("Low", "Medium", "High", "Very High"), include.lowest = TRUE)
pred_class <- cut(predictions_raw, breaks = breaks, labels = c("Low", "Medium", "High", "Very High"), include.lowest = TRUE)

# 生成误差矩阵
error_matrix <- table(Predicted = pred_class, Actual = actual_class)

cat("\n=== 误差矩阵 (Error Matrix) ===\n")
print(error_matrix)

# 9. 计算分类正确率 (Accuracy)
accuracy <- sum(diag(error_matrix)) / sum(error_matrix)
cat("\n基于分级的预测正确率 (Accuracy):", round(accuracy * 100, 2), "%\n")

# 10. 可视化误差矩阵 (Heatmap)
melted_cmat <- melt(error_matrix)

ggplot(data = melted_cmat, aes(x = Actual, y = Predicted, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value), color = "white", size = 5) +
  scale_fill_gradient(low = "#3498DB", high = "#E74C3C") +
  labs(
    title = "Prediction Error Matrix (预测误差矩阵)",
    subtitle = paste("通过将播放量划分为4个等级计算 | 正确率:", round(accuracy * 100, 2), "%"),
    x = "真实等级 (Actual Level)",
    y = "预测等级 (Predicted Level)",
    fill = "数量"
  ) +
  theme_minimal()

