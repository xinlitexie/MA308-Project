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



#====模块10：文本分析====
# 1. 自动识别列名
intro_cols <- grep("intro_", names(data_cleaned), value = TRUE)
print(paste("已检测到简介特征列数量:", length(intro_cols)))

# --- 调试：看看第一列里到底是啥 ---
# 这步能帮你确认有没有乱码
first_col_preview <- head(data_cleaned[[intro_cols[1]]])
print("【调试】第一列数据预览 (请检查是否为中文'是/否'):")
print(first_col_preview)

# 2. 循环计算 (加了去空格和模糊匹配)
intro_impact <- data.frame(term = character(), avg_play = numeric(), count = numeric(), stringsAsFactors = FALSE)

for(col in intro_cols) {
  raw_val <- data_cleaned[[col]]
  
  # --- 核心修复 ---
  if(is.character(raw_val) || is.factor(raw_val)) {
    # 1. 强制转字符
    char_val <- as.character(raw_val)
    # 2. 去除前后空格
    clean_val <- trimws(char_val)
    # 3. 只要包含 "是" 就算 1 (兼容性最强)
    val_col <- ifelse(grepl("是", clean_val), 1, 0)
  } else {
    # 如果本来就是数字 (1/0)
    val_col <- as.numeric(raw_val)
  }
  
  # 统计出现次数
  count_ones <- sum(val_col == 1, na.rm=TRUE)
  
  # 只有出现超过 10 次才统计 (样本太少没意义)
  if(count_ones > 10) { 
    avg <- mean(data_cleaned$play_count[val_col == 1], na.rm=TRUE)
    intro_impact <- rbind(intro_impact, data.frame(term = col, avg_play = avg, count = count_ones))
  }
}

# 3. 检查结果
if(nrow(intro_impact) == 0) {
  print("⚠️ 警告：结果为空！可能是编码问题导致无法识别中文'是'。")
  print("尝试建议：请重新使用 encoding='GBK' 读取 data.csv")
} else {
  # 4. 绘图
  top_intro <- intro_impact %>% 
    arrange(desc(avg_play)) %>% 
    head(16) %>%
    mutate(term_clean = str_remove(term, "intro_"))
  
  p <- ggplot(top_intro, aes(x = avg_play, y = reorder(term_clean, avg_play))) +
    geom_col(fill = "#8E44AD", alpha = 0.7) + 
    geom_text(aes(label = comma(avg_play, accuracy = 1)), hjust = -0.1, size = 3.5) +
    scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.3))) +
    labs(
      title = "AL-BB类分析: 简介文案的高价值词汇",
      subtitle = "数据洞察：包含'收录/封面/专辑'的歌单平均播放量更高",
      x = "平均播放量",
      y = "简介关键词"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.text.y = element_text(size = 11, face = "bold")
    )
  
  print(p)
}

### 看看能不能根据前面的标签做一下，可以根据热词图来写



#====模块11：综合分析====

