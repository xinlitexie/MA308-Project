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

data <- read_csv("网易云音乐歌单分析/data.csv")

###1、清洗数据

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

data <- data %>%
  mutate(
    create_datetime = as.POSIXct(create_time, origin = "1970-01-01"),
    create_year = year(create_datetime),
    create_month = month(create_datetime),
    create_date = as.Date(create_datetime)
  )

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
###2、描述性分布和热图

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



###3、模块1：探究播放量、收藏量、分享量、评论量以及作者粉丝数之间的线性相关关系。
result_vars <- data_cleaned %>%
  select(play_count, collect_count, share_count, comment_count, fans)
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







###模块2：高价值标签挖掘
tag_analysis <- data_cleaned %>%
  select(topics, play_count) %>%
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

tag_analysis_avoid_extreme <- data_cleaned %>%
  select(topics, play_count) %>%
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

tag_freq <- data_cleaned %>%
  select(topics, play_count) %>%
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
ggplot(tag_analysis, aes(x = reorder(topics, avg_play), y = avg_play)) +
  geom_col(fill = "steelblue") +
  coord_flip() + # 翻转坐标轴便于阅读标签
  labs(
    title = "Top 10 Tags by Average Play Count",
    x = "Tags",
    y = "Average Play Count"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))

ggplot(tag_analysis_avoid_extreme, aes(x = reorder(topics, avg_play), y = avg_play)) +
  geom_col(fill = "steelblue") +
  coord_flip() + # 翻转坐标轴便于阅读标签
  labs(
    title = "Top 10 Tags by Average Play Count(Happen more than ten times)",
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


###模块6：随机森林（针对上面的30个标签（可能有重复）做随机森林）
# --- 1. 数据准备 ---
# 清洗 topics 列，移除多余符号
tag_df <- data_cleaned %>%
  select(play_count, fans, topics) %>%
  mutate(topics = str_remove_all(topics, "\\[|\\]|'|\"| ")) %>%
  separate_rows(topics, sep = ",") %>%
  filter(topics != "")

# 找出 Top 30 最常用的标签
top_tags_list <- tag_df %>%
  count(topics, sort = TRUE) %>%
  slice_head(n = 30) %>%
  mutate(
    # 【关键修复】给每个中文标签创建一个英文 ID (tag_1, tag_2...)
    # 这样进模型时绝对不会报错
    tag_id = paste0("tag_", row_number()) 
  )

print("Top Tags Mapping (前5个示例):")
print(head(top_tags_list))

# --- 2. 构建回归数据集 ---
reg_data_full <- df_clean %>%
  select(play_count, fans, topics) %>%
  mutate(
    log_play = log1p(play_count), 
    log_fans = log1p(fans)
  )

# 【关键修复】循环创建 0/1 列，使用英文 ID 作为列名
# 我们在这里遍历映射表，把 topics 里的中文匹配到对应的英文 tag_id 列中
for(i in 1:nrow(top_tags_list)) {
  real_name <- top_tags_list$topics[i]   # 真实的中文名 (用来匹配)
  safe_name <- top_tags_list$tag_id[i]   # 安全的英文名 (用来做列名)
  
  # 如果 topics 包含这个中文词，则该英文列标为 1
  reg_data_full[[safe_name]] <- ifelse(str_detect(reg_data_full$topics, real_name), 1, 0)
}

# --- 3. 运行回归模型 ---
# 公式现在变成了：log_play ~ log_fans + tag_1 + tag_2 ... (全英文，非常稳定)
features <- top_tags_list$tag_id
formula_str <- paste("log_play ~ log_fans +", paste(features, collapse = " + "))

# 运行模型
model_full <- lm(as.formula(formula_str), data = reg_data_full)

# --- 4. 提取结果并换回中文名 ---
model_full_tidy <- tidy(model_full, conf.int = TRUE) %>%
  # 只筛选出标签变量
  filter(term %in% features) %>%
  # 【关键修复】把英文 ID (tag_1) 换回中文名 (欧美) 用于画图
  left_join(top_tags_list, by = c("term" = "tag_id")) %>%
  filter(p.value < 0.05) %>%
  mutate(
    direction = ifelse(estimate > 0, "Positive (流量增益)", "Negative (流量减益)")
  ) %>%
  arrange(desc(estimate))

# --- 5. 绘制森林图 ---
ggplot(model_full_tidy, aes(x = estimate, y = fct_reorder(topics, estimate), color = direction)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, size = 0.8) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("Positive (流量增益)" = "#E41A1C", "Negative (流量减益)" = "#377EB8")) +
  labs(
    title = "Top Tags Impact on Play Count (Fixed Version)",
    subtitle = "Regression Coefficients (Controlling for Fans)",
    x = "Effect Size (Impact on Play Count)",
    y = "Tags",
    color = "Impact Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 10, face = "bold") # 这里的中文应该能正常显示了
  )





###模块5：检测播放数异常值点和杠杆点


###模块5：聚类分析（需要去掉异常值点），之后再补充进去

###肘部算法
set.seed(1234)
wss = numeric(10)
for (i in 1:10) {
  km = kmeans(data_cleaned$play_count, centers = i, nstart = 25)
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
result <- kmeans_function(data_cleaned, "play_count", k = 4)

###可视化操作
data_clustered <- result$data_with_clusters
ggplot(data_clustered, aes(x = factor(cluster), y = play_count, fill = factor(cluster))) +
  geom_boxplot() +
  labs(title = "不同聚类的 play_count 分布",
       x = "聚类",
       y = "播放次数",
       fill = "聚类") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()

###注意到解释方差比例达到了88.07%，故而可以认为这个聚类算法十分有效
explained_variance <- result$kmeans_result$betweenss / result$kmeans_result$totss
cat("解释方差比例: ", round(explained_variance * 100, 2), "%\n")


###模块4：日均播放量
# 假设当前时间是数据集中最后一条数据的后一天，或者使用系统时间
reference_date <- max(data_cleaned$create_time) + days(1)

data_velocity <- data_cleaned %>%
  mutate(
    # 计算歌单存活天数
    days_alive = as.numeric(difftime(reference_date, create_time, units = "days")),
    # 计算日均播放量 (Velocity)
    plays_per_day = play_count / days_alive
  ) %>%
  arrange(desc(plays_per_day))

# 看看是谁在“爆发”：取出日均播放量最高的 Top 10
top_velocity <- head(data_velocity %>% select(name, plays_per_day, days_alive, fans), 10)

# 可视化：存活天数 vs 日均播放量
ggplot(data_velocity, aes(x = days_alive, y = plays_per_day)) +
  geom_point(alpha = 0.5, color = "purple") +
  scale_y_log10() + # 使用对数轴，因为爆发系数差异巨大
  labs(
    title = "Content Velocity Analysis",
    x = "Days Since Creation",
    y = "Average Plays Per Day (Log Scale)",
    subtitle = "Newer hits vs. Old classics"
  ) +
  theme_minimal()


###再补充一次k聚类算法 k=4






###模块7：歌单信息的影响
data_morph <- data_cleaned %>%
  dplyr::select(play_count, length_name, length_intro, number_songs, number_hot_singers) %>%
  filter(play_count > 0) %>%
  na.omit()


boxcox(lm(play_count ~ length_name + length_intro + number_songs + number_hot_singers, data = data_morph), 
       lambda = seq(-2, 2, length.out = 100))

# --- 2. 建立回归模型 ---
# 公式：播放量 ~ 标题长度 + 简介长度 + 歌曲总数 + 热门歌手数
model_morph <- lm(log(play_count) ~ length_name + length_intro + number_songs + number_hot_singers, data = data_morph)
print("--- Regression Summary: Length & Size Effects ---")
print(summary(model_morph))
stepAIC(model_morph, direction = "backward")
model_morph_changed <- lm(log(play_count) ~ length_name + length_intro + number_hot_singers, data = data_morph)
anova(model_morph_changed, model_morph)





###模块8：作者信息的影响
data_author <- data_cleaned %>%
  dplyr::select(play_count, fans, grade, playlists, talent, verification, musician) %>%
  filter(play_count > 0) %>%
  na.omit()

model_full <- lm(
  play_count ~ fans * grade * playlists + talent * verification * musician,
  data = data_author
)

summary(model_full)
stepAIC(model_full, direction = "backward")

model_changed <- lm(
  play_count ~ fans * grade * playlists + verification,
  data = data_author
)

summary(model_changed)
anova(model_changed, model_full)

###模块11：身份的作用
data_id <- data_cleaned %>%
  mutate(
    is_talent = ifelse(talent == "是", 1, 0),
    is_verified = ifelse(verification == "是", 1, 0),
    is_musician = ifelse(musician == "是", 1, 0)
  )

identity_stats <- data.frame(
  Identity = c("Verification (认证用户)", "Talent (达人)", "Musician (音乐人)", "none(无)"),
  Avg_Play = c(
    mean(data_id$play_count[data_id$is_verified == 1], na.rm=TRUE),
    mean(data_id$play_count[data_id$is_talent == 1], na.rm=TRUE),
    mean(data_id$play_count[data_id$is_musician == 1], na.rm=TRUE),
    mean(data_id$play_count[data_id$is_musician == 0 & data_id$is_verified == 0 & data_id$is_talent == 0], na.rm=TRUE)
  )
)

# 2. 绘图：独立身份对比图
ggplot(identity_stats, aes(x = reorder(Identity, -Avg_Play), y = Avg_Play, fill = Identity)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = comma(Avg_Play, accuracy = 1)), vjust = -0.5, size = 4) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = c("black", "gray50", "#E74C3C", "#F1C40F")) + # 金银铜配色逻辑
  labs(
    title = "BY-CA类分析: 创作者身份价值评估 (Identity ROI)",
    subtitle = "数据洞察：'认证用户' (V标) 的平均流量表现优于 '音乐人'",
    x = "身份类型",
    y = "平均播放量"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14))

###补一个期望的hypothesis test，主要说明只有verification能拉开显著差距，musician和talent影响低或者几乎无差别，跟none对比就行




###模块9：时序分析
## --- Part 1: C类 - 最佳发布时间分析 ---

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


###补一个月份的
# 1. 数据准备
data_time_month <- data_cleaned %>%
  mutate(
    create_dt = as_datetime(create_time),
    # 将月份转化为有序因子 (1月到12月)
    month_of_year = factor(month(create_dt, label = TRUE), 
                           levels = month.abb)  # 确保1-12月顺序
  ) %>%
  group_by(month_of_year) %>%
  summarise(avg_play = mean(play_count, na.rm = TRUE))

# 2. 绘图：独立时间柱状图
ggplot(data_time_month, aes(x = month_of_year, y = avg_play, fill = month_of_year)) +
  geom_col(alpha = 0.8) +
  # 在柱子上添加具体数值标签
  geom_text(aes(label = comma(avg_play, accuracy = 1)), vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) + # 让Y轴上方留白
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "C类分析: 不同发布月份的流量差异 (Monthly Analysis)",
    subtitle = "数据洞察：X月发布的歌单平均播放量最高 (季节性效应)",
    x = "发布时间 (月份)",
    y = "平均播放量 (Average Play Count)"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14))
###好像有问题



###文本分析
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
