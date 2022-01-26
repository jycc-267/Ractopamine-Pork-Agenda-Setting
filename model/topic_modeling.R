load("data/final/Summarized_Ractopamine_Pork.RData")
load("data/final/pan_blue_topic.RData")
load("data/final/pan_green_topic.RData")

library(tidyverse)
library(tidytext)
library(jiebaR)
library(ropencc)
library(tictoc)
library(patchwork)
library(reshape2)
library(viridis)
library(RColorBrewer) # 容易辨識的離散顏色套件

# 模型會用到的套件
library(topicmodels) # 主題模型
library(ldatuning) # 協助判斷主題數
library(stm) # 結構式主題模型
library(LDAvis)

#####################################BLUE#######################################
# 建立dtm
blue_dtm <- blue_tokenized %>%
    select(id, tokens) %>% 
    count(id, tokens) %>% 
    tidytext::cast_dtm(document = id, term = tokens, value = n)

# 建立LDA主題模型，決定主題數為12
K <- 12
blue_lda <- topicmodels::LDA(blue_dtm, 
                             # 主題數
                             k = K, 
                             # 估測方法，Gibbs速度會比較快一點。
                             method = "Gibbs", 
                             # 為了要讓結果可以重現，另外設定seed。
                             # alpha值越小，文件的主題分布會在一定範圍內變極端，方便比較分析
                             control = list(seed = 5691, alpha = 0.2))

# 取得beta與theta
blue_theta <- posterior(blue_lda)$topics
blue_beta <- posterior(blue_lda)$terms
blue_topicNames <- apply(lda::top.topic.words(blue_beta, 5, by.score = T), 2, paste, collapse = " ")

# 取得beta與theta形式的tidy dataframe
blue_topics_token <- tidytext::tidy(blue_lda, matrix = "beta")
blue_topics_doc <- tidytext::tidy(blue_lda, matrix = "gamma")

# 畫圖將主題分類呈現出來
blue_plot <- blue_topics_token %>%
    group_by(topic) %>%
    top_n(15, wt = beta) %>% 
    ungroup() %>% 
    arrange(topic, -beta) %>% 
    mutate(order = row_number()) %>%
    ggplot(aes(reorder_within(term, by = beta, within = topic), 
               y = beta,
               fill = factor(topic))) + 
    geom_col(show.legend = FALSE) +
    scale_x_reordered() + 
    facet_wrap(~ topic, scales = "free") +
    coord_flip() +
    labs(x = "token", y = "beta", title = "泛藍萊豬主題論述內容", 
         subtitle = "2020/8-2021/12", caption = "Source: Chinatimes/TVBS") +
    theme(plot.title = element_text(hjust = 0.5, size = 20), 
          plot.subtitle = element_text(hjust = 0.5, size = 15), 
          plot.caption = element_text(size = 10)) +
    # 讓y軸往右靠近圖一點
    theme(axis.title.y = element_text(margin = margin(t = 0, r = -20, b = 0, l = 0)))

blue_plot

# 提取每筆document id的年月資料，插入轉換成df的blue theta成為新的row，計算theta值平均
pan_blue_df <- pan_blue_df %>% mutate(year_month = str_extract(date, pattern = "^\\d{4}-\\d{2}"))
blue_topic_proportion <- aggregate(blue_theta, by = list(time = pan_blue_df$year_month), mean)

# 將主題名稱加入blue_topic_proportion
colnames(blue_topic_proportion)[2:(K+1)] <- blue_topicNames
blue_viz <- melt(blue_topic_proportion, id.vars = "time")

ggplot(blue_viz, aes(x = time, y = value, group = factor(variable), color = factor(variable))) +
    geom_smooth(size = 1.25, se = FALSE) +
    labs(title = '泛藍主題成分分布', 
         subtitle = '2020/8-2021/12', 
         caption = 'Source: Chinatimes/TVBS', 
         color = 'Topic', 
         x = 'Month', 
         y = 'Topic Proportion') +
    # 套用RColorBrewer套件的離散顏色組合
    scale_colour_brewer(palette = "Paired") +
    coord_fixed(ratio = 20) +
    theme(plot.title = element_text(hjust = 0.5, size = 20), 
          plot.subtitle = element_text(hjust = 0.5, size = 15), 
          plot.caption = element_text(size = 10), 
          legend.title = element_text(size = 15), 
          legend.text = element_text(size = 15))

save(blue_dtm, blue_lda, file = "pan_blue_topic.RData")

#####################################GREEN######################################

# 建立dtm
green_dtm <- green_tokenized %>%
    select(id, tokens) %>% 
    count(id, tokens) %>% 
    tidytext::cast_dtm(document = id, term = tokens, value = n)

# 建立LDA主題模型，決定主題數為12
K <- 9
green_lda <- topicmodels::LDA(green_dtm, 
                              # 主題數
                              k = K, 
                              # 估測方法，Gibbs速度會比較快一點。
                              method = "Gibbs", 
                              # 為了要讓結果可以重現，另外設定seed。
                              control = list(seed = 1234, alpha = 0.2))

# 取得beta與theta
green_theta <- posterior(green_lda)$topics
green_beta <- posterior(green_lda)$terms
green_topicNames <- apply(lda::top.topic.words(green_beta, 5, by.score = T), 2, paste, collapse = " ")

# 取得beta與theta形式的tidy dataframe
green_topics_token <- tidytext::tidy(green_lda, matrix = "beta")
green_topics_doc <- tidytext::tidy(green_lda, matrix = "gamma")

# 畫圖將主題分類呈現出來
green_plot <- green_topics_token %>%
    group_by(topic) %>%
    top_n(15, wt = beta) %>% 
    ungroup() %>% 
    arrange(topic, -beta) %>% 
    mutate(order = row_number()) %>%
    ggplot(aes(reorder_within(term, by = beta, within = topic), 
               y = beta,
               fill = factor(topic))) + 
    geom_col(show.legend = FALSE) +
    scale_x_reordered() + 
    facet_wrap(~ topic, scales = "free") +
    coord_flip() +
    labs(x = "token", y = "beta", title = "泛綠萊豬主題論述內容", 
         subtitle = "2020/8-2021/12", caption = "Source: LTN/FTV") +
    theme(plot.title = element_text(hjust = 0.5, size = 20), 
          plot.subtitle = element_text(hjust = 0.5, size = 15), 
          plot.caption = element_text(size = 10)) +
    # 讓y軸往右靠近圖一點
    theme(axis.title.y = element_text(margin = margin(t = 0, r = -10, b = 0, l = 0)))

green_plot

# 提取每筆document id的年月資料，插入轉換成df的blue theta成為新的row，計算theta值平均
pan_green_df <- pan_green_df %>% mutate(year_month = str_extract(date, pattern = "^\\d{4}-\\d{2}"))
green_topic_proportion <- aggregate(green_theta, by = list(time = pan_green_df$year_month), mean)

# 將主題名稱加入blue_topic_proportion
colnames(green_topic_proportion)[2:(K+1)] <- green_topicNames
green_viz <- melt(green_topic_proportion, id.vars = "time")

ggplot(green_viz, aes(x = time, y = value, group = factor(variable), color = factor(variable))) +
    geom_smooth(size = 1.25, se = FALSE) +
    labs(title = '泛綠主題成分分布', 
         subtitle = '2020/8-2021/12', 
         caption = 'Source: LTN/FTV', 
         color = 'Topic', 
         x = 'Month', 
         y = 'Topic Proportion') +
    # 套用RColorBrewer套件的離散顏色組合
    scale_colour_brewer(palette = "Paired") +
    coord_fixed(ratio = 20) +
    theme(plot.title = element_text(hjust = 0.5, size = 20), 
          plot.subtitle = element_text(hjust = 0.5, size = 15), 
          plot.caption = element_text(size = 10), 
          legend.title = element_text(size = 15), 
          legend.text = element_text(size = 15))

save(green_dtm, green_lda, file = "pan_green_topic.RData")

##############################最適主題數檢測####################################
tic()
result <- ldatuning::FindTopicsNumber(
    blue_dtm,
    topics = seq(from = 5, to = 15, by = 1),
    metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
    method = "Gibbs",
    control = list(seed = 5691, alpha = 0.2),
    mc.cores = 2L, # 用雙CPU運算
    verbose = TRUE)
toc()

ldatuning::FindTopicsNumber_plot(result)














