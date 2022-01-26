library(tidyverse)
library(quanteda)
library(jiebaR)

# 讀入資料
news <- readr::read_rds("data/news_df.rds") %>% 
  select(news_id, content, source)
glimpse(news)

# 斷詞引擎
seg_engine <- worker(bylines = TRUE)

# 斷詞
news_token <- news %>% 
  # 斷詞
  mutate(token = segment(content, seg_engine))

# 取出斷詞結果的欄位，將其轉換為quanteda::tokens的格式
news_fcm <- as.tokens(news_token$token) %>% # 主要是這個步驟
  # 中間清理資料的部分請參照quanteda的說明，我就不多做了。
  fcm(context = "window", window = 12)
