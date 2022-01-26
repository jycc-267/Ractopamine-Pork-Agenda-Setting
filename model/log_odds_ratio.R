load("data/final/Summarized_Ractopamine_Pork.RData")

library(tidyverse)
library(jiebaR)
library(ropencc)


# 停用字辭典簡轉繁
trans <- converter(S2TWP)
zh_stopwords <- stopwords::stopwords(language = "zh", source = "misc")
zh_stopwords <- run_convert(trans, zh_stopwords)
zh_stopwords <- tibble(stopwords = zh_stopwords)

# 自建停用字
custom <- tibble(stopwords = c("與", "於", "並", "為", "項",
                               "案", "有", "有些", "件", "豬在",
                               "七點", "自可", "拉克", "一斤", "這跟",
                               "這一", "美的", "逾", "值將", "圖說", 
                               "保基", "及齡", "天將", "二二", "一年", 
                               "一一", "九比", "之友", "不換", "給點", 
                               "更是", "一再", "沒有", "一步", "在內", 
                               "Act", "QPP", "SOGO", "home", "stay", 
                               "KEYPO", "the", "Pet", "Stop", "Clinic"))
custom_stopwords <- rbind(zh_stopwords, custom)

# 建立斷詞引擎
tokenizer <- worker(bylines = 1, user = "data/final/userdict.txt")

pork_tokenized <- pork_df %>% 
    mutate(tokens = segment(code = clean_content, jiebar = tokenizer)) %>%
    unnest(cols = c(tokens)) %>%
    anti_join(custom_stopwords, by = c("tokens" = "stopwords")) %>%
    select(id, date, stance, tokens) %>%
    filter(nchar(tokens) > 1)
    
pork_odds <- pork_tokenized %>% 
    count(tokens, stance) %>%
    filter(n >= 2000) %>%
    spread(stance, n, fill = 0) %>%
    ungroup() %>%
    mutate(across(-tokens, ~ ((. + 1) / sum(. + 1))
                  )) %>%
    mutate(logratio = log2(blue / green)) %>%
    arrange(desc(logratio))

pork_odds %>%
    group_by(logratio > 0) %>%
    top_n(15, abs(logratio)) %>%
    ungroup() %>%
    mutate(tokens = reorder(tokens, logratio)) %>%
    ggplot(aes(tokens, logratio, fill = logratio < 0)) +
    geom_bar(stat = "identity") +
    coord_flip() + 
    labs(title = "泛藍泛綠高頻關鍵字勝算比", 
         x = "High Frequency Terms", 
         y = "Log Odds Ratio", 
         fill = "Party ID", 
         caption = "Source: Chinatimes/TVBS/LTN/FTV") +
    scale_fill_manual(labels = c("泛藍", "泛綠"),
                      values = c("skyblue", "springgreen3")) +
    theme(plot.title = element_text(hjust = 0.5, size = 20), 
          plot.caption = element_text(size = 10), 
          axis.title.x = element_text(size = 15), 
          axis.title.y = element_text(size = 15), 
          axis.text.x = element_text(size = 10), 
          axis.text.y = element_text(size = 10))






