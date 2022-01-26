load("data/final/Summarized_Ractopamine_Pork.RData")

library(tidyverse)
library(tidytext)
library(ropencc)
library(ggthemes)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textmodels)
library(ggrepel)
library(jiebaR)
library(wordVectors)
library(irlba) # for svd
library(Rtsne) # 降維
library(broom)

################################載入停用詞辭典##################################

# 停用字辭典簡轉繁
trans <- converter(S2TWP)
zh_stopwords <- stopwords::stopwords(language = "zh", source = "misc")
zh_stopwords <- run_convert(trans, zh_stopwords)

# 自建停用字
custom <- c("與", "於", "並", "為", "項",
            "案", "有", "有些", "件", "豬在",
            "七點", "自可", "拉克", "一斤", "這跟",
            "這一", "美的", "逾", "值將", "圖說", 
            "保基", "及齡", "天將", "二二", "一年", 
            "一一", "九比", "之友", "不換", "給點", 
            "更是", "一再", "沒有", "一步", "在內", 
            "Act", "QPP", "SOGO", "home", "stay", 
            "KEYPO", "the", "Pet", "Stop", "Clinic")

custom_stopwords <- c(zh_stopwords, custom)

###############################建立泛藍詞共現矩陣###############################

# 方法一 以tokens object餵入fcm()
tokenizer <- worker(bylines = 1, user = "data/final/userdict.txt")
blue_token <- pan_blue_df %>% 
    # 斷詞
    mutate(token = segment(clean_content, tokenizer)) %>% 
    select(id, source, token)

# 用quanteda建立詞共現矩陣
# 由於quanteda的tokens()沒辦法自訂tokenizer，中文斷詞效果很差
# 因此將jieba斷詞好的結果保留column list的形式直接as.tokens()後pipe到fcm()裡面
blue_fcm <- as.tokens(blue_token$token) %>%
    tokens_remove(pattern = custom_stopwords) %>%
    tokens_select(min_nchar = 2, 
                  padding = TRUE) %>%
    # x可以是 tokens object or dfm object，此處是tokens object
    fcm(context = "window", 
        count = "frequency", 
        window = 10L, 
        ordered = FALSE)

# 方法二 以dfm餵入fcm()
blue_dfm <- blue_tokenized %>% 
    count(id, tokens) %>% 
    # 轉換成dfm的格式
    tidytext::cast_dfm(document = id, term = tokens, value = n)

# 詞共現矩陣
blue_fcm <- quanteda::fcm(
    x = blue_dfm,
    context = "document", # dfm只能設document不能設window
    count = "frequency", 
    window = 10L, 
    ordered = FALSE)

##############################泛藍詞共現關聯圖##################################

# 取方法一執行
sum(blue_fcm)
# 取出 feature frequency 前50大的詞，然後只取出這些字
feat <- names(quanteda::topfeatures(blue_fcm, 50))
# fcm中只留下前50常出現的詞彙
blue_fcm_top <- fcm_keep(blue_fcm, pattern = feat)
dim(blue_fcm_top)
size <- log(colSums(blue_fcm_top))

# 接著要把共現網絡的詞彙畫出來
set.seed(232)
blue_co_word <- textplot_network(blue_fcm_top, min_freq = 0.85, 
                                 vertex_size = size / max(size) * 3, 
                                 vertex_labelsize = 6, 
                                 vertex_labelcolor = "red") +
    labs(title = "泛藍萊豬詞共現關聯圖", 
         subtitle = "2020/8-2021/12",
         caption = "Source: Chinatimes/TVBS") +
    theme(plot.title = element_text(hjust = 0.5, size = 25), 
          plot.subtitle = element_text(hjust = 0.5, size = 15), 
          plot.caption = element_text(size = 15))

blue_co_word

############################建立泛綠詞共現關聯圖################################

green_token <- pan_green_df %>% 
    # 斷詞
    mutate(token = segment(clean_content, tokenizer)) %>% 
    select(id, source, token)

# 用quanteda建立詞共現矩陣
# 由於quanteda的tokens()沒辦法自訂tokenizer，中文斷詞效果很差
# 因此將jieba斷詞好的結果保留column list的形式直接as.tokens()後pipe到fcm()裡面
green_fcm <- as.tokens(green_token$token) %>%
    tokens_remove(pattern = custom_stopwords) %>%
    tokens_select(min_nchar = 2, 
                  padding = TRUE) %>%
    # x可以是 tokens object or dfm object，此處是tokens object
    fcm(context = "window", 
        count = "frequency", 
        window = 10L, 
        ordered = FALSE)


sum(green_fcm)

# 取出 feature frequency 前50大的詞，然後只取出這些字
feat <- names(quanteda::topfeatures(green_fcm, 50))
# fcm中只留下前50常出現的詞彙
green_fcm_top <- fcm_keep(green_fcm, pattern = feat)
dim(green_fcm_top)
size <- log(colSums(green_fcm_top))

# 接著要把共現網絡的詞彙畫出來
set.seed(2222)
green_co_word <- textplot_network(green_fcm_top, min_freq = 0.85, 
                                  vertex_size = size / max(size) * 3, 
                                  vertex_labelsize = 6, 
                                  vertex_labelcolor = "red") +
    labs(title = "泛綠萊豬詞共現關聯圖", 
         subtitle = "2020/8-2021/12",
         caption = "Source: LTN/FTV") +
    theme(plot.title = element_text(hjust = 0.5, size = 25), 
          plot.subtitle = element_text(hjust = 0.5, size = 15), 
          plot.caption = element_text(size = 15))

green_co_word






