load("data/final/Ractopamine_Pork.RData")
rm(pan_blue)
rm(pan_green)

library(tidyverse)
library(tidytext)
library(jiebaR)
library(ropencc)
library(ggthemes)

###################################中時#########################################
# 檢測 NA 值
sum(is.na(chinatimes_df$content))
for (i in seq_along(chinatimes_df$content)){
    if (is.na(chinatimes_df$content[i]) == TRUE){
        print(i)
    }
}

# 用 title 填補 NA 值
for (i in seq_along(chinatimes_df$content)){
    if (is.na(chinatimes_df$content[i]) == TRUE){
        chinatimes_df$content[i] = chinatimes_df$title[i]
    }
}

# 清除雜訊(注意順序)
chinatimes_df <- chinatimes_df %>%
    ## 百分比、日期
    mutate(clean_content = str_replace_all(content, 
                                           pattern = '[0-9.]+(%|％)|\\d{4}年|\\d{1,2}月|\\d{1,2}日', 
                                           replacement = '')) %>%
    ## 數字、年月日
    mutate(clean_content = str_replace_all(clean_content, 
                                           pattern = '[0-9]+|年月日', 
                                           replacement = '')) %>%
    ## 數字單位
    mutate(clean_content = str_replace_all(clean_content, 
                                           pattern = '萬票|千票|千萬|百萬|萬噸|億度|億元|千噸|萬份|萬劑', 
                                           replacement = '')) %>%
    ## 移除案件編號，ex: 第17案
    mutate(clean_content = str_replace_all(clean_content, 
                                           pattern = '第案|第張|第條|第場|第，|第度', 
                                           replacement = '')) %>%
    ## 移除立場聲明
    mutate(clean_content = str_replace_all(clean_content, 
                                           pattern = '以上言論不代表旺中媒體集團立場|作者為資深媒體人', 
                                           replacement = '')) %>%
    ## 資料來源、資料照等
    mutate(clean_content = str_replace_all(clean_content, 
                                           pattern = '\\(資料.+\\)', 
                                           replacement = '')) %>%
    ## 特殊符號與網址
    mutate(clean_content = str_replace_all(clean_content, 
                                           pattern = '「」|（）|／|/|~|～|→|＋|\\+|-|https?:\\/\\/[a-zA-Z0-9.\\/_]+', 
                                           replacement = '')) %>%
    ## 記者xxx/xx報導
    mutate(clean_content = str_replace_all(clean_content, 
                                           pattern = '^.{2,6}／.{2,15}報導）?|【愛傳媒.{3,12}專欄】', 
                                           replacement = ''))

#####################################民視#######################################

# 填補 ftv$content 的 NA 值
sum(is.na(ftv_df$content))
for (i in seq_along(ftv_df$content)){
    if (is.na(ftv_df$content[i]) == TRUE){
        ftv_df$content[i] = ftv_df$title[i]
    }
}

# 清除雜訊(注意順序)
ftv_df <- ftv_df %>%
    ## 百分比、日期
    mutate(clean_content = str_replace_all(content, 
                                           pattern = '[0-9.]+(%|％)|\\d{4}年|\\d{1,2}月|\\d{1,2}日', 
                                           replacement = '')) %>%
    ## 數字、年月日
    mutate(clean_content = str_replace_all(clean_content, 
                                           pattern = '[0-9]+|年月日', 
                                           replacement = '')) %>%
    ## 數字單位
    mutate(clean_content = str_replace_all(clean_content, 
                                           pattern = '萬票|千票|千萬|百萬|萬噸|億度|億元|千噸|萬份|萬劑', 
                                           replacement = '')) %>%
    ## 案件編號，法條、量詞
    mutate(clean_content = str_replace_all(clean_content, 
                                           pattern = '第案|第張|第條|第場|第，|第度', 
                                           replacement = '')) %>%
    ## 中二補選雜訊
    mutate(clean_content = str_replace_all(clean_content, 
                                           pattern = '中二補選最新消息看這裡|📣|➡️', 
                                           replacement = '')) %>%
    ## 立場聲明
    mutate(clean_content = str_replace_all(clean_content, 
                                           pattern = '〈全民筆讚〉為公開投稿平台，投書言論不代表《民視新聞網》立場', 
                                           replacement = '')) %>%
    ## 特殊符號與網址
    mutate(clean_content = str_replace_all(clean_content, 
                                           pattern = '「」|（）|／|/|~|～|→|＋|\\+|-|https?:\\/\\/[a-zA-Z0-9.\\/_]+', 
                                           replacement = '')) %>%
    ## 記者xxx/xx報導
    mutate(clean_content = str_replace_all(clean_content, 
                                           pattern = '文／.{3}|(政治|影音)中心／.{2,15}報導|（民視.+／.+報導）', 
                                           replacement = ''))

######################################TVBS######################################

tvbs_df <- tvbs_df %>%
    ## 百分比、日期
    mutate(clean_content = str_replace_all(content, 
                                           pattern = '[0-9.]+(%|％)|\\d{4}年|\\d{1,2}月|\\d{1,2}日', 
                                           replacement = '')) %>%
    ## 數字、年月日
    mutate(clean_content = str_replace_all(clean_content, 
                                           pattern = '[0-9]+|年月日', 
                                           replacement = '')) %>%
    ## 數字單位
    mutate(clean_content = str_replace_all(clean_content, 
                                           pattern = '萬票|千票|千萬|百萬|萬噸|億度|億元|千噸|萬份|萬劑', 
                                           replacement = '')) %>%
    ## 案件編號，法條、量詞
    mutate(clean_content = str_replace_all(clean_content, 
                                           pattern = '第案|第張|第條|第場|第，|第度', 
                                           replacement = '')) %>%
    ## 宣傳
    mutate(clean_content = str_replace_all(clean_content, 
                                           pattern = "更多新聞：.{1,25}|最HOT話題在這！想跟上時事，快點我加入TVBS新聞LINE好友！|開啟小鈴鐺新聞搶先看|TVBS新聞YouTube頻道改版囉！快點我按讚訂閱|開啟小鈴鐺TVBS YouTube頻道新聞搶先看快點我按讚訂閱|《TVBS》提醒您：輕生解決不了問題，卻留給家人無比悲痛。請珍惜生命，請再給自己一次機會生命線請撥1995；衛福部諮詢安心專線：珍愛生命，請撥打 1925（24小時）；張老師專線：1980", 
                                           replacement = "")) %>%
    ## 來源
    mutate(clean_content = str_replace_all(clean_content, 
                                           pattern = "（中央社）|【財訊】|錢怡君.{1,2}|彭志宇.{1,2}|實習編輯／.{3}|文／.{3}|作者：\\S{2,3}（\\S{2,20}）", 
                                           replacement = "")) %>%
    ## 新聞大白話
    mutate(clean_content = str_replace_all(clean_content, 
                                           pattern = "新聞大白話YT頻道|新聞大白話粉絲專頁|新聞大白話頻道|新聞大白話粉專|新聞大白話頻道 新聞大白話粉專|《TVBS新聞網》與《新聞大白話》聯手出擊|《新聞大白話》主持人.{3}街頭開講、傾聽民意，現場也將邀請特別來賓共同討論", 
                                           replacement = "")) %>%
    ## 網路溫度計，KEYPO，OpView
    mutate(clean_content = str_replace_all(clean_content, 
                                           pattern = "【網路溫度計調查結果之圖文，未經授權請勿轉載、改寫】想看完整名次、更多主題趣味調查報導，請詳見|更多精采報導，請上《DailyView網路溫度計官網https://dailyview.tw》.+|《DailyView網路溫度計粉絲團www.facebook.com/DailyView.tw》.+|本研究資料由《KEYPO大數據關鍵引擎》提供|本分析報告使用《OpView社群口碑資料庫》.+", 
                                           replacement = "")) %>%
    ## 立場聲明
    mutate(clean_content = str_replace_all(clean_content, 
                                           pattern = "本文為作者評論意見並授權刊登，不代表TVBS立場|版權所有，未經許可請勿引用，以免侵權|未經授權，請勿轉載|本文為作者評論意見並獨家授權刊登，不代表TVBS立場|僅反映作者意見，不代表TVBS立場", 
                                           replacement = "")) %>%
    ## 新冠疫情
    mutate(clean_content = str_replace_all(clean_content, 
                                           pattern = "【加入社團‧疫起發聲】邀請世界各角落的你加入Facebook社團【全球說疫情】訴說封城和防疫大小事→https://www.facebook.com/groups/894299717696509/|《TVBS》提醒您：因應新冠肺炎疫情，疾管署持續加強疫情監測與邊境管制措施，如有疑似症狀，請撥打：1922專線，或0800-001922|並依指示配戴口罩儘速就醫，同時主動告知醫師旅遊史及接觸史，以利及時診斷及通報", 
                                           replacement = "")) %>%
    ## 其他
    mutate(clean_content = str_replace_all(clean_content, 
                                           pattern = "TVBS 56 台推出「立院萊豬決戰特報」.+|國民黨跟秋鬥\\d{1,2}月\\d{1,2}日號召民眾走上街頭反萊豬|開始直播，請參考以下平台|網路民調|TVBS邀您來表態|2020美選專頁上線啦！TVBS與您即時掌握選情|https://pse.is/38jajc|緊抓話題熱點，與你討論全球大小事！點我追蹤【TVBS Twitter】|擠出小時間，看看大新奇！全新影音品牌上線囉，一起擠看看|關於美豬進口，TVBS邀請您來表態|https://bit.ly/3lPXTgV", 
                                           replacement = "")) %>% 
    ## 特殊符號與網址
    mutate(clean_content = str_replace_all(clean_content,
                                           pattern = "「」|（）|／|/|~|～|→|＋|\\+|-|●.+https?:\\/\\/[a-zA-Z0-9.\\/_]+\\/|https?:\\/\\/[a-zA-Z0-9.\\/_]+", 
                                           replacement = ""))

#####################################自由時報###################################
# 填補NA值
sum(is.na(ltn_df$content))
for (i in seq_along(ltn_df$content)){
    if (is.na(ltn_df$content[i]) == TRUE){
        ltn_df$content[i] = ltn_df$title[i]
    }
}

# 清除雜訊
ltn_df <- ltn_df %>%
    ## 百分比、日期
    mutate(clean_content = str_replace_all(content, 
                                           pattern = '[0-9.]+(%|％)|\\d{4}年|\\d{1,2}月|\\d{1,2}日', 
                                           replacement = '')) %>%
    ## 數字、年月日
    mutate(clean_content = str_replace_all(clean_content, 
                                           pattern = '[0-9]+|年月日', 
                                           replacement = '')) %>%
    ## 數字單位
    mutate(clean_content = str_replace_all(clean_content, 
                                           pattern = '萬票|千票|千萬|百萬|萬噸|億度|億元|千噸|萬份|萬劑', 
                                           replacement = '')) %>%
    ## 案件編號，法條、量詞
    mutate(clean_content = str_replace_all(clean_content, 
                                           pattern = '第案|第張|第條|第場|第，|第度', 
                                           replacement = '')) %>%
    ## 來源提供
    mutate(clean_content = str_replace_all(clean_content, 
                                           pattern = "（(國民黨|民進黨)提供）|（.{1,20}提供）", 
                                           replacement = "")) %>%
    ## 圖片人物位置
    mutate(clean_content = str_replace_all(clean_content, 
                                           pattern = "（.?(中|左|右).?）|（第?.排(中|左|右).?）", 
                                           replacement = "")) %>%
    ## 圖跟資料照
    mutate(clean_content = str_replace_all(clean_content, 
                                           pattern = "（見圖.?）|（(.{2,10})?資料照）|（資料照，記者.{2,15}翻?攝）", 
                                           replacement = "")) %>%
    ## 圖片獲取
    mutate(clean_content = str_replace_all(clean_content, 
                                           pattern = "（圖?(擷|取|擷取)自.{1,30}）", 
                                           replacement = "")) %>%
    ## 記者xxx/xx報導
    mutate(clean_content = str_replace_all(clean_content, 
                                           pattern = "〔(記者|即時新聞)?.+／.{2,20}報導〕|記者.{2,3}／特稿", 
                                           replacement = "")) %>%
    ## 翻攝
    mutate(clean_content = str_replace_all(clean_content, 
                                           pattern = "（記者.{2,3}翻?攝）|（翻攝自.{1,15}）", 
                                           replacement = "")) %>%
    ## 自由開講徵稿
    mutate(clean_content = str_replace_all(clean_content, 
                                           pattern = "《自由開講》是一個提供民眾對話的電子論壇.+", 
                                           replacement = "")) %>%
    ## 特殊符號與網址
    mutate(clean_content = str_replace_all(clean_content, 
                                           pattern = "「」|（）|／|/|~|～|→|＋|\\+|-|https?:\\/\\/[a-zA-Z0-9.\\/_]+", 
                                           replacement = ''))

# 檢測clean_content空值
# 填補clean_content空值
for (i in seq_along(ltn_df$clean_content)){
    if (ltn_df$clean_content[i] == ""){
        print(i)
    }
}

for (i in seq_along(ltn_df$clean_content)){
    if (ltn_df$clean_content[i] == ""){
        ltn_df$clean_content[i] = ltn_df$title[i]
    }
}

##################################合併清完的資料################################
# 字串檢測
for (i in seq_along(tvbs_df$clean_content)){
    if (str_detect(tvbs_df$clean_content[i], pattern = '作者：') == TRUE){
        print(i)
    }
}

# 合併chinatimes跟tvbs；ltn跟ftv
pan_blue_df <- rbind(chinatimes_df, tvbs_df)
pan_blue_df <- pan_blue_df %>%
    mutate(date = as.Date(date)) %>%
    arrange(desc(date)) %>%
    filter(date >= "2020-08-01" & date <= "2021-12-31") %>%
    mutate(id = row_number()) %>%
    mutate(stance = "blue") %>%
    select(id, title, date, source, stance, link, content, clean_content)

pan_green_df <- rbind(ltn_df, ftv_df)
pan_green_df <- pan_green_df %>%
    mutate(date = as.Date(date)) %>%
    arrange(desc(date)) %>%
    filter(date >= "2020-08-01" & date <= "2021-12-31") %>%
    mutate(id = row_number()) %>%
    mutate(stance = "green") %>%
    select(id, title, date, source, stance, link, content, clean_content)

# 把泛藍泛綠併在一起    
pork_df <- rbind(pan_blue_df, pan_green_df)
pork_df <- pork_df %>% arrange(desc(date))

################################載入停用詞辭典##################################

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

#####################################斷詞#######################################

# 斷詞引擎，設定斷詞字典路徑
tokenizer <- worker(bylines = 1, user = "data/final/userdict.txt")

# 移除停用字、斷詞
## 斷詞後移除長度為 1 的詞
## 抽取年月資料
blue_tokenized <- pan_blue_df %>% 
    mutate(year_month = str_extract(date, pattern = "^\\d{4}-\\d{2}")) %>%
    mutate(tokens = segment(code = clean_content, jiebar = tokenizer)) %>%
    unnest(cols = c(tokens)) %>%
    anti_join(custom_stopwords, by = c("tokens" = "stopwords")) %>%
    select(id, date, year_month, source, link, tokens) %>%
    filter(nchar(tokens) > 1)

green_tokenized <- pan_green_df %>% 
    mutate(year_month = str_extract(date, pattern = "^\\d{4}-\\d{2}")) %>%
    mutate(tokens = segment(code = clean_content, jiebar = tokenizer)) %>%
    unnest(cols = c(tokens)) %>%
    anti_join(custom_stopwords, by = c("tokens" = "stopwords")) %>%
    select(id, date, year_month, source, link, tokens) %>%
    filter(nchar(tokens) > 1)

#####################################詞頻#######################################

# 詞頻
blue_tf <- blue_tokenized %>% 
    count(year_month, tokens) %>%
    group_by(year_month) %>%
    arrange(desc(n)) %>%
    slice_head(n = 10) %>%
    ungroup()

green_tf <- green_tokenized %>% 
    count(year_month, tokens) %>%
    group_by(year_month) %>%
    arrange(desc(n)) %>%
    slice_head(n = 10) %>%
    ungroup()

# 用 tfidf 篩選關鍵字
blue_tfidf <- blue_tokenized %>% 
    count(year_month, tokens) %>%
    tidytext::bind_tf_idf(term = tokens, document = year_month, n = n) %>% 
    # 選取出每月TF-IDF值最高的10字
    group_by(year_month) %>%
    arrange(desc(tf_idf)) %>%
    slice_head(n = 10) %>%
    ungroup()

green_tfidf <- green_tokenized %>% 
    count(year_month, tokens) %>%
    tidytext::bind_tf_idf(term = tokens, document = year_month, n = n) %>% 
    # 選取出每月TF-IDF值最高的10字
    group_by(year_month) %>%
    arrange(desc(tf_idf)) %>%
    slice_head(n = 10) %>%
    ungroup()

###################################畫圖#########################################

# 泛藍泛綠新聞量變化折線圖
pork_news <- pork_df %>% count(date, stance)
ggplot(pork_news, aes(x = date, y = n, color = factor(stance))) + 
    geom_line(size = 0.75) + 
    geom_vline(xintercept = as.Date("2021-12-18")) +
    geom_smooth(color = "skyblue", se = TRUE) +
    scale_color_manual(labels = c("泛藍", "泛綠"), 
                       values=c("steelblue", "springgreen3")) +
    # 增加標題、副標、xy軸名稱、出處
    labs(x = "Date", 
         y = "Count",
         title = "泛藍泛綠新聞量變化趨勢",
         subtitle = "2020/08-2021/12", 
         caption = "Source: Chinatimes/TVBS/LTN/FTV", 
         color = "Party ID") +
    annotate(geom = "text", label = "2021-12-18", x = as.Date("2021-12-18"), y = 240) +
    theme_stata() +
    theme(plot.title = element_text(hjust = 0.5, size = 20), 
          plot.subtitle = element_text(hjust = 0.5, size = 15), 
          plot.caption = element_text(hjust = 1, size = 10))

# 泛藍tfidf作圖
ggplot(blue_tfidf) +
    geom_bar(aes(x = tidytext::reorder_within(x = tokens, 
                                              by = tf_idf, 
                                              within = year_month), 
                 y = tf_idf, 
                 fill = year_month), 
             stat = "identity") +
    tidytext::scale_x_reordered() +
    # 以year_month為類別變項繪製圖表
    facet_wrap(vars(year_month), scales = "free") +
    # 增加標題、副標、xy軸名稱、出處
    labs(title = "泛藍萊豬 tfidf 關鍵字", 
         subtitle = "2020/08-2021/12", 
         caption = "Source: Chinatimes/TVBS", 
         x = "tokens", 
         y = "tf-idf") + 
    # 翻轉xy軸
    coord_flip() +
    theme(text = element_text(size = 10), axis.text.x = element_text(angle = 45, hjust = 1)) +
    # 移除圖例
    theme(legend.position = "none") +
    # 標題、副標置中，更改字體大小
    theme(plot.title = element_text(hjust = 0.5, size = 20), 
          plot.subtitle = element_text(hjust = 0.5, size = 15), 
          plot.caption = element_text(size = 10)) +
    # 讓y軸往右靠近圖一點
    theme(axis.title.y = element_text(margin = margin(t = 0, r = -20, b = 0, l = 0)))

# 泛綠tfidf作圖
ggplot(green_tfidf) +
    geom_bar(aes(x = tidytext::reorder_within(x = tokens, 
                                              by = tf_idf, 
                                              within = year_month), 
                 y = tf_idf, 
                 fill = year_month), 
             stat = "identity") +
    tidytext::scale_x_reordered() +
    # 以year_month為類別變項繪製圖表
    facet_wrap(vars(year_month), scales = "free") +
    # 增加標題、副標、xy軸名稱、出處
    labs(title = "泛綠萊豬 tfidf 關鍵字", 
         subtitle = "2020/08-2021/12", 
         caption = "Source: LTN/FTV", 
         x = "tokens", 
         y = "tf-idf") + 
    # 翻轉xy軸
    coord_flip() +
    theme(text = element_text(size = 10), axis.text.x = element_text(angle = 45, hjust = 1)) +
    # 移除圖例
    theme(legend.position = "none") +
    # 標題、副標置中，更改字體大小
    theme(plot.title = element_text(hjust = 0.5, size = 20), 
          plot.subtitle = element_text(hjust = 0.5, size = 15), 
          plot.caption = element_text(size = 10)) +
    # 讓y軸往右靠近圖一點
    theme(axis.title.y = element_text(margin = margin(t = 0, r = -20, b = 0, l = 0)))


save(chinatimes_df, tvbs_df, ltn_df, ftv_df, 
     pan_blue_df, pan_green_df, 
     pork_df, pork_news, 
     blue_tokenized, green_tokenized,
     blue_tfidf, green_tfidf, file = "Summarized_Ractopamine_Pork.RData")

load("data/final/Summarized_Ractopamine_Pork.RData")
