tvbs_df <- readr::read_csv("data/final/tvbs_20201005_20211230.csv")
ftv_df <- readr::read_csv("data/final/ftv_20200912_20211228.csv")
ltn_df <- readr::read_csv("data/final/ltn_20200828_20220101.csv")
chinatimes_df <- readr::read_csv("data/final/chinatimes_20200829_20220104.csv")

tvbs_df <- tvbs_df %>%
    mutate(source = 'tvbs') %>%
    select(title, date, source, link, content)
ftv_df <- ftv_df %>%
    mutate(source = 'ftv')  %>%
    select(title, date, source, link, content)
chinatimes_df <- chinatimes_df %>%
    mutate(source = 'chinatimes') %>%
    select(title, date, source, link, content)
ltn_df <- ltn_df %>%
    mutate(source = 'ltn') %>%
    select(title, date, source, link, content)

pan_green <- rbind(ltn_df, ftv_df)
pan_blue <- rbind(chinatimes_df, tvbs_df)
pan_green <- pan_green %>%
    arrange(desc(date)) %>%
    mutate(id = row_number()) %>%
    select(id, title, date, source, link, content)
pan_blue <- pan_blue %>%
    arrange(desc(date)) %>%
    mutate(id = row_number()) %>%
    select(id, title, date, source, link, content)

save(chinatimes_df, tvbs_df, ltn_df, ftv_df, pan_green, pan_blue, file = "Ractopamine_Pork.RData")
load("data/final/Ractopamine_Pork.RData")






