# 光のキャリブレーション
# 
# 時系列データ
# 
# 流向流速のデータ使い方
#　
# 質問：
# 　流速の上限の扱い


library(tidyverse)
library(lubridate) # 時系列データの解析に必要


data = tibble(fnames = dir("Data", pattern = "[Cc][Ss][Vv]", full.names = TRUE))

data = data %>% 
  filter(str_detect(fnames, pattern = "Light_45_bise"))


data = data %>% 
  mutate(data = map(fnames, read_csv, 
                    skip = 9,
                    col_names = c("n", "date", "time", "raw", "ppfd")))

start_time = ymd_hms("2019-10-03 17:00:00")
end_time   = ymd_hms("2019-11-06 08:15:00")

data = data %>% 
  mutate(start_time,
         end_time)


# ひらいてからの処理
data %>% 
  unnest(data) %>% 
  mutate(datetime = str_glue("{date} {time}")) %>% 
  mutate(datetime = parse_date_time(datetime, "dmY T*!"))

# たたんだままの　data の処理　
data = data %>% 
  mutate(data = map(data, function(df) {
    df %>% 
      mutate(datetime = str_glue("{date} {time}")) %>% 
      mutate(datetime = parse_date_time(datetime, "dmY T*!"))
  })) %>% 
  unnest(data)
  

# 調査期間のフィルター
data = data %>% filter(between(datetime, left = start_time, right = end_time))

data

# 作図 

ggplot(data) +
  geom_point(aes(x = datetime, y = ppfd))



################### 瀬底データ 

sesoko = tibble(fnames = dir("Data", pattern = "[Cc][Ss][Vv]", full.names = TRUE))

sesoko = sesoko %>% 
  filter(str_detect(fnames, pattern = "Light_45_sesoko"))


sesoko = sesoko %>% 
  mutate(data = map(fnames, read_csv, 
                    skip = 9,
                    col_names = c("n", "date", "time", "raw", "ppfd")))

start_time = ymd_hms("2019-10-02 11:35:00")
end_time   = ymd_hms("2019-10-02 18:20:00")

sesoko = sesoko %>% 
  mutate(start_time,
         end_time)

# たたんだままの　data の処理　
sesoko = sesoko %>% 
  mutate(data = map(data, function(df) {
    df %>% 
      mutate(datetime = str_glue("{date} {time}")) %>% 
      mutate(datetime = parse_date_time(datetime, "dmY T*!"))
  })) %>% 
  unnest(data)


# 調査期間のフィルター
sesoko = sesoko %>% filter(between(datetime, 
                                   left = start_time, 
                                   right = end_time))
sesoko

# LiCor
calib = read_tsv(file = "Data/sesoko_calibration_191002.txt",
                 col_names = c("n", "datetime", "value"))

calib = calib %>% 
  filter(between(datetime, start_time, end_time)) %>% 
  mutate(ppfd = as.numeric(value)) %>% 
  select(datetime, ppfd)

#　ここまでは calib の単位は： mol/m2/5 minutes

# floor_* : 切り捨て
# ceiling_* : 切り上げ

# ここからは 10分間の積算光量子量 (mol/m2/10 minutes.)
calib = calib %>% 
  mutate(datetime = ceiling_date(datetime, "10 min")) %>% 
  group_by(datetime) %>% 
  summarise(ppfd = sum(ppfd))

sesoko = sesoko %>% 
  select(-ppfd) %>% 
  full_join(calib, by = "datetime")

# データの確認
ggplot(sesoko) +
  geom_point(aes(x = raw, y = ppfd)) +
  geom_smooth(
    aes(x = raw, y = ppfd),
    method = "lm",
    formula = y ~ x -1)


# calibration

calib_model = lm(ppfd ~ raw - 1 , data = sesoko)

data = data %>% 
  mutate(ppfd = predict(calib_model, newdata = .))

data = data %>% 
  mutate(ppfd = 1000000* ppfd / (10 * 60))

ggplot(data) + 
  geom_point(aes(x = datetime,
                 y = ppfd))




















