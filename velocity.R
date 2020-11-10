
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
  mutate(start_time, end_time)


# ひらいてからの処理
data %>% 
  unnest(data) %>% 
  mutate(datetime = str_glue("{date} {time}")) %>% 
  mutate(datetime = parse_date_time(datetime, "dmY T*!"))
