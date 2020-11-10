
library(tidyverse)
library(lubridate) # 時系列データの解析に必要


data = tibble(fnames = dir("Data", pattern = "[Cc][Ss][Vv]", full.names = TRUE))

data = data %>% filter(str_detect(fnames, pattern = "Flow_733"))


get_cols = cols_only(`YYYY/MM/DD` = col_character(),
          `hh:mm:ss`  = col_character(),
          `Velo[cm/s]` = col_double(),
          `Dir[Deg]` = col_double(),
          `Vel EW[cm/s]` = col_double(),
          `Vel NS[cm/s]` = col_double(),
          `Temp[ﾟC]` = col_double(),
          `Vel X[cm/s]` = col_double(),
          `Vel Y[cm/s]` = col_double())

data = data %>% 
  mutate(data = map(fnames, read_csv, 
                    skip = 36,
                    locale = locale(encoding = "CP932"),
                    col_types = get_cols))
data %>% unnest(data)

fn = function(x) c("date", 
                   "hms", 
                   "velocity", 
                   "dir",
                   "ew",
                   "ns",
                   "temperature",
                   "vx", "vy")

data = data %>% 
  mutate(data = map(data, function(df) {
    df %>% rename_with(fn)
  })) %>% 
  unnest(data)

data

data = data %>% mutate(datetime = str_glue("{date} {hms}")) %>% 
  mutate(datetime = parse_date_time(datetime, "ymd T*!"))

start_time = ymd_hms("2019-10-03 17:00:00")
end_time   = ymd_hms("2019-11-06 08:15:00")

data = data %>% 
  mutate(start_time, end_time)


ggplot(data) + 
  geom_point(aes(x = datetime, y = velocity))
