
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

data

ggplot(data) + 
  geom_point(aes(x = datetime, y = velocity))

ggplot(data) + 
  geom_point(aes(x = datetime, y = temperature))


# 外れ値の処理
# 
data = data %>% 
  select(-fnames, -date, -hms, -start_time, -end_time) %>% 
  mutate(ng = (abs(vy) > 200 | abs(vx) > 200))

data =data  %>% 
  mutate(velocity =ifelse(ng, NA, velocity)) %>%
  mutate(dir =ifelse(ng, NA, dir)) %>% 
  mutate(ew =ifelse(ng, NA, ew)) %>% 
  mutate(ns =ifelse(ng, NA, ns))

ggplot(data) + 
  geom_point(aes(x = datetime, y = velocity))
  


# 統計量

# 一日あたり
floor_date(end_time, "day") - minutes(1)

data = data %>% 
  filter(between(datetime,
                 ceiling_date(start_time, "day"),
                 floor_date(end_time, "day") - minutes(1))) %>% 
  mutate(date = floor_date(datetime, "day"))


data %>% 
  group_by(date) %>% 
  summarise(velocity = mean(velocity, na.rm = T))


data %>% 
  group_by(date) %>% 
  summarise(across(c(velocity, temperature,
                     ew, ns),
                   ~mean(.x, na.rm = T)))


data_summary = data %>% 
  group_by(date) %>% 
  summarise(across(c(velocity, temperature,
                     ew, ns),
                   list(mean = ~mean(.x, na.rm=T),
                        max = ~max(.x, na.rm=T),
                        sd = ~sd(.x, na.rm=T))))
data_summary %>%   tail()


ggplot(data_summary) +
  geom_col(aes( x = date,
                y = velocity_mean)) +
  geom_errorbar(aes(x = date,
                    ymin =velocity_mean,
                    ymax = velocity_mean + velocity_sd),
                width = 0)










  



