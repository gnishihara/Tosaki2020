library(tidyverse)
library(lubridate)

##### 関数の定義 ################################################################
read_cem = function(filename, ...) {
  # 魔法の関数。CEMデータを読み込んでくれます。さらに、外れ値をNAにしてくれます。
  test_file = system(paste("file --brief", filename), intern = TRUE)
  if(grepl("xlsx", tools::file_ext(filename))){
    # If it is xlsx, read as an xlsx file.
    id = read_xlsx(filename, range = "A13")
    out = read_xlsx(filename, skip = 36)
  } else if(grepl("extended-ASCII", test_file)) {
    # If it is csv, read as a csv file.
    id = read_lines(filename, skip = 12, n_max = 1)
    out = suppressMessages(read_csv(filename, skip = 36, locale = locale(encoding = "CP932")))
  } else if(grepl("CSV", test_file)) {
    id = read_lines(filename, skip = 12, n_max = 1)
    out = suppressMessages(read_csv(filename, skip = 36))
  } else {
    # Exit if not either.
    stop(paste(filename, "is not a readable file."))
  }
  # Add two attributes to the data frame. One for the type of data (CKU or CEM)
  # and one for the filename.
  attributes(out)$loggertype = str_extract(id, "T[GK].*[B]")
  attributes(out)$filename = basename(filename)
  out = out %>%
    select(ymd = matches("YYYY"),
           hms = matches("hh:mm"),
           speed = matches("Velo"),
           dir = matches("Dir"),
           ew = matches("EW"),
           ns = matches("NS"),
           vx = matches("Vel X\\["),
           vy = matches("Vel Y\\["),
           temperature = matches("Temp\\[")) %>%
    mutate(datetime = paste(.data$ymd, .data$hms)) %>%
    mutate(datetime = ymd_hms(.data$datetime)) %>%
    mutate_at(vars(matches("dir")), ~(. / 360 * 2*pi)) %>%
    select(-.data$ymd, -.data$hms) %>%
    select(.data$datetime, everything()) %>%
    mutate_at(vars(matches("vx|vy")), ~(ifelse(abs(.) > 200, NA, .))) %>%
    mutate_at(vars(matches("temperature")), ~(ifelse(((. < 0) | (. > 40)), NA, .)))
  
  chk = colnames(out)
  if("vx" %in% chk) {
    out %>%
      mutate_at(vars(-matches("datetime")), ~(ifelse((is.na(vx)|is.na(vy)|is.na(temperature)), NA, .))) %>%
      select(-matches("vx|vy"))
  } else {
    out
  }
}

##### geosphere パッケージが必要です。 #####
# GPS データから2点の直線距離を計算してくれます。
calc_dist = function(X,Y) {
  # 単位は m です。
  geosphere::distGeo(X$value[c(2,1)],Y$value[c(2,1)]) 
}

# GPS データから2点のベアリングを計算してくれます。
calc_bear = function(X,Y) {
  # 単位は degrees (0 から 360) です。
  geosphere::bearing(X$value[c(2,1)],Y$value[c(2,1)]) 
}
#######################################################


# サイトごとのID番号を使って、south, north, west, churaumi などのラベルを付けます。
chksite = function(id) {
  case_when(
    id == "15" ~ "south",
    id == "44" ~ "north",
    id == "60" ~ "west",
    id == "00" ~ "churaumi")
}

## GPSデータをDMSからD.xyz に変換する。
dms2dd = function(x) {
  dd = as.numeric(str_sub(x, 1, str_locate(x, "d")[,1]-1))
  dm = as.numeric(str_sub(x, str_locate(x, "d")[,1]+1, str_locate(x, "'")[,1]-1))/60
  ds = as.numeric(str_sub(x, str_locate(x, "'")[,1]+1, str_locate(x, "\"")[,1]-1))/60/60
  dd+dm+ds
}
#######################################################################################################################

# ロガーのGPS情報の処理

tosakigps = 
  tibble(
    site = c("north",          "west",             "south"),
    lat = c("26d42\'32.79\"",  "26d42\'29.39\"",  "26d42\'29.08\""),
    lon = c("127d52\'39.78\"", "127d52\'39.80\"", "127d52\'42.01\"")
  )

tosakigps = tosakigps %>% 
  mutate(across(c(lat,lon), dms2dd))

station_parameters = tosakigps %>% 
  select(site, lat, lon) %>% 
  pivot_longer(c(lat,lon)) %>% 
  group_nest(site) %>% 
  pivot_wider(names_from = site, values_from = data) %>% 
  mutate(ns_distance = map2_dbl(north, south, calc_dist)) %>% 
  mutate(nw_distance = map2_dbl(north, west, calc_dist)) %>% 
  mutate(sw_distance = map2_dbl(south, west, calc_dist)) %>% 
  mutate(ns_bearing = map2_dbl(north, south, calc_bear)) %>% 
  mutate(nw_bearing = map2_dbl(north, west, calc_bear)) %>% 
  mutate(sw_bearing = map2_dbl(south, west, calc_bear)) %>% 
  select(-c(north, west,south)) %>% 
  pivot_longer(everything()) %>% 
  separate(name, c("point", "type")) %>% 
  mutate(value = ifelse(str_detect(type, "distance"), value * 100, value)) %>% 
  mutate(value = ifelse(str_detect(type, "bearing") & str_detect(point, "ns|nw"), 180-value, value)) %>% 
  mutate(value = ifelse(str_detect(type, "bearing") & str_detect(point, "sw"), 90+value, value))


station_parameters = station_parameters %>% 
  pivot_wider(names_from = type, values_from = value) %>% 
  mutate(data = map2(distance, bearing, function(d,b) {
    x = d * sin(b / 180 * pi) # NS方向の距離
    y = d * cos(b / 180 * pi) # EW方向の距離
    tibble(x, y)
  })) %>% unnest(data) %>% 
  select(point, x, y)



# 戸崎流向流速データの読み込みと処理
# 
FOLDER = "~/Dropbox/備瀬2019-2020/戸崎データ/"

cemdata = tibble(fnames = dir(FOLDER,
                              recursive = TRUE, 
                              full.names = TRUE, 
                              pattern = c("[Cc][Ss][Vv]"))) %>% 
  mutate(bname = basename(fnames),
         type = str_extract(bname, "[:alpha:]+"),
         id = str_extract(bname, "[:digit:]+"),
         site = str_extract(str_extract(bname, "_s.*h|_n.*h|_w.*t"), "[:alpha:]+")) %>% 
  filter(str_detect(type, "Flow")) %>% 
  mutate(site = ifelse(!is.na(site), site, chksite(id))) %>% 
  mutate(data = map(fnames, read_cem)) %>% 
  select(site, data) %>% unnest(data)

cemdata # データの形を確認してね。

# 下記tibbleの例
################################################################################
# # A tibble: 960,220 x 7
# site  datetime            speed    dir    ew    ns temperature
# <chr> <dttm>              <dbl>  <dbl> <dbl> <dbl>       <dbl>
#   1 north 2019-10-03 13:00:00  19.9 2.21    15.9 -11.9        26.9
# 2 north 2019-10-03 13:00:01   9.8 1.52     9.8   0.5        26.9
# 3 north 2019-10-03 13:00:02  37   5.78   -17.9  32.4        26.9
# 4 north 2019-10-03 13:00:03   8.9 4.64    -8.9  -0.7        26.9
# 5 north 2019-10-03 13:00:04  37.7 2.47    23.3 -29.6        26.9
# 6 north 2019-10-03 13:00:05  20   0.0768   1.5  20          26.9
# 7 north 2019-10-03 13:00:06  40.5 5.45   -29.9  27.2        26.9
# 8 north 2019-10-03 13:00:07  23.1 2.63    11.3 -20.1        26.9
# 9 north 2019-10-03 13:00:08  21.4 0.0471   1    21.3        26.9
# 10 north 2019-10-03 13:00:09  19.5 5.34   -15.7  11.5        26.9
# # … with 960,210 more rows
################################################################################




################################################################################
# カール（curl, 回転）の計算
# 回転が正のときは半時計周り、負のときは時計回り。

cemdata = cemdata %>% 
  filter(datetime < ymd_h("2020-12-1 0")) # 誤ったデータがあったので、外す。


prepare_data = function(df) {
  df %>% 
    select(site, ew, ns, datetime) %>% 
    pivot_longer(cols = c(ew,ns),
                 names_to = "direction") %>% 
    pivot_wider(names_from = site,
                values_from = value) %>% 
    mutate(ns_value = north - south,
           nw_value = north - west,
           sw_value = south - west) %>% 
    select(datetime, direction, ns_value, nw_value, sw_value) %>% 
    pivot_longer(-c(datetime, direction))
}

calculate_curl = function(df) {
  df  %>% 
    mutate(point = str_extract(name, "ns|nw|sw")) %>% 
    select(-name) %>% 
    pivot_wider(names_from = direction,
                values_from = value) %>% 
    left_join(station_parameters, by = c("point")) %>% 
    mutate(curl = (ns / x) - (ew / y))
}

curldata = cemdata %>% prepare_data()

curldata = curldata %>% calculate_curl()


curldata %>% 
  mutate(month = month(datetime),
         hour = hour(datetime)) %>% 
  group_by(month, hour, point) %>% 
  summarise(across(curl,  list(mean = ~mean(.x, na.rm=T), 
                               sd =   ~sd(.x, na.rm=T)))) %>% 
  ggplot() +
  stat_summary(aes(x = month, y = curl_mean/2/pi),
               fun = mean,
               geom = "point")











