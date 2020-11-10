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


fnames = dir("Data", pattern = "[Cc][Ss][Vv]", full.names = TRUE)
