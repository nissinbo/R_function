library(tidyverse)
library(lubridate)

# startdate: 処方日
# presc_days: 処方日数
# enddate: 投与終了日 (処方日 + 処方日数)
# persistency: startdate と enddate を lubridate::interval で保持
df_sample <- read.csv(text = "
              startdate,presc_days
              2022-03-09,180
              2022-03-10,40
              2022-06-27,60
              2023-03-01,10") %>% 
  mutate(startdate = ymd(startdate)) %>% 
  mutate(enddate = startdate + days(presc_days)) %>% 
  mutate(persistency = interval(startdate, enddate))

# このデータより、「persistencyが30日以上間隔が空いた場合、persistency終了日で打ち切り」と定義して打ち切り日を計算したい。
# 本来の打ち切りは1行目のpersistency終了日だが、単純に「隣り合う行の比較で30日以上離れている」を打ち切りとすると、2行目と3行目の間が空いていることで2行目のpersistency終了日が打ち切りになる。

a <- df_sample %>% 
  mutate(censor = if_else(lead(startdate) - enddate > 30, "1", "0"))

# よって、各行を上から見ていき「persistencyが重複してる場合は併合して1つのpersistencyとみなし、重複してない場合は新たなpersistencyとして設定する」という総当たりに近い方法をとる必要がある。

# 自作関数
fun <- function(x, y) {
  if (int_overlaps(x, y) & int_end(x) != int_start(y)) {
    res <- interval(
      min(int_start(x), int_start(y)), 
      max(int_end(x), int_end(y))
    )
  }
  else { res <- y }
  return(res)
}

# 打ち切りを算出
df_res <- df_sample %>% 
  mutate(merged_date = accumulate(persistency, fun)) %>% 
  mutate(censor = if_else(int_end(merged_date) %m+% days(30) < int_start(lead(merged_date)), 
                          "1", "0"))
