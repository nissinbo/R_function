library(tidyverse)
library(lubridate)
library(AdhereR)


# event date 特定 -------------------------------------------------------------------

df_sample <- med.events %>% 
  filter(CATEGORY == "medA") %>%   # 簡単のため medA に限定
  mutate(id = PATIENT_ID, startdate = mdy(DATE), days_presc = DURATION, .keep = "none") %>%   # 列名変更、フォーマット、列選択
  mutate(enddate = startdate + days_presc, .after = "startdate") %>%   # startdate に days_presc を足して飲み終わりの日を出す
  mutate(interval_presc = interval(startdate, enddate)) %>%   # 薬剤使用期間を interval の形で保持
  arrange(id, startdate, enddate)

df <- df_sample %>% 
  filter(id == 4)

# persistencyが被ってたらひとまとめにしたベクトルを返す関数
# method: PDC or MPR
int_merge <- function(vec, method = "pdc", tolerate_adjacent = TRUE, take_today = FALSE) {
  if (is.unsorted(int_start(vec))) {
    warning("vector is unsorted !")
  }
  accumulate(vec, function(x, y) {
    condition <- if (tolerate_adjacent) {
      int_overlaps(x, y) | int_end(x) == int_start(y)
    } else {
      int_overlaps(x, y)
    }
    
    if (condition) {
      if (method == "pdc") {
        res <- interval(
          min(int_start(x), int_start(y)), 
          max(int_end(x), int_end(y)) - days(take_today)
        )
      }
      if (method == "mpr") {
        res <- interval(
          min(int_start(x), int_start(y)), 
          max(int_end(x), int_end(y)) - days(take_today)
          + days(interval(int_start(y), min(int_end(x), int_end(y))) %/% days(1)))
      }
    }
    else { res <- y }
    return(res)
  }
  )
}

# PDC
res_pdc_individual <- df %>% 
  mutate(merged_date = int_merge(interval_presc))

res_pdc <- df_sample %>% 
  group_by(id) %>% 
  mutate(merged_date = int_merge(interval_presc))

# MPR
res_mpr_individual <- df %>% 
  mutate(merged_date = int_merge(interval_presc, "mpr"))

# 打ち切りを出すにはleadとって差を出す
res_censor_individual <- res_pdc_individual %>% 
  filter(lead(int_start(merged_date)) - int_end(merged_date) > 30)

res_censor <- res_pdc %>% 
  group_by(id) %>% 
  filter(lead(int_start(merged_date)) - int_end(merged_date) > 30)

# アドヒアランス出すにはid, startdate, desc(enddate)で並べ替えて重複削除して割合を出す
res_adherence <- res_pdc %>% 
  arrange(id, int_start(merged_date), desc(int_end(merged_date))) %>% 
  distinct(id, int_start(merged_date), .keep_all = TRUE) %>% 
  group_by(id) %>% 
  summarise(total = int_length(interval(int_start(first(merged_date)), int_end(last(merged_date)))), filled = sum(int_length(merged_date)), pdc = filled / total, .groups = "drop")
