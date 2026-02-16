library(tidyverse)
news <- readr::read_csv("OnlineNewsPopularity.csv")
news <- dplyr::select(
  news,
  -url,
  -timedelta,
  -is_weekend,
  -n_non_stop_words,
  -n_non_stop_unique_tokens,
  -self_reference_max_shares,
  -kw_min_max
) %>%
  as.data.frame()

save(news, file = "../CSwR_package/data/news.RData", compress = "xz")
