library(tidyverse)
vegetables <- readr::read_csv(
  "vegetables.csv",
  col_types = cols(store = "c")
) %>%
  as.data.frame()

save(
  vegetables,
  file = "../CSwR_package/data/vegetables.RData",
  compress = "xz"
)
