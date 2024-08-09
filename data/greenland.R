# Script to read and store greenland temperature data in package
library(tidyverse)

nuuk <- read_table("nuuk.dat.txt", 
  col_names = c("Year", 1:12), 
  na = "-999", 
  skip = 1) %>% 
  gather(key = "Month", value = "Temp_nuuk", -Year, convert = TRUE) %>% 
  mutate(Temp_nuuk = Temp_nuuk / 10) %>% 
  filter(Year > 1866)

Qaqortoq <- read_table("qaqortoq.dat", 
  col_names = c("Year", 1:12), 
  na = "-999", 
  skip = 1) %>% 
  gather(key = "Month", value = "Temp_Qaqortoq", -Year, convert = TRUE) %>% 
  mutate(Temp_Qaqortoq = Temp_Qaqortoq / 10) %>% 
  filter(Year > 1866)

greenland <- left_join(nuuk, Qaqortoq) %>% 
  filter(complete.cases(.))  %>% 
  mutate(Temp_diff = Temp_nuuk - Temp_Qaqortoq) %>% 
  as.data.frame()

Qaqortoq <- group_by(Qaqortoq, Year) %>% 
  summarise(
    Median = median(Temp_Qaqortoq),
    High = max(Temp_Qaqortoq), 
    Low = min(Temp_Qaqortoq),
    Temperature = mean(Temp_Qaqortoq),
    Range = High - Low
  )

nuuk <- group_by(nuuk, Year) %>% 
  summarise(
    Temperature = mean(Temp_nuuk),
    Median = median(Temp_nuuk),
    High = max(Temp_nuuk), 
    Low = min(Temp_nuuk),
    Range = High - Low
  ) %>% 
  as.data.frame()

save(greenland, file = "../CSwR_package/data/greenland.RData")
save(nuuk, file = "../CSwR_package/data/nuuk.RData")


## Some figures

Qaqortoq$place = "Qaqortoq"
nuuk$place <- "nuuk"
greenland_agg <- rbind(nuuk, Qaqortoq)

ggplot(mapping = aes(Qaqortoq$Low, nuuk$Low)) + 
  geom_point() + geom_smooth() + geom_smooth(method = "lm", se = FALSE, color = "red")

ggplot(greenland_agg, aes(Temperature, High)) + geom_point() + 
  geom_smooth() + geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_smooth(method = "gam", formula = y ~s(x, bs = "cr"), se = FALSE, color = "purple") +
  facet_wrap("place")





