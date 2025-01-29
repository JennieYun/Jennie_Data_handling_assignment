
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)

eruptions <- read_csv("tidytuesday:2020:2020-05-12:volcano/eruptions.csv")
events <- read_csv("tidytuesday:2020:2020-05-12:volcano/events.csv")
sulfur <- read_csv("tidytuesday:2020:2020-05-12:volcano/sulfur.csv")
tree_ring <- read_csv("tidytuesday:2020:2020-05-12:volcano/tree_rings.csv")
volcanos <- read_csv("tidytuesday:2020:2020-05-12:volcano/volcano.csv")

#1900-2020년 사이에 n_tree 혹은 europe index가 음수였던 데이터를 필터링한 후 그 연도에 다른 연도보다 더 많은 화산활동이 많았다는 것을 보여줘도 좋을 듯

View(eruptions) # volcano name & vei & coordinates
View(events) # volcano name & volcano type -> explosion type might effect weather/temp during that time
View(sulfur) # link sulfur and eruptions
View(tree_ring) # plot the correlation between eruption & n_tree + europe_temp_index



###### Tidy dataset ######

# 1. Sulfur and Tree_ring datasets have correlation, so combine these two would be helpful to analyze the data. But sulfur data is provided with decimal year, and tree_ring data presents yearly observation, so it is needed to do tidy up the sulfur data into yearly by 각 연도별 variables 항목별 평균을 내서

sulfur_year <- sulfur %>%
  filter(year >= 500 & year <= 706) %>% 
  mutate(year = floor(year)) %>%          # 연도를 반내림 처리 (예: 501.999 -> 501)
  group_by(year) %>%                     # 연도별로 그룹화
  summarise(mean_neem = mean(neem), mean_wdc = mean(wdc))

sulfur_tree_ring <- sulfur_year %>%
  inner_join(tree_ring, by = "year") 

View(sulfur_tree_ring)


# 2.  tree_ring 데이터를 pivot_longer()를 사용하여 n_tree와 europe_temp_index를 하나의 measurement 변수로 합쳐서 하나의 긴 데이터로 변환할 수 있습니다.

measurement <- sulfur_tree_ring %>%
pivot_longer(cols = c(n_tree, europe_temp_index, mean_neem, mean_wdc), 
             names_to = "measurement", values_to = "value")

View(measurement)


