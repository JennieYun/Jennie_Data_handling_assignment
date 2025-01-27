
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)

eruptions <- read_csv("tidytuesday:2020:2020-05-12:volcano/eruptions.csv")
events <- read_csv("tidytuesday:2020:2020-05-12:volcano/events.csv")
sulfur <- read_csv("tidytuesday:2020:2020-05-12:volcano/sulfur.csv")
tree_ring <- read_csv("tidytuesday:2020:2020-05-12:volcano/tree_rings.csv")
volcanos <- read_csv("tidytuesday:2020:2020-05-12:volcano/volcano.csv")

View(eruptions) # volcano name & vei & coordinates
View(events) # volcano name & volcano type -> explosion type might effect weather/temp during that time
View(sulfur) # link sulfur and eruptions
View(tree_ring) # plot the correlation between eruption & n_tree + europe_temp_index

head(eruptions)
head(events)
head(sulfur)
head(tree_ring)


###### Tidy dataset ######

# 1. Sulfur and Tree_ring datasets have correlation, so combine these two would be helpful to analyze the data. But sulfur data is provided with decimal year, and tree_ring data presents yearly observation, so it is needed to do tidy up the sulfur data into yearly by 각 연도별 variables 항목별 평균을 내서


sulfur_year <- sulfur %>%
  filter(year >= 500 & year < 706) %>%   # 500 ~ 705.999 연도 필터링
  mutate(year = floor(year)) %>%          # 연도를 반내림 처리 (예: 501.999 -> 501)
  group_by(year) %>%                     # 연도별로 그룹화
  summarise(mean_neem = mean(neem, na.rm = TRUE), mean_wdc = mean(wdc, na.rm = TRUE))

sulfur_tree_ring <- sulfur_year %>%
  inner_join(tree_ring, by = "year") 

View(sulfur_tree_ring)


# Basic map of eruptions
ggplot(data = eruptions, aes(x = longitude, y = latitude, color = vei)) +
  geom_point(size = 3, alpha = 0.7) +
  borders("world", colour = "gray80", fill = "gray95") +
  scale_color_gradient(low = "blue", high = "red", na.value = "gray50") +
  theme_minimal() +
  labs(
    title = "Volcano Eruptions by VEI",
    x = "Longitude",
    y = "Latitude",
    color = "VEI"
  )


