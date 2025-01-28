
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)

eruptions <- read_csv("tidytuesday:2020:2020-05-12:volcano/eruptions.csv")
events <- read_csv("tidytuesday:2020:2020-05-12:volcano/events.csv")
sulfur <- read_csv("tidytuesday:2020:2020-05-12:volcano/sulfur.csv")
tree_ring <- read_csv("tidytuesday:2020:2020-05-12:volcano/tree_rings.csv")
volcanos <- read_csv("tidytuesday:2020:2020-05-12:volcano/volcano.csv")


# Basic map of eruptions
ggplot(data = eruptions, aes(x = longitude, y = latitude, color = vei)) +
  borders("world", colour = "gray80", fill = "gray95") +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_gradient(low = "blue", high = "red", na.value = "gray50") +
  theme_minimal() +
  labs(title = "Volcano Eruptions by VEI",
    x = "Longitude",
    y = "Latitude",
    color = "VEI")



# n_tree와 europe_temp_index 간의 산점도
ggplot(tree_ring, aes(x = n_tree, y = europe_temp_index)) +
  geom_point(size=1,stroke=1, color = "grey40") +  # 산점도 추가
  geom_smooth(method = "lm", color = "red") + # 선형 회귀선 추가
  ggtitle(paste("Correlation between n_tree and Europe Temp Index"))+
  theme_minimal() +
  xlab("Number of Trees (n_tree)") +
  ylab("Europe Temperature Index (europe_temp_index)")


# eruption events 중에 기온 지표에 영향을 줄 수 있는 화산활동을 확인
str(events$event_type)
weather_affect_event <- unique(events$event_type)
print(weather_affect_event)



# 유럽 지역의 위도와 경도 범위 설정
european_volcanoes <- eruptions %>%
  filter(latitude >= 35 & latitude <= 71,   # 위도 범위: 유럽 대륙
         longitude >= -31 & longitude <= 40) # 경도 범위: 유럽 대륙

# 유럽 지역 화산 번호 추출
european_volcano_numbers <- european_volcanoes$volcano_number


# 유럽 화산 번호에 해당하는 event_type과 1900년 이후 활동만 추출
events_european <- events %>%
  filter(volcano_number %in% european_volcano_numbers) %>%
  filter(event_type %in% c("Explosion", "Ash", "Eruption cloud", "Ash Plume", "Tephra", "Volcanic smoke")) %>%
  filter(eruption_start_year >= 1900)  # 1900년 이후의 활동만 필터링

# 결과 확인
View(events_european)


# eruption_european에서 event_date_year를 기준으로 화산 활동 연도 추출
eruption_years <- events_european %>%
  select(volcano_number, event_date_year)

# tree_ring에서 해당 연도의 유럽 기온 지수 추출
european_temp_impact <- tree_ring %>%
  filter(year %in% eruption_years$event_date_year) %>%
  select(year, europe_temp_index)

# 해당 연도 동안의 유럽 기온 지수 확인
View(european_temp_impact)



