
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)

eruptions <- read_csv("tidytuesday:2020:2020-05-12:volcano/eruptions.csv")
events <- read_csv("tidytuesday:2020:2020-05-12:volcano/events.csv")
sulfur <- read_csv("tidytuesday:2020:2020-05-12:volcano/sulfur.csv")
tree_ring <- read_csv("tidytuesday:2020:2020-05-12:volcano/tree_rings.csv")
volcanos <- read_csv("tidytuesday:2020:2020-05-12:volcano/volcano.csv")



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
  filter(event_type %in% c("Explosion", "Ash", "Eruption cloud", "Ash Plume", "Tephra", "Volcanic smoke")) #%>%
  # filter(eruption_start_year >= 1900)  # 1900년 이후의 활동만 필터링

# 결과 확인
View(events_european)


# eruption_european에서 event_date_year를 기준으로 화산 활동 연도 추출
eruption_years <- events_european %>%
  select(volcano_number, eruption_start_year)

# tree_ring에서 해당 연도의 유럽 기온 지수 추출
european_temp_impact <- tree_ring %>%
  filter(year %in% eruption_years$eruption_start_year) %>%
  select(year, europe_temp_index)

# 해당 연도 동안의 유럽 기온 지수 확인
View(european_temp_impact)


## 화산 활동이 있었던 연도의 기온 지수 평균
avg_temp_impact_eruption <- mean(european_temp_impact$europe_temp_index, na.rm = TRUE)

# 일반 연도의 기온 지수 평균 (전체 기온 지수의 평균)
european_temp_impact_all <- tree_ring #%>%
  # filter(year >= 1900)

avg_temp_impact_all <- mean(european_temp_impact_all$europe_temp_index, na.rm = TRUE)

# 평균값 비교 출력
avg_temp_impact_eruption
avg_temp_impact_all



##### VEI 값별로 기온 변화량의 평균을 계산하고, 이를 바탕으로 화산 활동의 영향을 비교

# 1. 모든 지역의 화산 활동 데이터에서 VEI 값을 포함한 데이터 가져오기
# eruptions 데이터셋에서 VEI 값 포함한 데이터 가져오기
events_vei <- eruptions  # eruptions 데이터셋 사용

# 2. tree_ring 데이터에서 기온 변화량 계산 (현재 연도 - 이전 연도)
tree_ring <- tree_ring %>%
  arrange(year) %>%
  mutate(temp_change = europe_temp_index - lag(europe_temp_index))  # 기온 변화량 계산

# 3. tree_ring 데이터와 eruptions 데이터셋 결합 (year와 eruption_start_year 기준으로)
tree_ring_vei <- tree_ring %>%
  inner_join(events_vei, by = c("year" = "start_year")) %>%
  filter(!is.na(vei))  # VEI가 NA가 아닌 경우만 포함

# 결합된 데이터 확인
View(tree_ring_vei)


# 4. VEI 값에 따라 기온 변화량 계산
# VEI 값에 따른 카테고리 분류
tree_ring_vei <- tree_ring_vei %>%
  mutate(vei_category = case_when(
    vei >= 5 ~ "VEI >= 5",
    vei >= 3 ~ "VEI 3-4",
    vei >= 1 ~ "VEI 1-2",
    TRUE ~ "No VEI"  # VEI 값이 없는 경우 처리
  ))

# 5. VEI 카테고리별 기온 변화량 평균 계산
vei_temp_change <- tree_ring_vei %>%
  group_by(vei_category) %>%
  summarise(avg_temp_change = mean(temp_change, na.rm = TRUE))

# 6. 기온 변화량 시각화 (VEI 카테고리별)
library(ggplot2)
ggplot(vei_temp_change, aes(x = vei_category, y = avg_temp_change, fill = vei_category)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Average Temperature Change by VEI Category", x = "VEI Category", y = "Average Temperature Change (°C)") +
  theme_minimal()



install.packages("purrr")
library(purrr)
library(tidyr)
library(dplyr)
library(ggplot2)

# 1. 1800년 이후 데이터 필터링
tree_ring_1800 <- tree_ring %>%
  filter(year >= 1800)

events_vei_1800 <- eruptions %>%
  filter(start_year >= 1800)

# 2. 화산 활동 기간을 각 연도별로 확장 (end_year가 유효한 값인 경우만 처리)
events_vei_long <- events_vei_1800 %>%
  filter(!is.na(end_year)) %>%  # end_year가 NA인 행은 제외
  mutate(years_active = map2(start_year, end_year, ~seq(.x, .y))) %>%
  unnest(cols = years_active) %>%
  rename(year = years_active) %>%
  select(year, vei)  # 필요한 열만 추출

# 3. tree_ring 데이터와 결합 (inner_join)
tree_ring_vei_1800 <- tree_ring_1800 %>%
  inner_join(events_vei_long, by = "year")

# 4. VEI 값에 따른 기온 변화량 계산
tree_ring_vei_1800 <- tree_ring_vei_1800 %>%
  arrange(year) %>%
  mutate(temp_change = europe_temp_index - lag(europe_temp_index))

# VEI 카테고리별 기온 변화량 계산
tree_ring_vei_1800 <- tree_ring_vei_1800 %>%
  mutate(vei_category = case_when(
    vei >= 5 ~ "VEI >= 5",
    vei >= 3 ~ "VEI 3-4",
    vei >= 1 ~ "VEI 1-2",
    TRUE ~ "No VEI"
  ))

# 중복된 관측값 제거 (각 year 및 VEI 카테고리별로 temp_change 평균값 계산)
tree_ring_vei_1800_avg <- tree_ring_vei_1800 %>%
  group_by(year, vei_category) %>%
  summarise(temp_change = mean(temp_change, na.rm = TRUE), .groups = "drop")

# "No VEI" 카테고리는 제거
tree_ring_vei_1800_avg <- tree_ring_vei_1800_avg %>%
  filter(vei_category != "No VEI")

# 면적 그래프 (Area Plot)으로 VEI 카테고리별 기온 변화 시각화
ggplot(tree_ring_vei, aes(x = year, y = temp_change, fill = vei_category)) +
  geom_area(alpha = 0.6) +  # 면적 그래프
  scale_fill_manual(values = c("VEI >= 5" = "red", "VEI 3-4" = "yellow", "VEI 1-2" = "green")) +
  labs(
    title = "Temperature Change by VEI intensity (1800s onward)",
    x = "Year",
    y = "Temperature Change (°C)"
  ) +
  theme_minimal()

# # 박스 플롯 (Box Plot)으로 VEI 카테고리별 기온 변화 분포 시각화
# ggplot(tree_ring_vei, aes(x = vei_category, y = temp_change, fill = vei_category)) +
#   geom_boxplot() +  # 박스 플롯 생성
#   labs(
#     title = "Temperature Change by VEI Category (Box Plot)",
#     x = "VEI Category",
#     y = "Temperature Change (°C)"
#   ) +
#   scale_fill_manual(values = c("VEI >= 5" = "red", "VEI 3-4" = "yellow", "VEI 1-2" = "green")) +
#   theme_minimal()


# # VEI 카테고리별 기온 변화량 시각화 (선 그래프)
# ggplot(tree_ring_vei %>% filter(vei_category != "No VEI"), aes(x = year, y = temp_change, color = vei_category)) +
#   geom_line(size = 1) +  # 선 굵기 조정
#   geom_point(data = tree_ring_vei %>% filter(vei_category == "VEI >= 5"), 
#              aes(x = year, y = temp_change), size = 3, color = "red") +  # VEI >= 5 값 강조
#   labs(
#     title = "Temperature Change by Year and VEI Category",
#     x = "Year", 
#     y = "Temperature Change (°C)",
#     color = "VEI Category"
#   ) +
#   theme_minimal() +
#   scale_color_manual(values = c("VEI >= 5" = "red", "VEI 3-4" = "yellow", "VEI 1-2" = "green")) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))  # x축 텍스트 회전

# # VEI 카테고리별 기온 변화량 시각화 (VEI 5 이상 강조)
# ggplot(tree_ring_vei_1800_avg, aes(x = year, y = temp_change, color = factor(vei_category))) +
#   geom_line(aes(size = ifelse(vei_category == "VEI >= 5", 1.5, 1)), show.legend = FALSE) +  # VEI >= 5 강조
#   labs(
#     title = "Temperature Change by Year and VEI Category (VEI >= 5 Emphasized)",
#     x = "Year", 
#     y = "Temperature Change (°C)",
#     color = "VEI Category"
#   ) +
#   theme_minimal() +
#   scale_color_manual(values = c("VEI >= 5" = "red", "VEI 3-4" = "orange", "VEI 1-2" = "green")) +  # 색상 강조
#   scale_size_continuous(range = c(0.5, 1.5))  # 선 두께 조정



# # VEI 카테고리별 기온 변화량 시각화 (VEI 카테고리로 그룹화)
# ggplot(tree_ring_vei_1800, aes(x = year, y = temp_change, color = factor(vei_category))) +
#   geom_line() +
#   labs(
#     title = "Temperature Change by Year and VEI Category",
#     x = "Year",
#     y = "Temperature Change (°C)",
#     color = "VEI Category"
#   ) +
#   theme_minimal() +
#   scale_color_manual(values = c("VEI >= 5" = "red", "VEI 3-4" = "yellow", "VEI 1-2" = "green", "No VEI" = "gray"))  # 각 카테고리에 색상 지정

