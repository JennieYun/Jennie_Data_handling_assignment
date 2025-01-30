
library(dplyr)
library(ggplot2)
library(readr)


##################### Visualize with Bar graph ##############
# 1. Load datasets
eruptions <- read_csv("tidytuesday:2020:2020-05-12:volcano/eruptions.csv") # eruptions data
events <- read_csv("tidytuesday:2020:2020-05-12:volcano/events.csv") # events data
tree_ring <- read_csv("tidytuesday:2020:2020-05-12:volcano/tree_rings.csv") # tree_ring data


# 2. Select relevant columns (start_year and vei)
eruptions_vei <- eruptions %>%
  select(start_year, vei) %>%
  filter(!is.na(vei))  # Only use rows with VEI values

# 3. VEI 값을 tree_ring 데이터셋에 추가
# Merge tree_ring data with eruptions data based on year
tree_ring_vei <- tree_ring %>%
  inner_join(eruptions_vei, by = c("year" = "start_year"))

# 4. VEI 카테고리 추가
# Categorize VEI values into specific categories
tree_ring_vei <- tree_ring_vei %>%
  mutate(vei_category = case_when(
    vei >= 5 ~ "VEI >= 5",  # VEI >= 5
    vei >= 3 ~ "VEI 3-4",    # VEI 3-4
    vei >= 1 ~ "VEI 1-2",    # VEI 1-2
    TRUE ~ "No VEI"         # No VEI value
  ))

# 5. VEI 카테고리별 Europe Temperature Index 평균 계산
# Calculate the average Europe Temperature Index for each VEI category
vei_temp_index <- tree_ring_vei %>%
  group_by(vei_category) %>%
  summarise(avg_temp_index = mean(europe_temp_index, na.rm = TRUE))

# Check the summary of the calculated averages
vei_temp_index

# 6. VEI 카테고리별 Europe Temperature Index 시각화
# Visualize the average Europe Temperature Index by VEI category using a bar chart
ggplot(vei_temp_index, aes(x = vei_category, y = avg_temp_index, fill = vei_category)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(
    title = "Average Europe Temperature Index by VEI Category",  # Title of the plot
    x = "VEI Category",  # X-axis label
    y = "Average Europe Temperature Index"  # Y-axis label
  ) +
  theme_minimal() +  # Minimal theme for the plot
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis text for better readability



######################## Visualize with line graph ############################

library(dplyr)
library(ggplot2)

# 1. 두 데이터셋을 불러옵니다
tree_ring <- read.csv("tree_ring_data.csv")  # tree_ring data
eruptions <- read.csv("eruptions_data.csv")  # eruptions data

# 2. eruptions 데이터셋에서 VEI 값 추출
# Select relevant columns (start_year and vei)
eruptions_vei <- eruptions %>%
  select(start_year, vei) %>%
  filter(!is.na(vei))  # Only use rows with VEI values

# 3. VEI 값을 tree_ring 데이터셋에 추가
# Merge tree_ring data with eruptions data based on year
tree_ring_vei <- tree_ring %>%
  inner_join(eruptions_vei, by = c("year" = "start_year"))

# 4. 1800년 이후 데이터만 필터링
tree_ring_vei_1800 <- tree_ring_vei %>%
  filter(year >= 1800)  # Filter data from 1800 onwards

View(tree_ring_vei_1800)

# 5. VEI 카테고리 추가
# Categorize VEI values into specific categories
tree_ring_vei_1800 <- tree_ring_vei_1800 %>%
  mutate(vei_category = case_when(
    vei >= 5 ~ "VEI >= 5",  # VEI >= 5
    vei >= 3 ~ "VEI 3-4",    # VEI 3-4
    vei >= 1 ~ "VEI 1-2",    # VEI 1-2
    TRUE ~ "No VEI"         # No VEI value
  ))

# 6. VEI 카테고리별 Europe Temperature Index 시각화 (라인 그래프)
# Visualize the fluctuation of Europe Temperature Index by VEI category for each year
ggplot(tree_ring_vei_1800, aes(x = year, y = europe_temp_index, color = vei_category, group = vei_category)) +
  geom_line(size = 1) +  # Use geom_line for line chart
  labs(
    title = "Fluctuation of Europe Temperature Index by VEI Category",  # Title of the plot
    x = "Year",  # X-axis label
    y = "Europe Temperature Index"  # Y-axis label
  ) +
  theme_minimal() +  # Minimal theme for the plot
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis text for better readability





####################### Visualize with bar and line graph ######################

# VEI Intensity vs Europe Temperature Index (Scaled)

# 1. Filter data for years after 1800
tree_ring_vei_1800 <- tree_ring_vei %>%
  filter(year >= 1800)

# 2. Add VEI category based on VEI values
tree_ring_vei_1800 <- tree_ring_vei_1800 %>%
  mutate(vei_category = case_when(
    vei >= 5 ~ "VEI >= 5",          # Category for VEI >= 5
    vei >= 3 ~ "VEI 3-4",           # Category for VEI 3-4
    vei >= 1 ~ "VEI 1-2",           # Category for VEI 1-2
    TRUE ~ "No VEI"                 # Category for no VEI
  ))

# 3. Calculate VEI intensity based on the count of events per year and category
vei_intensity <- tree_ring_vei_1800 %>%
  group_by(year, vei_category) %>%
  summarise(vei_count = n()) %>%
  mutate(vei_intensity_value = case_when(
    vei_category == "VEI >= 5" ~ vei_count * 10000,   # Multiply by 10000 for VEI >= 5
    vei_category == "VEI 3-4" ~ vei_count * 100,      # Multiply by 100 for VEI 3-4
    vei_category == "VEI 1-2" ~ vei_count * 1,        # Multiply by 1 for VEI 1-2
    TRUE ~ 0
  ))

# 4. Calculate total VEI intensity per year
vei_intensity_total <- vei_intensity %>%
  group_by(year) %>%
  summarise(total_intensity = sum(vei_intensity_value, na.rm = TRUE))

# 5. Join VEI intensity data with Europe Temperature Index by year
vei_temp_joined <- left_join(vei_intensity_total, tree_ring_vei_1800, by = "year")

# 6. Calculate the correlation between VEI intensity and Europe Temperature Index
correlation_result <- cor(vei_temp_joined$total_intensity, vei_temp_joined$europe_temp_index, use = "complete.obs")

# Print correlation result
print(paste("Correlation between VEI intensity and Europe Temperature Index: ", correlation_result))

# 7. Visualize VEI Intensity and Europe Temperature Index on the same plot
ggplot(vei_temp_joined, aes(x = year)) +
  # Bar graph for VEI Intensity
  geom_bar(aes(y = total_intensity, fill = "VEI Intensity"), stat = "identity", alpha = 0.6) +
  # Line graph for Europe Temperature Index
  geom_line(aes(y = europe_temp_index * 100000, color = "Europe Temperature Index"), size = 1) +  # Multiply temp index by 100000 to scale
  scale_y_continuous(
    name = "VEI Intensity",
    sec.axis = sec_axis(~ . / 100000, name = "Europe Temperature Index (°C)")  # Adjust scaling for second y-axis
  ) +
  scale_fill_manual(values = c("VEI Intensity" = "blue")) +  # Color for VEI Intensity
  scale_color_manual(values = c("Europe Temperature Index" = "red")) +  # Color for Temperature Index
  labs(
    title = "VEI Intensity and Europe Temperature Index Over Time",
    x = "Year",
    fill = "Legend",  # Fill legend title
    color = "Legend (scaled by 100,000)"  # Color legend title
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"  # Move legend to the top
  )
