
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



# n_tree와 europe_temp_index 간의 산점도
ggplot(tree_ring, aes(x = n_tree, y = europe_temp_index)) +
  geom_point(size=1,stroke=1, color = "grey40") +  # 산점도 추가
  geom_smooth(method = "lm", color = "red") + # 선형 회귀선 추가
  ggtitle(paste("Correlation between n_tree and Europe Temp Index"))+
  theme_minimal() +
  xlab("Number of Trees (n_tree)") +
  ylab("Europe Temperature Index (europe_temp_index)")

