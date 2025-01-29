
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

ggplot(data = eruptions %>% filter(!is.na(vei)), aes(x = longitude, y = latitude, color = vei)) +
  borders("world", colour = "gray80", fill = "gray95") +
  geom_point(size = 2, alpha = 0.5) +  # NA 값이 이미 제거된 상태
  scale_color_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(title = "Map of Volcano Eruptions with VEI Scale",
       x = "Longitude",
       y = "Latitude",
       color = "VEI")


# Map of eruptions 1800s onwards

ggplot(data = eruptions %>% 
         filter(!is.na(vei), start_year >= 1800), aes(x = longitude, y = latitude, color = vei)) +
  borders("world", colour = "gray80", fill = "gray95") +
  geom_point(size = 2, alpha = 0.5) +  # NA 값이 이미 제거된 상태
  scale_color_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(title = "Volcano Eruptions (1800s onwards) with VEI Scale",
       x = "Longitude",
       y = "Latitude",
       color = "VEI")

