
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)

eruptions <- read_csv("tidytuesday:2020:2020-05-12:volcano/eruptions.csv")
events <- read_csv("tidytuesday:2020:2020-05-12:volcano/events.csv")
sulfur <- read_csv("tidytuesday:2020:2020-05-12:volcano/sulfur.csv")
tree_ring <- read_csv("tidytuesday:2020:2020-05-12:volcano/tree_rings.csv")
volcanos <- read_csv("tidytuesday:2020:2020-05-12:volcano/volcano.csv")


# Basic map of eruptions over time

ggplot(data = eruptions, aes(x = longitude, y = latitude, color = vei)) +
  borders("world", colour = "gray80", fill = "gray95") +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_gradient(low = "blue", high = "red", na.value = "gray50") +
  theme_minimal() +
  labs(title = "Volcano Eruptions by VEI",
       x = "Longitude",
       y = "Latitude",
       color = "VEI")