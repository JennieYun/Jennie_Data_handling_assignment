
library(readr)
eruptions <- read_csv("tidytuesday:2020:2020-05-12:volcano/eruptions.csv")
events <- read_csv("tidytuesday:2020:2020-05-12:volcano/events.csv")
sulfur <- read_csv("tidytuesday:2020:2020-05-12:volcano/sulfur.csv")
tree_ring <- read_csv("tidytuesday:2020:2020-05-12:volcano/tree_rings.csv")
volcanos <- read_csv("tidytuesday:2020:2020-05-12:volcano/volcano.csv")

View(eruptions) # volcano name & vei & coordinates
View(events) # volcano name & volcano type -> explosion type might effect weather/temp during that time
View(sulfur) # neen=greenland, wdc=antartica, so if sulfur was reached to both area, eruption was big
View(tree_ring) # plot the correlation between eruption & n_tree + europe_temp_index
View(volcanos) 


eruptions_by_year <- eruptions %>%
  group_by(start_year) %>%
  summarize(total_eruptions = n())


