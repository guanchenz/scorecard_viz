library(ggmap)
library(tidyverse)

# data <- read_csv('../data/scorecard.csv')
coords <- geocode('Fullerton College,')

data_filtered <- data %>%
  drop_na() %>%
  filter(!grepl('College', INSTNM) &
         !grepl('Campus', INSTNM) &
         !grepl('University-', INSTNM) &
         !grepl('Technical Institute', INSTNM) &
         !grepl('Institute-', INSTNM) &
         !grepl(' at ', INSTNM))

data_filtered[1:2,] %>%
  coords <- geocode(paste(INSTNM, STABBR, sep = ', ')) %>%

longs <- c()
lats <- c()
for (i in 1:nrow(data_filtered)) {
  addr <- paste(data_filtered[i,]$INSTNM, data_filtered[i,]$STABBR, sep = ', ')
  new_coords <- geocode(addr)
  longs <- c(longs, new_coords$lon)
  lats <- c(lats, new_coords$lat)
  Sys.sleep(1)
}

data_cleaned <- data_filtered %>%
  mutate(lon = longs,
         lat = lats) %>%
  drop_na()
