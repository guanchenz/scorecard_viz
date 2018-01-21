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

get_year <- Vectorize(function(x) {
  if (x == 'mn_earn_wne_p6') {
    return(6)
  } else if (x == 'mn_earn_wne_p7') {
    return(7)
  } else if (x == 'mn_earn_wne_p8') {
    return(8)
  } else if (x == 'mn_earn_wne_p9') {
    return(9)
  } else if (x == 'mn_earn_wne_p10') {
    return(10)
  }
})

data_gathered <- data_cleaned %>%
  gather(`mn_earn_wne_p6`, `mn_earn_wne_p7`, `mn_earn_wne_p8`,
         `mn_earn_wne_p9`, `mn_earn_wne_p10`, key = "year", value = "earning") %>%
  mutate(year = get_year(year))
