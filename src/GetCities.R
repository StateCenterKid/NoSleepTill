library(dplyr)
library(maps)

# Create the 'input' folder if it doesn't exist in your working directory
if (!dir.exists("input")) {
  dir.create("input")
}

message("Fetching city data from GitHub...")
cities_url <- "https://raw.githubusercontent.com/kelvins/US-Cities-Database/main/csv/us_cities.csv"
all_cities_raw <- read.csv(cities_url, stringsAsFactors = FALSE)

message("Cleaning city data...")
all_cities <- all_cities_raw %>%
  filter(!is.na(LATITUDE), !is.na(LONGITUDE)) %>%
  transmute(
    clean_name = CITY,
    state = STATE_CODE,
    lat = as.numeric(LATITUDE),
    long = as.numeric(LONGITUDE)
  ) %>%
  distinct(clean_name, lat, long, .keep_all = TRUE)

message("Calculating top 10 most common city names...")
top_10_names <- all_cities %>%
  count(clean_name) %>%
  arrange(desc(n)) %>%
  head(10) %>%
  pull(clean_name)

# Ensure Brooklyn and State Center are always available
dropdown_choices <- unique(c("Brooklyn", "State Center", top_10_names))

message("Generating county map polygons and centroids...")
counties <- map_data("county")
county_centroids <- counties %>%
  group_by(region, subregion) %>%
  summarize(cent_long = mean(long), cent_lat = mean(lat), .groups = 'drop')

message("Saving everything to input/map_data.RData...")
save(all_cities, dropdown_choices, counties, county_centroids, file = "input/map_data.RData")

message("Done! You can now run your Shiny app.")