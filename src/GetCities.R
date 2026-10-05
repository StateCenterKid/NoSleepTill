library(dplyr)
library(maps)

# Create the 'input' folder if it doesn't exist
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

message("Injecting Buc-ee's coordinates...")
# A representative spread of major Buc-ee's locations across the country
bucees_data <- data.frame(
  clean_name = "Buc-ee's",
  state = c("TX", "TX", "TX", "TX", "TX", "TX", "AL", "AL", "AL", "FL", "FL", "GA", "GA", "SC", "TN", "TN", "KY", "CO", "MO", "TX"),
  lat = c(29.7265, 29.7820, 30.1118, 30.9327, 31.1350, 29.6738, 30.6015, 33.5414, 34.7831, 29.2307, 29.9880, 32.6133, 34.4851, 34.2045, 35.9752, 35.9328, 37.7171, 40.3255, 37.2514, 32.7410),
  long = c(-98.0772, -95.8239, -97.3364, -95.9015, -97.3622, -97.6621, -87.7289, -86.5492, -86.9174, -81.0853, -81.4746, -83.6938, -84.9392, -79.7266, -85.0069, -83.5855, -84.3010, -104.9754, -93.2389, -96.3015)
)

# Bind the Buc-ee's data into the master city list
all_cities <- bind_rows(all_cities, bucees_data)

message("Calculating top 10 most common city names...")
top_10_names <- all_cities %>%
  filter(clean_name != "Buc-ee's") %>% # Keep it out of the general top 10 math just in case
  count(clean_name) %>%
  arrange(desc(n)) %>%
  head(10) %>%
  pull(clean_name)

# Ensure Buc-ee's, Brooklyn, and State Center are always available in the dropdown
dropdown_choices <- unique(c("Buc-ee's", "Brooklyn", "State Center", top_10_names))

message("Generating county map polygons and centroids...")
counties <- map_data("county")
county_centroids <- counties %>%
  group_by(region, subregion) %>%
  summarize(cent_long = mean(long), cent_lat = mean(lat), .groups = 'drop')

message("Saving everything to input/map_data.RData...")
save(all_cities, dropdown_choices, counties, county_centroids, file = "input/map_data.RData")

message("Done! You can now run your Shiny app.")