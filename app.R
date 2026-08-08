library(shiny)
library(ggplot2)
library(dplyr)
library(sf) # Using the modern spatial package instead of mapproj
library(geosphere)
library(bslib)

# ==========================================
# 1. LOAD DATA & PRE-PROJECT
# ==========================================
load("input/map_data.RData")

# True Geographic Projection using sf
# EPSG: 4326 is standard GPS Longitude/Latitude
# EPSG: 5070 is the official Continental US Albers Equal Area projection (in meters)
apply_albers <- function(lon, lat, reverse = FALSE) {
  if (!reverse) {
    # Convert Lon/Lat -> Albers Meters
    pts <- st_as_sf(data.frame(lon = lon, lat = lat), coords = c("lon", "lat"), crs = 4326)
    pts_proj <- st_transform(pts, crs = 5070)
    coords <- st_coordinates(pts_proj)
    return(data.frame(x = coords[,1], y = coords[,2]))
  } else {
    # Convert Albers Meters -> Lon/Lat
    pts <- st_as_sf(data.frame(x = lon, y = lat), coords = c("x", "y"), crs = 5070)
    pts_proj <- st_transform(pts, crs = 4326)
    coords <- st_coordinates(pts_proj)
    return(data.frame(x = coords[,1], y = coords[,2]))
  }
}

# Project the massive county base map exactly once on startup to keep the app fast
proj_counties <- apply_albers(counties$long, counties$lat)
counties$x_proj <- proj_counties$x
counties$y_proj <- proj_counties$y

# ==========================================
# 2. SHINY UI
# ==========================================
ui <- fluidPage(
  theme = bs_theme(bg = "#121212", fg = "#FFFFFF", primary = "#1DB954"),
  
  titlePanel("No. Sleep. Til..."),
  
  sidebarLayout(
    sidebarPanel(
      width = 3, 
      selectInput(
        inputId = "cityName", 
        label = "Where you going", 
        choices = dropdown_choices,
        selected = "Brooklyn"
      ),
      hr(),
      p("How much sleep are you going to lose until you reach the nearest ___?"),
      hr(),
      h4(textOutput("clickResult"), style = "color: #1DB954; font-weight: bold;"),
      hr(),
      br(),
      tags$small("City data sourced from ", 
                 tags$a(href="https://github.com/kelvins/US-Cities-Database", 
                        "kelvins/US-Cities-Database", 
                        target="_blank", 
                        style="color: #1DB954; text-decoration: none; font-weight: bold;"))
    ),
    
    mainPanel(
      width = 9, 
      plotOutput("travelMap", height = "85vh", click = "map_click") 
    )
  )
)

# ==========================================
# 3. SHINY SERVER
# ==========================================
server <- function(input, output) {
  
  target_cities <- reactive({
    all_cities %>% filter(clean_name == input$cityName)
  })
  
  # Render the Map
  output$travelMap <- renderPlot({
    target_name <- input$cityName
    targets <- target_cities() 
    
    if(nrow(targets) == 0) {
      return(ggplot() + theme_void() + ggtitle("City coordinates not found."))
    }
    
    # Run target dots through our Albers projection so they align with the map
    proj_targets <- apply_albers(targets$long, targets$lat)
    targets$x_proj <- proj_targets$x
    targets$y_proj <- proj_targets$y
    
    # Distance is still calculated using TRUE long/lat for accuracy
    county_centroids$min_dist_miles <- sapply(1:nrow(county_centroids), function(i) {
      distances <- distHaversine(
        p1 = c(county_centroids$cent_long[i], county_centroids$cent_lat[i]),
        p2 = as.matrix(targets[, c("long", "lat")])
      )
      min(distances) / 1609.344 
    })
    
    county_centroids$hours <- county_centroids$min_dist_miles / 50
    map_data_merged <- left_join(counties, county_centroids, by = c("region", "subregion"))
    
    # Plot using our pre-calculated x_proj and y_proj
    ggplot(map_data_merged, aes(x = x_proj, y = y_proj, group = group, fill = hours)) +
      geom_polygon(color = NA) +
      geom_point(
        data = targets, 
        aes(x = x_proj, y = y_proj), 
        color = "white", 
        size = 2, 
        inherit.aes = FALSE
      ) +
      coord_fixed() + # Keeps the Albers curve perfectly scaled
      scale_fill_gradientn(
        colors = c("#00FF00", "#0000FF", "#8B0000"), 
        name = paste("Hours from a", target_name)
      ) +
      theme_void() +
      theme(
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.position = "bottom",
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white", face = "bold"),
        legend.key.width = unit(3, "cm"),
        plot.title = element_text(color = "white", hjust = 0.5, size = 20, face="bold"),
        plot.margin = margin(t = 0, r = 20, b = 20, l = 20) 
      ) +
      ggtitle(paste("Sleep Deprivation Until", target_name))
  }, bg = "transparent")
  
  # Render the Click Output

  output$clickResult <- renderText({
    
    # 1. Check if a click has happened yet
    if (is.null(input$map_click)) {
      return("Click a point")
    } 
    
    targets <- target_cities()
    
    # 2. Grab the raw click (These are now in EPSG: 5070 Meters!)
    click_raw_x <- input$map_click$x
    click_raw_y <- input$map_click$y
    
    # 3. Reverse-engineer the projection to get True Lon/Lat
    true_coords <- apply_albers(click_raw_x, click_raw_y, reverse = TRUE)
    
    click_lon <- true_coords$x
    click_lat <- true_coords$y
    
    # Fail-safe just in case the click goes wildly off the grid
    if(is.na(click_lon) || is.na(click_lat) || click_lon > 180 || click_lon < -180 || click_lat > 90 || click_lat < -90) {
      return("Click closer to the map!")
    }
    
    # 4. Calculate actual distance normally using true geo-coordinates
    distances <- distHaversine(
      p1 = c(click_lon, click_lat),
      p2 = as.matrix(targets[, c("long", "lat")])
    )
    
    min_miles <- min(distances) / 1609.344
    hours <- min_miles / 50
    
    sprintf("That's %.1f hours away!", hours)
  })
}

shinyApp(ui = ui, server = server)