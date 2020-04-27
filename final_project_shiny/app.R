#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidycensus)
library(janitor)
library(gt)
library(haven)
library(infer)
library(fivethirtyeight)
options(scipen = 999)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
require(maps)
require(ggmap)
options(tigris_class = "sf")
library(tigris)
options(tigris_use_cache = TRUE)
library(gganimate)
theme_set(theme_bw())
library(imputeTS)
library(shiny)

# cleaning data

bluebikes <- read_csv("raw-data/current_bluebikes_stations.csv", 
                      skip = 1, col_types = cols()) %>%
  clean_names() %>%
  rename(Long = longitude,
         Lat = latitude) %>%
  na.omit()

streetlights <- read_csv("raw-data/streetlight-locations.csv", 
                         col_types = cols()) %>%
  clean_names() %>%
  rename(Long = long,
         Lat = lat) %>%
  na.omit()

crime <- read_csv("raw-data/tmpgwmn6nt4.csv", col_types = cols()) %>%
  na.omit()

trees <- read_csv("raw-data/Trees.csv", col_types = cols()) %>%
  na.omit() %>%
  rename(Long = X,
         Lat = Y)

# counting crimes by zone

crime_count_zone <- crime %>%
  mutate(zone = case_when(Long > -71.16 & Long < -71.13 & Lat > 42.26 
                          & Lat < 42.28 ~ "z1",
                          Long > -71.13 & Long < -71.10 & Lat > 42.26 
                          & Lat < 42.28 ~ "z2",
                          Long > -71.10 & Long < -71.07 & Lat > 42.26 
                          & Lat < 42.28 ~ "z3",
                          Long > -71.07 & Long < -71.04 & Lat > 42.26 
                          & Lat < 42.28 ~ "z4",
                          Long > -71.16 & Long < -71.13 & Lat > 42.28 
                          & Lat < 42.3 ~ "z5",
                          Long > -71.13 & Long < -71.10 & Lat > 42.28 
                          & Lat < 42.3 ~ "z6",
                          Long > -71.10 & Long < -71.07 & Lat > 42.28 
                          & Lat < 42.3 ~ "z7",
                          Long > -71.07 & Long < -71.04 & Lat > 42.28 
                          & Lat < 42.3 ~ "z8",
                          Long > -71.13 & Long < -71.10 & Lat > 42.3 
                          & Lat < 42.32 ~ "z9",
                          Long > -71.10 & Long < -71.07 & Lat > 42.3 
                          & Lat < 42.32 ~ "z10",
                          Long > -71.07 & Long < -71.04 & Lat > 42.3 
                          & Lat < 42.32 ~ "z11",
                          Long > -71.13 & Long < -71.10 & Lat > 42.32
                          & Lat < 42.34 ~ "z12",
                          Long > -71.10 & Long < -71.07 & Lat > 42.32 
                          & Lat < 42.34 ~ "z13",
                          Long > -71.07 & Long < -71.04 & Lat > 42.32 
                          & Lat < 42.34 ~ "z14",
                          Long > -71.16 & Long < -71.13 & Lat > 42.34 
                          & Lat < 42.36 ~ "z15",
                          Long > -71.10 & Long < -71.07 & Lat > 42.34 
                          & Lat < 42.36 ~ "z16",
                          Long > -71.07 & Long < -71.04 & Lat > 42.34 
                          & Lat < 42.36 ~ "z17",
                          Long > -71.04 & Long < -71.01 & Lat > 42.34 
                          & Lat < 42.36 ~ "z18",
                          Long > -71.07 & Long < -71.04 & Lat > 42.36 
                          & Lat < 42.38 ~ "z19",
                          Long > -71.04 & Long < -71.01 & Lat > 42.36 
                          & Lat < 42.38 ~ "z20",
                          Long > -71.04 & Long < -71.01 & Lat > 42.38 
                          & Lat < 42.40 ~ "z21")) %>%
  filter(!is.na(zone))

# counting streetlights by zone

streetlight_count_zone <- streetlights %>%
  mutate(zone = case_when(Long > -71.16 & Long < -71.13 & Lat > 42.26 
                          & Lat < 42.28 ~ "z1",
                          Long > -71.13 & Long < -71.10 & Lat > 42.26 
                          & Lat < 42.28 ~ "z2",
                          Long > -71.10 & Long < -71.07 & Lat > 42.26 
                          & Lat < 42.28 ~ "z3",
                          Long > -71.07 & Long < -71.04 & Lat > 42.26 
                          & Lat < 42.28 ~ "z4",
                          Long > -71.16 & Long < -71.13 & Lat > 42.28 
                          & Lat < 42.3 ~ "z5",
                          Long > -71.13 & Long < -71.10 & Lat > 42.28 
                          & Lat < 42.3 ~ "z6",
                          Long > -71.10 & Long < -71.07 & Lat > 42.28 
                          & Lat < 42.3 ~ "z7",
                          Long > -71.07 & Long < -71.04 & Lat > 42.28 
                          & Lat < 42.3 ~ "z8",
                          Long > -71.13 & Long < -71.10 & Lat > 42.3 
                          & Lat < 42.32 ~ "z9",
                          Long > -71.10 & Long < -71.07 & Lat > 42.3 
                          & Lat < 42.32 ~ "z10",
                          Long > -71.07 & Long < -71.04 & Lat > 42.3 
                          & Lat < 42.32 ~ "z11",
                          Long > -71.13 & Long < -71.10 & Lat > 42.32
                          & Lat < 42.34 ~ "z12",
                          Long > -71.10 & Long < -71.07 & Lat > 42.32 
                          & Lat < 42.34 ~ "z13",
                          Long > -71.07 & Long < -71.04 & Lat > 42.32 
                          & Lat < 42.34 ~ "z14",
                          Long > -71.16 & Long < -71.13 & Lat > 42.34 
                          & Lat < 42.36 ~ "z15",
                          Long > -71.10 & Long < -71.07 & Lat > 42.34 
                          & Lat < 42.36 ~ "z16",
                          Long > -71.07 & Long < -71.04 & Lat > 42.34 
                          & Lat < 42.36 ~ "z17",
                          Long > -71.04 & Long < -71.01 & Lat > 42.34 
                          & Lat < 42.36 ~ "z18",
                          Long > -71.07 & Long < -71.04 & Lat > 42.36 
                          & Lat < 42.38 ~ "z19",
                          Long > -71.04 & Long < -71.01 & Lat > 42.36 
                          & Lat < 42.38 ~ "z20",
                          Long > -71.04 & Long < -71.01 & Lat > 42.38 
                          & Lat < 42.40 ~ "z21")) %>%
  filter(!is.na(zone))

# coutning trees by zone

tree_count_zone <- trees %>%
  mutate(zone = case_when(Long > -71.16 & Long < -71.13 & Lat > 42.26 
                          & Lat < 42.28 ~ "z1",
                          Long > -71.13 & Long < -71.10 & Lat > 42.26 
                          & Lat < 42.28 ~ "z2",
                          Long > -71.10 & Long < -71.07 & Lat > 42.26 
                          & Lat < 42.28 ~ "z3",
                          Long > -71.07 & Long < -71.04 & Lat > 42.26 
                          & Lat < 42.28 ~ "z4",
                          Long > -71.16 & Long < -71.13 & Lat > 42.28 
                          & Lat < 42.3 ~ "z5",
                          Long > -71.13 & Long < -71.10 & Lat > 42.28 
                          & Lat < 42.3 ~ "z6",
                          Long > -71.10 & Long < -71.07 & Lat > 42.28 
                          & Lat < 42.3 ~ "z7",
                          Long > -71.07 & Long < -71.04 & Lat > 42.28 
                          & Lat < 42.3 ~ "z8",
                          Long > -71.13 & Long < -71.10 & Lat > 42.3 
                          & Lat < 42.32 ~ "z9",
                          Long > -71.10 & Long < -71.07 & Lat > 42.3 
                          & Lat < 42.32 ~ "z10",
                          Long > -71.07 & Long < -71.04 & Lat > 42.3 
                          & Lat < 42.32 ~ "z11",
                          Long > -71.13 & Long < -71.10 & Lat > 42.32
                          & Lat < 42.34 ~ "z12",
                          Long > -71.10 & Long < -71.07 & Lat > 42.32 
                          & Lat < 42.34 ~ "z13",
                          Long > -71.07 & Long < -71.04 & Lat > 42.32 
                          & Lat < 42.34 ~ "z14",
                          Long > -71.16 & Long < -71.13 & Lat > 42.34 
                          & Lat < 42.36 ~ "z15",
                          Long > -71.10 & Long < -71.07 & Lat > 42.34 
                          & Lat < 42.36 ~ "z16",
                          Long > -71.07 & Long < -71.04 & Lat > 42.34 
                          & Lat < 42.36 ~ "z17",
                          Long > -71.04 & Long < -71.01 & Lat > 42.34 
                          & Lat < 42.36 ~ "z18",
                          Long > -71.07 & Long < -71.04 & Lat > 42.36 
                          & Lat < 42.38 ~ "z19",
                          Long > -71.04 & Long < -71.01 & Lat > 42.36 
                          & Lat < 42.38 ~ "z20",
                          Long > -71.04 & Long < -71.01 & Lat > 42.38 
                          & Lat < 42.40 ~ "z21")) %>%
  filter(!is.na(zone))

# counting bluebikes by zone

bluebike_count_zone <- bluebikes %>%
  mutate(zone = case_when(Long > -71.16 & Long < -71.13 & Lat > 42.26 
                          & Lat < 42.28 ~ "z1",
                          Long > -71.13 & Long < -71.10 & Lat > 42.26 
                          & Lat < 42.28 ~ "z2",
                          Long > -71.10 & Long < -71.07 & Lat > 42.26 
                          & Lat < 42.28 ~ "z3",
                          Long > -71.07 & Long < -71.04 & Lat > 42.26 
                          & Lat < 42.28 ~ "z4",
                          Long > -71.16 & Long < -71.13 & Lat > 42.28 
                          & Lat < 42.3 ~ "z5",
                          Long > -71.13 & Long < -71.10 & Lat > 42.28 
                          & Lat < 42.3 ~ "z6",
                          Long > -71.10 & Long < -71.07 & Lat > 42.28 
                          & Lat < 42.3 ~ "z7",
                          Long > -71.07 & Long < -71.04 & Lat > 42.28 
                          & Lat < 42.3 ~ "z8",
                          Long > -71.13 & Long < -71.10 & Lat > 42.3 
                          & Lat < 42.32 ~ "z9",
                          Long > -71.10 & Long < -71.07 & Lat > 42.3 
                          & Lat < 42.32 ~ "z10",
                          Long > -71.07 & Long < -71.04 & Lat > 42.3 
                          & Lat < 42.32 ~ "z11",
                          Long > -71.13 & Long < -71.10 & Lat > 42.32
                          & Lat < 42.34 ~ "z12",
                          Long > -71.10 & Long < -71.07 & Lat > 42.32 
                          & Lat < 42.34 ~ "z13",
                          Long > -71.07 & Long < -71.04 & Lat > 42.32 
                          & Lat < 42.34 ~ "z14",
                          Long > -71.16 & Long < -71.13 & Lat > 42.34 
                          & Lat < 42.36 ~ "z15",
                          Long > -71.10 & Long < -71.07 & Lat > 42.34 
                          & Lat < 42.36 ~ "z16",
                          Long > -71.07 & Long < -71.04 & Lat > 42.34 
                          & Lat < 42.36 ~ "z17",
                          Long > -71.04 & Long < -71.01 & Lat > 42.34 
                          & Lat < 42.36 ~ "z18",
                          Long > -71.07 & Long < -71.04 & Lat > 42.36 
                          & Lat < 42.38 ~ "z19",
                          Long > -71.04 & Long < -71.01 & Lat > 42.36 
                          & Lat < 42.38 ~ "z20",
                          Long > -71.04 & Long < -71.01 & Lat > 42.38 
                          & Lat < 42.40 ~ "z21")) %>%
  filter(!is.na(zone))

# joining data

crime_freq <- crime_count_zone %>%
  select(zone) %>%
  count(zone) %>%
  rename(Crime = n)

streetlight_freq <- streetlight_count_zone %>%
  select(zone) %>%
  count(zone) %>%
  rename(Streetlights = n)

crime_streetlight <- full_join(crime_freq, streetlight_freq, by = "zone") %>%
  na_replace(fill = 0)

tree_freq <- tree_count_zone %>%
  select(zone) %>%
  count(zone) %>%
  rename(Trees = n)

crime_tree <- full_join(crime_freq, tree_freq, by = "zone") %>%
  na_replace(fill = 0)

bluebike_freq <- bluebike_count_zone %>%
  select(zone) %>%
  count(zone) %>%
  rename(Bluebikes = n)

crime_bluebike <- full_join(crime_freq, bluebike_freq, by = "zone") %>%
  na_replace(fill = 0)

joined_data <- full_join(crime_streetlight, crime_tree, by = c("zone", "Crime")) %>%
  full_join(crime_bluebike, by = c("zone", "Crime"))


# code for maps

census_api_key("9f9584127dd506cdaf80bdb78927e9c01c12f2b8", install = T, overwrite = T)

suffolk <- get_acs(geography = "tract",
                   variables = c(medincome = "B19013_001"), 
                   year = 2018,
                   state = "MA",
                   county = "Suffolk County",
                   geometry = TRUE)

suffolk_map <- suffolk %>%
  ggplot() +
  geom_sf() +
  coord_sf(xlim = c(-71.168, -71.005), ylim = c(42.266, 42.415), 
           expand = FALSE) +
  scale_fill_viridis_c(direction = -1) +
  scale_color_viridis_c(direction = -1) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(5, "in"),
                         style = north_arrow_fancy_orienteering) +
  scale_x_continuous(breaks = seq(-71.16, -71.01, by = .03))


bluebike_map <- suffolk_map +
  ggtitle("Bluebike Map of Suffolk County, MA") +
  coord_sf(xlim = c(-71.168, -71.005), ylim = c(42.266, 42.415), 
           expand = FALSE) +
  geom_point(bluebikes, mapping = aes(longitude, latitude), color = "blue") +
  scale_x_continuous(breaks = seq(-71.16, -71.01, by = .03))

streetlight_map <- suffolk_map +
  geom_sf(color = "black", fill = "black") +
  geom_point(streetlights, mapping = aes(long, lat), color = "yellow", alpha = 0.01) +
  labs(title = "Streetlight Map of Suffolk County, MA",
       caption = "Source: Boston Analyze Streetlight Data",
       x = "Longitude",
       y = "Latitude")

tree_map <- suffolk_map +
  geom_point(trees, mapping = aes(X, Y), color = "dark green", alpha = 0.01) +
  labs(title = "Tree Map of Suffolk County, MA",
       caption = "Source: Boston Analyze Public Tree Data 2011",
       x = "Longitude",
       y = "Latitude")

trash_map <- suffolk_map +
  geom_point(trees, mapping = aes(X, Y), color = "purple", alpha = 0.01) +
  labs(title = "Trash Receptacle Map of Suffolk County, MA",
       caption = "Source: Department of Innovation and Technology Public Big Belly Data")

crime_map <- suffolk_map +
  geom_sf(color = "black", fill = "lightgreen") +
  labs(title = "Crime Map of Suffolk County, MA",
       caption = "Source: ANALYZE BOSTON CRIME INCIDENT REPORTS (AUGUST 2015 - TO DATE)") +
  coord_sf(xlim = c(-71.168, -71.005), ylim = c(42.266, 42.415), 
           expand = FALSE) +
  geom_point(crime, mapping = aes(Long, Lat), color = "red", alpha = 0.3)


# code for histograms

crime_count_histogram <- crime_count_zone %>%
  rename(Zone = zone) %>%
  ggplot(aes(Zone)) +
  geom_bar(stat = "count") +
  labs(title = "Crime Count per Zone in Suffolk, MA",
       subtitle = "Data From 2015 - Present",
       y = "Number of Crimes")

streetlight_count_histogram <- streetlight_count_zone %>%
  rename(Zone = zone) %>%
  ggplot(aes(Zone)) +
  geom_bar(stat = "count") +
  labs(title = "Streetlight Count per Zone in Suffolk, MA",
       subtitle = "Data from Boston Analyze, 2020",
       y = "Number of Streetlights")

tree_count_histogram <- tree_count_zone %>%
  rename(Zone = zone) %>%
  ggplot(aes(Zone)) +
  geom_bar(stat = "count") +
  labs(title = "Tree Count per Zone in Suffolk, MA",
       subtitle = "Data from Boston Analyze, 2020",
       y = "Number of Trees")

bluebike_count_histogram <- bluebike_count_zone %>%
  rename(Zone = zone) %>%
  ggplot(aes(Zone)) +
  geom_bar(stat = "count") +
  labs(title = "Bluebike Count per Zone in Suffolk, MA",
       subtitle = "Data from Boston Analyze, 2020",
       y = "Number of Bluebikes")

# relational graphs

crime_street_relation <- crime_streetlight %>%
  ggplot(., aes(n.y, n.x)) +
  geom_point(color = "gold2") +
  geom_smooth(method = "lm", se = FALSE, formula = 'y ~ x') +
  labs(title = "Relationship Between Streetlight and Crime Densities",
       x = "Streetlights",
       y = "Crime Rates")

crime_tree_relation <- crime_tree %>%
  ggplot(., aes(n.y, n.x)) +
  geom_point(color = "dark green") +
  geom_smooth(method = "lm", se = FALSE, formula = 'y ~ x') +
  labs(title = "Relationship Between Tree and Crime Densities",
       x = "Trees",
       y = "Crime Rates")

crime_bluebike_relation <- crime_bluebike %>%
  ggplot(., aes(n.y, n.x)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, formula = 'y ~ x') +
  labs(title = "Relationship Between Bluebike and Crime Densities",
       x = "Bluebikes",
       y = "Crime Rates")


# Define UI for application that draws a histogram
ui <- fluidPage(
   
  navbarPage(
    "Analysis of Public Infrastructure and Amenities on Crime Rates in Boston",
    tabPanel(
      title = "Introduction",
      h4("By Hannah Phan"),
      h3("Why Crime Levels in Boston?"),
      p(
        "COMPLETE: Generational changes, gentrification, urban renewel"
      )
    ),
    
    tabPanel(
      title = "Public Infrastructure and Amenity Relations",

      sidebarPanel(
        p("Select a Public Amenity"),
        selectInput(
          inputId = "stat",
          label = "Amenity",
          choices = c("Streetlights", "Trees", "Bluebikes")
        ),
      br(),
      ),
      mainPanel(plotOutput("relationPlot"))
  ),
  
  tabPanel(
    title = "Multiple Regression on Public Amenities vs. Crime",
    
    mainPanel(tableOutput("gttable"))
  )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$relationPlot <- renderPlot({
     joined_data %>%
       ggplot(., aes_string(input$stat, "Crime")) +
       geom_point(color = "red") +
       geom_smooth(method = "lm", se = FALSE, formula = 'y ~ x') +
       labs(title = "Relationship Between Selected Amenity and Crime",
            x = input$stat,
            y = "Crime Rates")
   })
   
   output$gttable <- renderTable({
     regression_table <- joined_data %>%
       lm(Crime ~ Streetlights * Bluebikes * Trees, data = .) %>%
       tidy(conf.int = TRUE, conf.level = 0.90) %>%
       mutate_if(is.numeric, round, digits = 2) %>%
       clean_names() %>% 
       select(-std_error, -statistic, -p_value) %>% 
       rename("5th Percentile" = conf_low,
              "95th Percentile" = conf_high,
              "Coefficient" = estimate,
              "Term" = term) %>% 
       gt()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

