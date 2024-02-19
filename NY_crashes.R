#library(tidyverse) # for all that is holy
#library(viridis) # for my favorite ggplot color scheme
#library(readr) # for reading in the csv file
#library(lubridate) # for easier time and date data manipulation
#library(gridExtra) # for generating grids for multiple plots
#library(gganimate) # for creating an animated plot in ggplot
#library(magick) # for rendering the animation from gganimate
#library(sf) # for plotting geospatial data
#library(leaflet) # for importing OpenStreetMap
package_list = c("tidyverse", "viridis", "readr",
                 "lubridate", "gridExtra", "gganimate",
                 "magick", "sf", "leaflet")

for (pkg in package_list) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# Exploratory Data Analysis -----------------------------------------------


crashes <- read_csv("https://data.cityofnewyork.us/api/views/h9gi-nx95/rows.csv?accessType=DOWNLOAD")
str(crashes) # data contains 2,046,252 rows, 29 columns
glimpse(crashes)
summary(crashes)

# Visualize distribution of crashes by Borough
borough_freq <- table(crashes$BOROUGH)
borough_freq <- borough_freq[order(borough_freq, decreasing = TRUE)]
options(scipen = 999)
barplot(borough_freq, main = "Number of Crashes by Borough",
        xlab = "Borough",
        ylab = "No. of Crashes")

# Visualize crashes by time of day
p <- crashes %>% 
  ggplot(aes(x=as.POSIXct(`CRASH TIME`)))+
  geom_histogram(bins = 80, fill = "#414487FF")+
  xlab("Time of Day")+
  ylab(NULL)+
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M")+
  labs(caption = "Data from https://catalog.data.gov/dataset/motor-vehicle-collisions-crashes")
# seems like most crashes occur around the same time people are on their way
# home from work

# Visualize crashes by day
crashes$day <- day(as.Date(crashes$`CRASH DATE`, format = "%m/%d/%Y"))
day_freq <- as.data.frame(table(crashes$day))
#day_freq <- day_freq[order(day_freq, decreasing = TRUE)]
p1 <- day_freq %>% 
  ggplot(aes(x=Var1, y=Freq))+
  geom_bar(stat = 'identity', fill = "#2A788EFF")+
  xlab('Day of Month')+
  ylab(NULL)

# Visualize crashes by month
crashes$month <- month(as.Date(crashes$`CRASH DATE`, format = "%m/%d/%Y"))
month_freq <- as.data.frame(table(crashes$month))
#month_freq <- month_freq[order(month_freq, decreasing = TRUE)]
p2 <- month_freq %>% 
  ggplot(aes(x=Var1, y=Freq))+
  geom_bar(stat = 'identity', fill = "#440154FF")+
  xlab('Month')+
  ylab(NULL)

# Visualize crashes by year
crashes$year <- year(as.Date(crashes$`CRASH DATE`, format = "%m/%d/%Y"))
year_freq <- as.data.frame(table(crashes$year))
#year_freq <- year_freq[order(year_freq, decreasing = TRUE)]
p3 <- year_freq %>% 
  ggplot(aes(x=Var1, y=Freq))+
  geom_bar(stat = 'identity', fill = "#35B779FF")+
  xlab('Year')+
  ylab(NULL)+
  theme(axis.text.x = element_text(angle=45, hjust=1))

# Visualize crashes by DAY-OF-WEEK
crashes$dow <- weekdays.Date(as.Date(crashes$`CRASH DATE`))
dow_freq <- as.data.frame(table(crashes$dow))
p4 <- dow_freq %>% 
  ggplot(aes(Var1, Freq))+
  geom_bar(stat='identity', fill = '#20A486FF')+
  scale_x_discrete(limits = c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'))

# Plot on frequency charts on single figure
title1 = text_grob(
  "Frequency of Reported Crashes by Time, Day, Month, and Year in NYC (2012 - 2023)",
  size = 15, face = "bold")
grid.arrange(
  arrangeGrob(p, ncol = 1),
  arrangeGrob(p1, ncol = 1),
  arrangeGrob(p2, p3, ncol = 2),
  top = title1
)

# Determine locations of missing values
missing_vals <- as.data.frame(colSums(is.na(crashes)))
missing_vals$factor <- rownames(missing_vals)
names(missing_vals) <- c('n','variable')
rownames(missing_vals) <- NULL

missing_vals %>% 
  ggplot(aes(x = n, y = reorder(variable, n)))+
  geom_bar(stat = 'identity')+
  ggtitle('Location and Count of Missing Data')+
  xlab('Count')+
  ylab('Variable')


# Determine different types of 2-car collisions based on vehicle type
# Create a column that combines the types of vehicle in 'VEHICLE TYPE CODE 1' AND 'VEHICLE TYPE CODE 2'
crashes$two_car <- paste0(crashes$`VEHICLE TYPE CODE 1`, ' - ', crashes$`VEHICLE TYPE CODE 2`)
head(crashes$two_car)
two_car <- as.data.frame(table(crashes$two_car))
two_car %>% 
  arrange(desc(Freq))

# Determine different types of contributing factors from 'CONTRIBUTING FACTORS 1' and 'CONTRIBUTING FACTORS 2'
as.data.frame(table(crashes$`CONTRIBUTING FACTOR VEHICLE 1`)) %>% 
  filter(Var1 != 'Unspecified') %>%
  ggplot(aes(Freq, reorder(Var1, Freq)))+
  geom_bar(stat = 'identity')+
  ggtitle('Count of Contributing Crash Factors of Vehicle 1')+
  xlab('Count')+
  ylab('Factors')+
  scale_y_discrete(guide=guide_axis(n.dodge=1))

as.data.frame(table(crashes$`CONTRIBUTING FACTOR VEHICLE 2`)) %>%
  filter(Var1 != 'Unspecified') %>%
  ggplot(aes(Freq, reorder(Var1, Freq)))+
  geom_bar(stat = 'identity')+
  ggtitle('Count of Contributing Crash Factors of Vehicle 2')+
  xlab('Count')+
  ylab('Factors')

# Let's investigate the most common causes of driver/passenger deaths
# Filter crashes by deathcount >= 1
deaths <- crashes %>% 
  filter(`NUMBER OF PERSONS KILLED` >= 1)
death_factors <- as.data.frame(table(deaths$`CONTRIBUTING FACTOR VEHICLE 1`))
death_factors %>% 
  filter(Var1 != 'Unspecified' & Freq >= 50) %>% 
  ggplot(aes(Freq, reorder(Var1, Freq)))+
  geom_bar(stat = 'identity')+
  ggtitle('Most Common Causes of Crash Deaths')+
  xlab('Count')+
  ylab('Factors')

# Find out in which boroughs these deaths occur
death_borough <- as.data.frame(table(deaths$BOROUGH, deaths$year))
death_borough <- death_borough %>% 
  arrange(desc(Freq))

death_borough %>% 
  ggplot(aes(reorder(Var1, -Freq), Freq))+
  geom_bar(stat = 'identity', aes(fill = Var1))+
  guides(fill = guide_legend(title="Borough"))+
  xlab(NULL)+
  ggtitle(label = "Accident-related Deaths by Borough")+
  scale_fill_viridis(discrete = TRUE)
  

# gganimate plot of 'death_borough' over the years
death_animated <- death_borough %>% 
  ggplot(aes(reorder(Var1, -Freq), Freq))+
  geom_bar(stat='identity', width = 0.5, aes(fill = Var1))+
  transition_time(as.numeric(Var2)) +
  ease_aes('linear')+
  labs(title = "Deaths from Car Accidents in the Five NY Boroughs (2012-2023)",
       subtitle = "Year: {round(frame_time+2011)}", caption = 'Source: https://catalog.data.gov/dataset/motor-vehicle-collisions-crashes')+
  xlab(NULL)+
  ylab('No. of Deaths')+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=14))+
  guides(fill = guide_legend(title="Borough"))+
  scale_fill_viridis(discrete = TRUE)+
  theme_minimal()

animate(death_animated, width = 800, height = 400, nframes = 60, fps = 7)
#anim_save("death_animated.gif", animation = last_animation(), path = getwd())

# Visualizing geospatial data
# we use the 'sf' package to convert location data into "special feature" objects for geospatial plotting
# begin with plotting all vehicle accident-related deaths over the years
location_manhattan <- crashes %>% 
  filter(BOROUGH == "MANHATTAN" & `NUMBER OF PERSONS KILLED` >= 1 & LOCATION != "(0.0, 0.0)") # filter for only Manhattan deaths with valid location data

location_manhattan$LATITUDE <- as.numeric(gsub("\\((.*),.*\\)", "\\1", location_manhattan$LOCATION)) # the separate "LATITUDE" and "LONGITUDE" data wasn't precise enough so we use regex to extract info from the "LOCATION" column
location_manhattan$LONGITUDE <- as.numeric(gsub("\\(.*,(.*)\\)", "\\1", location_manhattan$LOCATION))

# convert dataframe to sf object
location_manhattan_sf <- st_as_sf(location_manhattan, coords = c("LONGITUDE","LATITUDE"), crs = 4326) # create new "special feature" data frame that contains only the coordinates

# import an OpenStreetMap using the 'leaflet' library
basemap <- leaflet() %>% addProviderTiles("CartoDB.Positron")
basemap %>% 
  addCircleMarkers(
    data = location_manhattan_sf,
    color = "red",
    radius = 1)
  )

# Writing a function that takes in a borough (e.g "MANHATTAN", "BROOKLYN", "STATEN ISLAND", "QUEENS", "BRONX) and generates a basemap plot
plot_deaths <- function(borough, num_killed = 1, color = "blue") {
  basemap <- leaflet() %>% addProviderTiles("CartoDB.Positron")
  
  location_borough <- crashes %>% 
    filter(BOROUGH == borough & `NUMBER OF PERSONS KILLED` >= num_killed & LOCATION != "(0.0, 0.0)")
  location_borough$LATITUDE <- as.numeric(gsub("\\((.*),.*\\)", "\\1", location_borough$LOCATION))
  location_borough$LONGITUDE <- as.numeric(gsub("\\(.*,(.*)\\)", "\\1", location_borough$LOCATION))
  location_borough_sf <- st_as_sf(location_borough, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
  basemap %>% 
    addCircleMarkers(
      data = location_borough_sf,
      color = color,
      radius = 1)
}
# Test with "QUEENS"
plot_deaths(borough ="QUEENS", num_killed = 1, color = "red")
