library(tidyverse)
library(readr)
library(lubridate)
library(gridExtra)
library(gganimate)
library(magick) # for rendering the animation from gganimate


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
crashes %>% 
  ggplot(aes(x=as.POSIXct(`CRASH TIME`)))+
  geom_histogram(color='black',fill='lightblue', bins = 60)+
  xlab("Time of Day")+
  ylab("Frequency")+
  ggtitle(label = "Frequency of Crashes by Time of Day")+
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M")
# seems like most crashes occur around the same time people are on their way
# home from work

# Visualize crashes by day
crashes$day <- day(as.Date(crashes$`CRASH DATE`, format = "%m/%d/%Y"))
day_freq <- as.data.frame(table(crashes$day))
#day_freq <- day_freq[order(day_freq, decreasing = TRUE)]
p1 <- day_freq %>% 
  ggplot(aes(x=Var1, y=Freq))+
  geom_bar(stat = 'identity')+
  xlab('Day of Month')+
  ylab('Frequency')

# Visualize crashes by month
crashes$month <- month(as.Date(crashes$`CRASH DATE`, format = "%m/%d/%Y"))
month_freq <- as.data.frame(table(crashes$month))
#month_freq <- month_freq[order(month_freq, decreasing = TRUE)]
p2 <- month_freq %>% 
  ggplot(aes(x=Var1, y=Freq))+
  geom_bar(stat = 'identity')+
  xlab('Month')+
  ylab('Frequency')

# Visualize crashes by year
crashes$year <- year(as.Date(crashes$`CRASH DATE`, format = "%m/%d/%Y"))
year_freq <- as.data.frame(table(crashes$year))
#year_freq <- year_freq[order(year_freq, decreasing = TRUE)]
p3 <- year_freq %>% 
  ggplot(aes(x=Var1, y=Freq))+
  geom_bar(stat = 'identity')+
  xlab('Year')+
  ylab('Frequency')+
  theme(axis.text.x = element_text(angle=45, hjust=1))

# Plot on frequency charts on single figure
grid.arrange(
  arrangeGrob(p2, p3, ncol = 2),
  arrangeGrob(p1, ncol = 1),
  top = "Frequency of Crashes by Day, Month, and Year"
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
  ylab('Factors')

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
  filter(Var1 != 'Unspecified') %>% 
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
  geom_bar(stat = 'identity', aes(fill = Var1))

# gganimate plot of 'death_borough' over the years
death_animated <- death_borough %>% 
  ggplot(aes(reorder(Var1, -Freq), Freq))+
  geom_bar(stat='identity', width = 0.5, aes(fill = Var1))+
  transition_time(as.numeric(Var2)) +
  ease_aes('linear')+
  labs(title = "Deaths from Car Accidents in the Five NY Boroughs (2012-2023)",
       subtitle = "Year: {round(frame_time+2011)}", caption = 'Source: https://catalog.data.gov/dataset/motor-vehicle-collisions-crashes')+
  ylab('No. of Deaths')+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=14))+
  guides(fill = guide_legend(title="Borough"))+
  theme_minimal()

animate(death_animated, width = 800, height = 400, nframes = 60, fps = 7)
anim_save("death_animated.gif", animation = last_animation(), path = getwd())

