library('ggplot2') # visualization
library('scales') #visualization
library('grid')# visualization
library('gridExtra')
library('RColorBrewer')# visualization
library('corrplot') # visualization
library('alluvial')# visualization
library('dplyr')# data manipulation
library('readr')#input/ output
library('data.table')# data manipulation
library('tibble') #data wrangling
library('tidyr')# data wrangling
library('stringr') #string manipulation
library('forcats') #factor manipulation
library('lubridate') #data and time
library('leaflet') #maps
library('geosphere')# spatial locations
library('leaflet.extras')#maps
library('maps') #maps

#function to plot multiple plots and arranging them in two columns

multiplot_function <- function(..., Col) {
  # Arrange the plots in a grid
  library(gridExtra)
  grid.arrange(..., ncol = Col)  # Arrange plots in columns
}

Ny_taxi<- as_tibble(read.csv('data/Taxi.csv'))

# Understading the variables 
summary(Ny_taxi)
glimpse(Ny_taxi)
sum(is.na(Ny_taxi))
table(Ny_taxi$vendor_id)
table(Ny_taxi$store_and_fwd_flag)
table(Ny_taxi$passenger_count)


#taking a look at the trip duration variable 
qplot(trip_duration, data=Ny_taxi, bins = 30)

#minimizing the trip duration to have a better look of the graph 
reduced_duration = Ny_taxi %>%
              filter(trip_duration < 10000)
            qplot(trip_duration, data=reduced_duration, bins =30)

#again making the graph look more informative by using the log for x axis and sqrt for the y axsis
Ny_taxi%>%
              ggplot(aes(trip_duration)) + 
              geom_histogram(fill ="red", bins= 150) +
              scale_x_log10()+
              scale_y_sqrt()
# Getting the locations of pickups coordinates by plotting the map of NYC using [leaflet] package 
 
#converting some variables to factor and some to date time using the function mutate()
Ny_taxi <- Ny_taxi %>%
              mutate(pickup_datetime= ymd_hms(pickup_datetime),
                     dropoff_datetime= ymd_hms(dropoff_datetime),
                     vendor_id= factor(vendor_id),
                     passenger_count= factor(passenger_count))

#creating new variable check and outputing the count of that variable
Ny_taxi %>%
              mutate(check = abs(int_length(interval(dropoff_datetime,pickup_datetime)) + trip_duration) > 0) %>%
              select(check,pickup_datetime, dropoff_datetime, trip_duration) %>%
              group_by(check) %>%
              count()
#setting the seed for a reproducible code 
set.seed(1234)

#creatting a sample dataset from the original one 
Ny_taxi_sample <- sample_n(Ny_taxi , 8e3)

#plotting the pickups using leaflets 
leaflet(data = Ny_taxi_sample) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
              addCircleMarkers(~ pickup_longitude, ~pickup_latitude, radius = 1,
                               color = "red", fillOpacity = 0.3)

#taking a look at the data by arranging the trip duration in a descending order 
Ny_taxi %>%
              arrange(desc(trip_duration)) %>%
              select(trip_duration, pickup_datetime,dropoff_datetime,everything()) %>%
              head(10)
# checking the unique values of the year for pickups and dropoffs 
unique(year(Ny_taxi$pickup_datetime))
unique(year(Ny_taxi$dropoff_datetime))
# Plotting a histogram for the pickup_datetime
pickup_dt <- Ny_taxi %>%
              ggplot(aes(pickup_datetime)) + 
              geom_histogram(fill = "red", bins =120) + 
              labs(x= "pickup dates")

# Plotting a histogram for the dropoff_datetime
dropoff_dt <- Ny_taxi %>%
              ggplot(aes(dropoff_datetime)) + 
              geom_histogram(fill = "blue", bins = 120) + 
              labs(x= "dropoff dates")
#plotting histogram only for filtered dates to get a better insight 
Ny_taxi %>%
              filter((pickup_datetime > ymd("2016-01-20")) & (pickup_datetime < ymd("2016-02-10"))) %>%
              ggplot(aes(pickup_datetime)) + 
              geom_histogram(fill ="red", bins =120)
#taking a look at the variation with the distributions of *passenger_count
#and *vendor_id by creating different plots with different components:

passengerCount<- Ny_taxi %>% 
              group_by(passenger_count) %>%
              count() %>%
              ggplot(aes(passenger_count, n, fill = passenger_count)) +
              geom_col() + 
              scale_y_sqrt()+
              theme(legend.position = "none")


vendorID<- Ny_taxi%>%
              ggplot(aes(vendor_id, fill= vendor_id)) +
              geom_bar()+
              theme(legend.position = "none")


str_fwd_flag <- Ny_taxi %>%
              ggplot(aes(store_and_fwd_flag)) +
              geom_bar()+
              theme(legend.position = "none")+
              scale_y_log10()

day_of_week<- Ny_taxi %>%
              mutate(wday = wday(pickup_datetime, label =TRUE,week_start = 1)) %>%
              group_by(wday,vendor_id) %>%
              count()%>%
              ggplot(aes(wday,n, colour = vendor_id)) +
              geom_point(size =4) +
              labs(x = "Day of the week", y = "Total number of the pickups") + 
              theme(legend.position = "none")

hours_pickup <- Ny_taxi %>%
              mutate(hpick = hour(pickup_datetime)) %>%
              group_by(hpick, vendor_id) %>%
              count() %>%
              ggplot(aes(hpick, n, color = vendor_id)) + 
              geom_point(size = 4) + 
              labs(x ="hour of the day", y = "Total number of pickups") +
              theme(legend.position = "none")

multiplot_function(passengerCount, vendorID, str_fwd_flag,day_of_week,hours_pickup,Col=2)

#the count of passengers that are using taxi to travel
Ny_taxi%>%
              group_by(passenger_count)%>%
              count()


Ny_taxi%>%
            group_by(store_and_fwd_flag)%>%
            count() 
# #plotting the hours of the day by using month as a color
Month_hOfDay <-Ny_taxi%>%
            mutate(hpick =hour(pickup_datetime),
                   Month = factor(month(pickup_datetime, label = TRUE))) %>%
            group_by(hpick, Month)%>%
            count()%>%
            ggplot(aes(hpick, n, color= Month)) +
            geom_line(size= 1.5) +
            labs(x = "Hour of the day", y = "count") 
#plotting the hours of the day by using week as a color
Week_hOfDay <-Ny_taxi %>%
            mutate(hpick= hour(pickup_datetime),
                   wday = factor(wday(pickup_datetime, label = TRUE, week_start = 1))) %>%
            group_by(hpick, wday) %>%
            count() %>%
            ggplot(aes(hpick, n, color=wday)) + 
            geom_line(size= 1.5) + 
            labs(x = "Hour of the day", y = "count")


#plotting the two plots in one plot using the multiplot function

multiplot_function(Month_hOfDay, Week_hOfDay, Col = 1)

#plotting based on latitude and longitude
pLongitude <- Ny_taxi %>%
            filter(pickup_longitude > -74.05 & pickup_longitude < -73.7) %>%
            ggplot(aes(pickup_longitude)) +
            geom_histogram(fill = "red", bins = 40)
dLongitude <- Ny_taxi %>%
            filter(dropoff_longitude > -74.05 & dropoff_longitude < -73.7) %>%
            ggplot(aes(dropoff_longitude)) +
            geom_histogram(fill = "blue", bins = 40)
pLatitude <- Ny_taxi %>%
            filter(pickup_latitude > 40.6 & pickup_latitude < 40.9) %>%
            ggplot(aes(pickup_latitude)) +
            geom_histogram(fill = "red", bins = 40)
dLatitude<- Ny_taxi %>%
            filter(dropoff_latitude > 40.6 & dropoff_latitude < 40.9) %>%
            ggplot(aes(dropoff_latitude)) +
            geom_histogram(fill = "blue", bins = 40)
#putting all the 4 plots into one 
multiplot_function(pLongitude,dLongitude,pLatitude,dLatitude,Col =2)

#pickup_latitude vs pickup_longitude
Ny_taxi %>%
            arrange(pickup_latitude) %>%
            select(pickup_latitude, pickup_longitude) %>%
            head(5)
Ny_taxi %>%
            arrange(desc(pickup_latitude)) %>%
            select(pickup_latitude, pickup_longitude) %>%
            head(5)
#plotting the week days vs median trip duration 
WeedD_median_TD<- Ny_taxi %>%
            mutate(wday = wday(pickup_datetime, label = TRUE, week_start = 1)) %>%
            group_by(wday, vendor_id) %>%
            summarise(median_duration = median(trip_duration)/60) %>%
            ggplot(aes(wday, median_duration, color = vendor_id)) +
            geom_point(size = 4) +
            labs(x = "Day of the week", y = "Median trip duration [min]")
#plotting 
HourPu_median_TD<- Ny_taxi %>%
            mutate(hpick = hour(pickup_datetime)) %>%
            group_by(hpick, vendor_id) %>%
            summarise(median_duration = median(trip_duration)/60) %>%
            ggplot(aes(hpick, median_duration, color = vendor_id)) +
            geom_smooth(method = "loess", span = 1/2) +
            geom_point(size = 4) +
            labs(x = "Hour of the day", y = "Median trip duration [min]") +
            theme(legend.position = "none")
#putting the two plots into one using the multiplot function

multiplot_function(WeedD_median_TD, HourPu_median_TD, Col = 1)

#checking the correlation between different vendors and the duration of the trip  
Ny_taxi %>%
            ggplot(aes(passenger_count, trip_duration, color = passenger_count)) +
            geom_boxplot() +
            scale_y_log10() +
            theme(legend.position = "none") +
            facet_wrap(~ vendor_id) +
            labs(y = "Trip duration [s]", x = "Number of passengers")

Ny_taxi %>%
            ggplot(aes(trip_duration, fill = vendor_id)) +
            geom_density(position = "stack") +
            scale_x_log10()

Ny_taxi %>%
            group_by(vendor_id) %>%
            summarise(mean_duration = mean(trip_duration),
                      median_duration = median(trip_duration))
#store and Forward vs *trip duration

Ny_taxi %>%
            group_by(vendor_id, store_and_fwd_flag) %>%
            count()
# plotting the number of passengers or only vendor one 
tripD_vendor1<- Ny_taxi %>%
  
            filter(vendor_id == 1) %>%
            ggplot(aes(passenger_count, trip_duration, color = passenger_count)) +
            geom_boxplot() +
            scale_y_log10() +
            facet_wrap(~ store_and_fwd_flag) +
            theme(legend.position = "none") +
            labs(y = "Trip duration [s]", x = "Number of passengers") +
            ggtitle("Store_and_fwd_flag impact")

#'build new features from the existing ones - (date, month, wday, hour) 
#'derived from the *pickup/_datetime*. 
#'#' From the coordinates of the pickup and dropoff points calculate 
#'the direct *distance* between the two points, and 
#'compare it to our *trip/_durations*, using the *distCosine* function of the [geosphere].


#creatting the cordinates for jfk and laguardia aeroport
jfk_coord <- tibble(lon = -73.778889, lat = 40.639722)
laguardia_coord <- tibble(lon = -73.872611, lat = 40.77725)

#pickup coordinates
pick_coord <- Ny_taxi %>%
          select(pickup_longitude, pickup_latitude)
# dropoff coordinates
drop_coord <- Ny_taxi %>%
          select(dropoff_longitude, dropoff_latitude)
#creating new variables 
Ny_taxi$dist <- distCosine(pick_coord, drop_coord)
Ny_taxi$bearing = bearing(pick_coord, drop_coord)
Ny_taxi$jfk_dist_pick <- distCosine(pick_coord, jfk_coord)
Ny_taxi$jfk_dist_drop <- distCosine(drop_coord, jfk_coord)
Ny_taxi$lg_dist_pick <- distCosine(pick_coord, laguardia_coord)
Ny_taxi$lg_dist_drop <- distCosine(drop_coord, laguardia_coord)
#creating new data set using the new variables that was created 
Ny_taxi <- Ny_taxi %>%
           mutate(speed = dist/trip_duration*3.6,
           date = date(pickup_datetime),
           month = month(pickup_datetime, label = TRUE),
           wday = wday(pickup_datetime, label = TRUE, week_start = 1),
           hour = hour(pickup_datetime),
           work = (hour %in% seq(8,18)) & (wday %in% c("Mon","Tues","Wed","Thurs","Fri")),
           jfk_trip = (jfk_dist_pick < 2e3) | (jfk_dist_drop < 2e3),
           lg_trip = (lg_dist_pick < 2e3) | (lg_dist_drop < 2e3),
           blizzard = !( (date < ymd("2016-01-22") | (date > ymd("2016-01-29"))) )
  )
print(colnames(Ny_taxi))

#'compute the average apparent velocity of the taxis, the average duration 
#'per day and hour, the average speed for these time bins


set.seed(4321)
Ny_taxi %>%
          sample_n(5e4) %>%
          ggplot(aes(dist, trip_duration)) +
          geom_point() +
          scale_x_log10() +
          scale_y_log10() +
          labs(x = "Direct distance [m]", y = "Trip duration [s]")

Ny_taxi %>%
          filter(trip_duration < 3600 & trip_duration > 120) %>%
          filter(dist > 100 & dist < 100e3) %>%
          ggplot(aes(dist, trip_duration)) +
          geom_bin2d(bins = c(500,500)) +
          scale_x_log10() +
          scale_y_log10() +
          labs(x = "Direct distance [m]", y = "Trip duration [s]")

Ny_taxi %>%
          filter(speed > 2 & speed < 1e2) %>%
          ggplot(aes(speed)) +
          geom_histogram(fill = "red", bins = 50) +
          labs(x = "Average speed [km/h] (direct distance)")


Wday_median_speed <- Ny_taxi %>%
          group_by(wday, vendor_id) %>%
          summarise(median_speed = median(speed)) %>%
          ggplot(aes(wday, median_speed, color = vendor_id)) +
          geom_point(size = 4) +
          labs(x = "Day of the week", y = "Median speed [km/h]")

Hour_d_median_speed <- Ny_taxi %>%
          group_by(hour, vendor_id) %>%
          summarise(median_speed = median(speed)) %>%
          ggplot(aes(hour, median_speed, color = vendor_id)) +
          geom_smooth(method = "loess", span = 1/2) +
          geom_point(size = 4) +
          labs(x = "Hour of the day", y = "Median speed [km/h]") +
          theme(legend.position = "none")

#'Create heatmap of speed over the week for hours.

 Hour_Week_medianSpeed<- Ny_taxi %>%
          group_by(wday, hour) %>%
          summarise(median_speed = median(speed)) %>%
          ggplot(aes(hour, wday, fill = median_speed)) +
          geom_tile() +
          labs(x = "Hour of the day", y = "Day of the week") +
          scale_fill_distiller(palette = "Spectral")

multiplot_function(Wday_median_speed,Hour_d_median_speed,Hour_Week_medianSpeed,Col = 2)


