library('ggplot2') # visualization
library('scales') #visualization
library('grid')# visualization
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

muplot <- function(... ,plotlist=NULL, file, cols=1, layout=NULL){
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                        ncol = cols, nrow = ceilling(numPlots/cols))
  }
  if(numPlots ==1) {
    print(plots[[1]])
  }
  if (numPlots==1) {
    print(plots[[1]])
  }else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      matchid <- as.data.frame(which(layout == i, arr.ind =TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchid$row,
                                      layout.pos.col = matchid$col))
    }
  }
}
taxi<- as_tibble(read.csv('data/Taxi.csv'))
summary(taxi)
glimpse(taxi)
sum(is.na(taxi))

table(taxi$vendor_id)
table(taxi$store_and_fwd_flag)
table(taxi$passenger_count)
qplot(trip_duration, data=taxi, bins = 30)

s1 = taxi %>%
  filter(trip_duration < 10000)
qplot(trip_duration, data=s1, bins =30)

taxi%>%
  ggplot(aes(trip_duration)) + 
  geom_histogram(fill ="red", bins= 150) +
  scale_x_log10()+
  scale_y_sqrt()

taxi <- taxi %>%
  mutate(pickup_datetime= ymd_hms(pickup_datetime),
         dropoff_datetime= ymd_hms(dropoff_datetime),
         vendor_id= factor(vendor_id),
         passenger_count= factor(passenger_count))

taxi %>%
  mutate(check = abs(int_length(interval(dropoff_datetime,pickup_datetime)) + trip_duration) > 0) %>%
  select(check,pickup_datetime, dropoff_datetime, trip_duration) %>%
  group_by(check) %>%
  count()

set.seed(1234)
foo <- sample_n(taxi , 8e3)
leaflet(data = foo) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircleMarkers(~ pickup_longitude, ~pickup_latitude, radius = 1,
                   color = "red", fillOpacity = 0.3)


taxi %>% 
  ggplot(aes(trip_duration)) +
  geom_histogram(fill= "red", bins = 150) + 
  scale_x_log10() + 
  scale_y_sqrt()


taxi %>%
  arrange(desc(trip_duration)) %>%
  select(trip_duration, pickup_datetime,dropoff_datetime,everything()) %>%
  head(10)

p1 <- taxi %>%
  ggplot(aes(pickup_datetime)) + 
  geom_histogram(fill = "red", bins =120) + 
  labs(x= "pickup dates")

p2 <- taxi %>%
  ggplot(aes(dropoff_datetime)) + 
  geom_histogram(fill = "blue", bins = 120) + 
  labs(x= "dropoff dates")
layout <- matrix(c(1,2),2,1,byrow = FALSE)
muplot(p1,p2, layout=layout)
p1 <-1; p2<-1

taxi %>%
  filter(pickup_datetime > ymd("2016-01-20") & pickup_datetime < ymd("2016-02-10")) %>%
  ggplot(aes(pickup_datetime)) + 
  geom_histogram(fill ="red", bins =120)

p1<- taxi %>% 
  group_by(passenger_count) %>%
  count() %>%
  ggplot(aes(passenger_count, n, fill = passenger_count)) +
  geom_col() + 
  scale_y_sqrt()+
  theme(legend.position = "none")

p2<- taxi%>%
  ggplot(aes(vendor_id, fill= vendor_id)) +
  geom_bar()+
  theme(legend.position = "none")

p3<-taxi %>%
  ggplot(aes(store_and_fwd_flag)) +
  geom_bar()+
  theme(legend.position = "none")+
  scale_y_log10()

p4<- taxi %>%
  mutate(wday = wday(pickup_datetime, label =TRUE,week_start = 1)) %>%
  group_by(wday,vendor_id) %>%
  count()%>%
  ggplot(aes(wday,n, colour = vendor_id)) +
  geom_point(size =4) +
  labs(x = "Day of the week", y = "Total number of the pickups") + 
  theme(legend.position = "none")

p5<- taxi %>%
  mutate(hpick = hour(pickup_datetime)) %>%
  group_by(hpick, vendor_id) %>%
  count() %>%
  ggplot(aes(hpick, n, color = vendor_id)) + 
  geom_point(size = 4) + 
  labs(x ="hour of the day", y = "Total number of pickups") +
  theme(legend.position = "none")

layout <- matrix(c(1,2,3,4,5,5),3,2,byrow = TRUE)
muplot(p1, p2, p3, p4, p5, layout=layout)
p1 <- 1; p2 <- 1; p3 <- 1; p4 <- 1; p5 <- 1


taxi%>%
  group_by(store_and_fwd_flag)%>%
  count()

p1<-taxi%>%
  mutate(hpick =hour(pickup_datetime),
         Month = factor(month(pickup_datetime, label = TRUE))) %>%
  group_by(hpick, Month)%>%
  count()%>%
  ggplot(aes(hpick, n, color= Month)) +
  geom_line(size= 1.5) +
  labs(x = "Hour of the day", y = "count") 

p2<-taxi %>%
  mutate(hpick= hour(pickup_datetime),
         wday = factor(wday(pickup_datetime, label = TRUE, week_start = 1))) %>%
  group_by(hpick, wday) %>%
  count() %>%
  ggplot(aes(hpick, n, color=wday)) + 
  geom_line(size= 1.5) + 
  labs(x = "Hour of the day", y = "count")
layout <- matrix(c(1,2), 2,1, byrow= FALSE )
muplot(p1, p2, layout = layout)
p1<- 1; p2<- 1

p1 <- taxi %>%
  filter(pickup_longitude > -74.05 & pickup_longitude < -73.7) %>%
  ggplot(aes(pickup_longitude)) +
  geom_histogram(fill = "red", bins = 40)
p2 <- taxi %>%
  filter(dropoff_longitude > -74.05 & dropoff_longitude < -73.7) %>%
  ggplot(aes(dropoff_longitude)) +
  geom_histogram(fill = "blue", bins = 40)
p3 <- taxi %>%
  filter(pickup_latitude > 40.6 & pickup_latitude < 40.9) %>%
  ggplot(aes(pickup_latitude)) +
  geom_histogram(fill = "red", bins = 40)
p4 <- taxi %>%
  filter(dropoff_latitude > 40.6 & dropoff_latitude < 40.9) %>%
  ggplot(aes(dropoff_latitude)) +
  geom_histogram(fill = "blue", bins = 40)
layout <- matrix(c(1,2,3,4),2,2,byrow=FALSE)
muplot(p1, p2, p3, p4, layout=layout)
p1 <- 1; p2 <- 1; p3 <- 1; p4 <- 1

taxi %>%
  arrange(pickup_latitude) %>%
  select(pickup_latitude, pickup_longitude) %>%
  head(5)
taxi %>%
  arrange(desc(pickup_latitude)) %>%
  select(pickup_latitude, pickup_longitude) %>%
  head(5)
p1 <- taxi %>%
  mutate(wday = wday(pickup_datetime, label = TRUE, week_start = 1)) %>%
  group_by(wday, vendor_id) %>%
  summarise(median_duration = median(trip_duration)/60) %>%
  ggplot(aes(wday, median_duration, color = vendor_id)) +
  geom_point(size = 4) +
  labs(x = "Day of the week", y = "Median trip duration [min]")
p2 <- taxi %>%
  mutate(hpick = hour(pickup_datetime)) %>%
  group_by(hpick, vendor_id) %>%
  summarise(median_duration = median(trip_duration)/60) %>%
  ggplot(aes(hpick, median_duration, color = vendor_id)) +
  geom_smooth(method = "loess", span = 1/2) +
  geom_point(size = 4) +
  labs(x = "Hour of the day", y = "Median trip duration [min]") +
  theme(legend.position = "none")
layout <- matrix(c(1,2),2,1,byrow=FALSE)
muplot(p1, p2, layout=layout)
p1 <- 1; p2 <- 1



train %>%
  ggplot(aes(passenger_count, trip_duration, color = passenger_count)) +
  geom_boxplot() +
  scale_y_log10() +
  theme(legend.position = "none") +
  facet_wrap(~ vendor_id) +
  labs(y = "Trip duration [s]", x = "Number of passengers")

train %>%
  ggplot(aes(trip_duration, fill = vendor_id)) +
  geom_density(position = "stack") +
  scale_x_log10()

train %>%
  group_by(vendor_id) %>%
  summarise(mean_duration = mean(trip_duration),
            median_duration = median(trip_duration))

