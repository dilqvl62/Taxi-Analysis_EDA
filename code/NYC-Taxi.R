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
library('leaflet') #map 
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
      matchid <- as.data.frame(which(layout == i, arr.ind =TTURE))
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
qplot(trip_duration, data=s1, bins = 30)

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
multiplot(p1,p2, layout=layout)
p1 <-1; p2<-1




