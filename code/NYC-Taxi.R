library('ggplot2') # visualization
library('scales') #visualization
library('grid')# visualization
library('RColorBrewer')# visualization
library('corrplot') # visualization
library('alluvial')# visualization
library('dplyr')# data manipulation
library('readr')#input/ output

getwd()
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











