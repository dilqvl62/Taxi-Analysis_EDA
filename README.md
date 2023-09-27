# Taxi-Analysis
The purpose of this project is to perform the Exploratory Data Analysis on the NYC yellow cap trip record in order to predict the duration of each trip. 

## Data
I get the data from [this webesite](https://www.kaggle.com/competitions/nyc-taxi-trip-duration/data) 

## Quastions 
*The ofllowing are the steps i took to explore and understand the data 
  * Plot a map of NYC along with number of pickup coordinates to get a get a general overview of the locations and distances
  * Taking a look at the distributions of passenger_count and vendor_id by creating a multi plot 
  * Check how many number of passenger s travel in a taxi
  * Check weather different number of passengers are correlated with the duration of the trip
  * Check the relationship between store_and_forward vs trip_duration
  * Build a new features from the existing one [date, month, wday, hour] derived from the pickup_datetime. From the coordinated of the pickup and dropoff points, 
    calculate the distace between the two pointsand compare it to trip_duration using the function distCosine of the geosphere
  * Compute the average apparent velocity of the taxis, the average duration per day and hour, the average speed of these time bins
  * Create a heatmap of speed over the week for hours
  * Create a new feature based on busy hours
  * Explore the trips to airport, long day trips, and zero distance trips
** For this project, I used the R language and these are the tools i imported:
     ![Screen Shot 2023-09-27 at 1 38 48 PM](https://github.com/dilqvl62/Taxi-Analysis_EDA/assets/107519883/8efc502f-fd72-434e-9e14-d86bc17302e7)

I created a function to vusualize and compare multiple plots side by side 

![Screen Shot 2023-09-27 at 1 40 30 PM](https://github.com/dilqvl62/Taxi-Analysis_EDA/assets/107519883/4b705e02-3153-4b3d-bffc-920ac435635c)

* See the [visualization created for this project](figures)
