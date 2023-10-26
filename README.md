# Taxi-Analysis

The purpose of this project is to perform the Exploratory Data Analysis on the NYC yellow cap trip record in order to predict the duration of each trip. 

## Data
I get the data from [this webesite](https://www.kaggle.com/competitions/nyc-taxi-trip-duration/data) 

## Objectives
**For this project, I used the R language and these are the tools (libraries) that I used**:

![Screen Shot 2023-09-27 at 1 38 48 PM](https://github.com/dilqvl62/Taxi-Analysis_EDA/assets/107519883/8efc502f-fd72-434e-9e14-d86bc17302e7)

*The following are the steps I took to explore and understand the data* 

I created a function to vusualize and compare multiple plots side by side

![Screen Shot 2023-09-27 at 1 40 30 PM](https://github.com/dilqvl62/Taxi-Analysis_EDA/assets/107519883/4b705e02-3153-4b3d-bffc-920ac435635c)
  
1 * [Plot a map of NYC along with number of pickup coordinates to get a get a general overview of the locations and distances using leaflet](https://github.com/dilqvl62/Taxi-Analysis_EDA/blob/main/figures/pickups_map.png)

2 * Plotting a histogram for the pickup datetime and dropoff datetime from Janrury to July respectively

**Note** Drop of the number of trip counts in the month of febrauray maybe due to the weather condition
![Screen Shot 2023-10-26 at 3 43 05 PM](https://github.com/dilqvl62/Taxi-Analysis_EDA/assets/107519883/07842c5b-1032-46eb-b1eb-f0dce54903e0)
![Screen Shot 2023-10-26 at 3 48 57 PM](https://github.com/dilqvl62/Taxi-Analysis_EDA/assets/107519883/d1a526e2-c7be-4754-8077-6d1b0ae96d3f)

* Creating a mutiplot with different components to investigate the variation and the distribution of passenger count and vendor_id and other created variables

![Screen Shot 2023-10-26 at 4 21 13 PM](https://github.com/dilqvl62/Taxi-Analysis_EDA/assets/107519883/9dbedc2c-2930-486e-942b-f9f5dca426e4)

* Build a new features from the existing one [date, month, wday, hour] derived from the pickup_datetime. From the coordinated of the pickup and dropoff points, 
calculate the distace between the two pointsand compare it to trip_duration using the function distCosine of the geosphere
* Create a heatmap of speed over the week for hours

![Screen Shot 2023-10-26 at 4 40 43 PM](https://github.com/dilqvl62/Taxi-Analysis_EDA/assets/107519883/cf972efd-c18d-4457-84a0-060d7dc8cd68)

* Bearing direction
  * If the direct **distance** is the magnitude of the trip vector then the **bearing** is its initial direction
  * Use the **geosphere** package that tells me whether the trips started out for instance in the direction of North-West or South-East
  * I visualize the **bearing** distribution and its relation to **trip_duration**, direct **distance** and **speed**

 ![Screen Shot 2023-10-26 at 4 51 32 PM](https://github.com/dilqvl62/Taxi-Analysis_EDA/assets/107519883/b99a11af-0319-420f-a328-992ac5d53014)





**Note** See the [visualization created for this project](figures)
