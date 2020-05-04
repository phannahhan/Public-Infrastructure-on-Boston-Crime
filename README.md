# The Effect of Public Infrastructure and Amenities on Boston Crime Levels

## Project Summary

##### My data came from a variety of publicly viewable datasets created by Analyze Boston and from the City of Boston’s website. For the geospatial data, I needed to standardize the names of the columns for Longitude and Latitude to combine my datasets as well as remove N/A values and header rows that were not actually data points. For my statistical analysis, I calculated the crime densities of Suffolk County by dividing the region into 21 zones. I compared this to the absolute number of streetlights, trees, and Bluebikes in each zone and applied a linear regression model between the crime count and each of the public amenities. At first, I wanted to divide the map into a square grid and calculate the densities of each square, but then I realized that squares that included land outside of Suffolk County would inaccurately have a lower density of each amenity in addition to crime levels, so I made the decision to pick square zones of equal area that only included Suffolk County. When creating my regression table, I chose to take the logs of each value because there were significantly fewer bikes to crimes as there were trees or streetlights to crimes. Taking the log of each value allowed me to see the percent change in crime density as the explanatory variable increased by one percentage point. To create my maps, I used the United States Census data to get a map of Suffolk County, and then I set a specific range for the Latitude and Longitude by looking at the highest and lowest Lat and Long values of the datasets to zoom into the area of interest. I overlayed a ggplot of crime, tree, streetlight, and Bluebike data over the map of Suffolk County and adjusted the transparency and colors of the points and land accordingly.

## Repo Guide

##### about.Rmd : initial draft of the about page
#### public-infra-crime.Rmd : An R Markdown version of my project, with detailed descriptions of how I chose to create my plots and maps.
#### README.md
