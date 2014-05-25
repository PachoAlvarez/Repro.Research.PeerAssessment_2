# ¡ DANGER TORNADOES !  
## The twister caused more deaths and material losses in the last 60 years than any other weather phenomenon.
**by Francisco J. Álvarez-Vargas**
========================================================

## SYNOPSIS
Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.

The U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database, tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage. The events in the database start in the year 1950 and end in November 2011. 

I analyzed this database to find events that cause more damage to the health of people and produce the greatest property damage.

## DATA PROCESSING
**----------< Packages used >----------**

```r
library(plyr)
```



**----------< Read Data >----------**

```r
setwd("D:/aToCoursera")
data.file <- "repdata-data-StormData.csv.bz2"
if (!file.exists(data.file)) {
    download.file("https://d396qusza40orc.cloudfront.net/\n                      repdata%2Fdata%2FStormData.csv.bz2", 
        data.file)
}
data <- read.csv(data.file, header = T, stringsAsFactors = T, nrows = 5)
vars.target = c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", 
    "CROPDMG", "CROPDMGEXP")
# columns.target <- names(data) %in% vars.target data <- read.csv(data.file,
# header=T, stringsAsFactors=T)[columns.target] data$date <-
# as.Date(data$BGN_DATE, '%m/%d/%Y') data$PROPDMGEXP <-
# as.character(data$PROPDMGEXP) data$CROPDMGEXP <-
# as.character(data$CROPDMGEXP) rm(columns.target, data.file, vars.target)
```


**----------< Preparing Data >----------**




## RESULTS



```r
summary(cars)
```

```
##      speed           dist    
##  Min.   : 4.0   Min.   :  2  
##  1st Qu.:12.0   1st Qu.: 26  
##  Median :15.0   Median : 36  
##  Mean   :15.4   Mean   : 43  
##  3rd Qu.:19.0   3rd Qu.: 56  
##  Max.   :25.0   Max.   :120
```


You can also embed plots, for example:


```r
plot(cars)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 


