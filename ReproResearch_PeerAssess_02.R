## PEER ASSESSMENT No. 2 - Module No. 5 -ReproResearch- (WEEK 3)

#---------------------------< Set working directory >---------------------------
setwd("D:/aToCoursera")
#------------------------------------------------------------------------------/

#---------------------------< Introduction
# Storms and other severe weather events can cause both public health and economic
# problems for communities and municipalities. Many severe events can result in
# fatalities, injuries, and property damage, and preventing such outcomes to the
# extent possible is a key concern. This project involves exploring the U.S.
# National Oceanic and Atmospheric Administration's (NOAA) storm database. This
# database tracks characteristics of major storms and weather events in the
# United States. including when and where they occur, as well as estimates of any
# fatalities, injuries, and property damage.

#---------------------------< The data
# The data for this assignment come in the form of a comma-separated-value file
# compressed via the bzip2 algorithm to reduce its size. You can download the
# file from the course web site:
# . Storm Data:
# https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2
# 
# There is also some documentation of the database available. Here you will find
# how some of the variables are constructed/defined.
# 
# . National Weather Service Storm Data Documentation:
# https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf
# . National Climatic Data Center Storm Events FAQ:
# https://d396qusza40orc cloudfront net/repdata%2Fpeer2 doc%2FNCDC%20Storm%
#         20Events-FAQ%20Page.pdf
# 
# The events in the database start in the year 1950 and end in November 2011. 
# In the earlier years of the database there are generally fewer events recorded,
# most likely due to a lack of good records. More recent years should be
# considered more complete.

#---------------------------< Read Data >---------------------------------------
data.file <- "repdata-data-StormData.csv.bz2"
if (!file.exists(data.file)) {
        download.file("https://d396qusza40orc.cloudfront.net/
                      repdata%2Fdata%2FStormData.csv.bz2", data.file)
}
data <- read.csv(data.file, header=T, stringsAsFactors=T, nrows = 5)
sapply(data, class)

# vars.target = c("BGN_DATE", "STATE", "EVTYPE", "LENGTH", "WIDTH", "F", "MAG",
#                 "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG",
#                 "CROPDMGEXP")
vars.target = c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", 
                "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")
columns.target  <- names(data) %in% vars.target
data            <- read.csv(data.file, header=T, stringsAsFactors=T, nrows = 300000)[columns.target] # nrows
data$date       <- as.Date(data$BGN_DATE, "%m/%d/%Y")
data$PROPDMGEXP <- as.character(data$PROPDMGEXP)
data$CROPDMGEXP <- as.character(data$CROPDMGEXP)
#-----
rm(columns.target, data.file, vars.target)
#------------------------------------------------------------------------------/

#---------------------------< Preparing Data >----------------------------------
# multiply        <- c(10^(0:9), 10^2, 10^2, 10^3, 10^3, 10^6, 10^6, 10^9, 10^9,
#                      1, 1, 1, 1)
# names(multiply) <- c(0:9, "h", "H", "k", "K", "m", "M", "b", "B", "-", "+",
#                      "?", NA)
# data$PROPDMGnew <- multiply[data$PROPDMGEXP]*data$PROPDMG
# data$CROPDMGnew <- multiply[data$CROPDMGEXP]*data$CROPDMG

#---------------------------< Subset for Q1: population health >----------------
data.PopulationDMG        <- subset(data, FATALITIES > 0 | INJURIES > 0, 
                                    select = c("EVTYPE", "FATALITIES", 
                                               "INJURIES"))
data.PopulationDMG$EVTYPE <- tolower(data.PopulationDMG$EVTYPE)
# (fatalities, injuries) by event type
FATALITIES.sum <- aggregate(FATALITIES ~ EVTYPE, data = data.PopulationDMG, 
                            FUN = sum)
FATALITIES.sum <- FATALITIES.sum[with(FATALITIES.sum, order(-FATALITIES)), ]

INJURIES.sum   <- aggregate(INJURIES ~ EVTYPE, data = data.PopulationDMG,
                            FUN = sum)
INJURIES.sum   <- INJURIES.sum[with(INJURIES.sum, order(-INJURIES)), ]

#---------------------------< Subset for Q2: economic consequences >------------
data.EconomicDMG          <- subset(data, PROPDMG > 0 | CROPDMG > 0, 
                                    select = c("EVTYPE", "PROPDMG", "PROPDMGEXP",
                                               "CROPDMG", "CROPDMGEXP"))
data.EconomicDMG$EVTYPE   <- tolower(data.EconomicDMG$EVTYPE)
#-----
rm(data.PopulationDMG)
#-----
multiply        <- c(10^(0:9), 10^2, 10^2, 10^3, 10^3, 10^6, 10^6, 10^9, 10^9,
                     1, 1, 1, 1)
names(multiply) <- c(0:9, "h", "H", "k", "K", "m", "M", "b", "B", "-", "+",
                     "?", NA)
data.EconomicDMG$PROPDMGnew <- multiply[data.EconomicDMG$PROPDMGEXP]*data.EconomicDMG$PROPDMG
data.EconomicDMG$CROPDMGnew <- multiply[data.EconomicDMG$CROPDMGEXP]*data.EconomicDMG$CROPDMG

# (property, crop, total) damage by event type
PROPDMG.sum <- aggregate(PROPDMGnew ~ EVTYPE, data = data.EconomicDMG, FUN = sum)
PROPDMG.sum <- PROPDMG.sum[with(PROPDMG.sum, order(-PROPDMGnew)), ]

CROPDMG.sum <- aggregate(CROPDMGnew ~ EVTYPE, data = data.EconomicDMG, FUN = sum)
CROPDMG.sum <- CROPDMG.sum[with(CROPDMG.sum, order(-CROPDMGnew)), ]
#-----
rm(data.EconomicDMG, multiply)
#-----
#rm(data)
#------------------------------------------------------------------------------/

#----- para leer linea x linea, la BD y no saturar la memoria
# install.packages('iterators')
# library(iterators)
# con <- bzfile('1988.csv.bz2', 'r')
# it <- ireadLines(con, n=1)
# nextElem(it)
# nextElem(it)
# tryCatch(expr=nextElem(it), error=function(e) return(FALSE))
#-----

# integer.columns <- sapply(data, is.integer)
# factor.columns  <- sapply(data, is.factor)
# factor.levels   <- lapply(data[, factor.columns], levels)

# data <- read.csv(unzip("StormData.csv.bz2"), stringsAsFactors=F, header=T)

#---------------------------< Assignment
# The basic goal of this assignment is to explore the NOAA Storm Database and
# answer some basic questions about severe weather events. You must use the
# database to answer the questions below and show the code for your entire
# analysis. Your analysis can consist of tables, figures, or other summaries.
# You may use any R package you want to support your analysis.

#////----------------------------- Ensayos ---------------------------------////
# length(table(data$EVTYPE))
# 
# freq.Above1.EV <- sort(table(data$EVTYPE), 
#                        decreasing=T)[sort(table(data$EVTYPE), decreasing=T)>1]
# length(freq.Above1.EV)
# 
# freq.Above2.EV <- sort(table(data$EVTYPE), 
#                        decreasing=T)[sort(table(data$EVTYPE), decreasing=T)>2]
# length(freq.Above2.EV)
# 
# freq.Above3.EV <- sort(table(data$EVTYPE), 
#                        decreasing=T)[sort(table(data$EVTYPE), decreasing=T)>3]
# length(freq.Above3.EV)
# 
# freq.Equal1.EV <- sort(table(data$EVTYPE), 
#                        decreasing=T)[sort(table(data$EVTYPE), decreasing=T)==1]
# length(freq.Equal1.EV)
#////-----------------------------------------------------------------------////

#---------------------------< QUESTION No. 1
# Across the United States, which types of events (as indicated in the EVTYPE
# variable) are most harmful with respect to population health?

library("plyr")
rpta.Q1 <- arrange(join(FATALITIES.sum, INJURIES.sum), EVTYPE)

rpta.Q1a <- rpta.Q1[with(rpta.Q1, order(-FATALITIES)), ]
row.names(rpta.Q1a) <- rpta.Q1a[, 1]
rpta.Q1a[, 1] <- NULL

rpta.Q1b <- rpta.Q1[with(rpta.Q1, order(-INJURIES)), ]
row.names(rpta.Q1b) <- rpta.Q1b[, 1]
rpta.Q1b[, 1] <- NULL

#---------------  Tabular display  ---------------
head(rpta.Q1a, 10)
head(rpta.Q1b, 10)
#---------------  Graphic display  ---------------
par(mfrow = c(2, 2))
par(mar = c(5.1, 4.1, 4.1, 2.1))
par(oma = c(0, 0, 3, 0))
#-------------------------
tmp.arrayL <- head(rpta.Q1a, 7)
tmp.arrayR <- head(rpta.Q1b, 7)
tmp.arrayL <- t(tmp.arrayL)
tmp.arrayR <- t(tmp.arrayR)

barplot(tmp.arrayL, beside = TRUE,
        col = c("lightblue", "lavender"),
        legend = rownames(tmp.arrayL))
title(main = "Decreasing Order of Fatalities", font.main = 2)
#-------------------------
barplot(tmp.arrayR, beside = TRUE,
        col = c("mistyrose", "cornsilk"),
        legend = rownames(tmp.arrayR))
title(main = "Decreasing Order of Injuries", font.main = 2)
#-------------------------
barplot(tmp.arrayL[, -1], beside = TRUE,
        col = c("lightblue", "lavender"))
title(main = "Without the firsth left", font.main = 6)
#-------------------------
barplot(tmp.arrayR[, -1], beside = TRUE,
        col = c("mistyrose", "cornsilk"))
title(main = "Without the firsth left", font.main = 6)
#-------------------------
title(main = "Impact Level in Population Health", outer = T, font.main = 2,
      sub = "sub título", xlab = "rótulos para el eje X")
#-------------------------
rm(FATALITIES.sum, INJURIES.sum, rpta.Q1, rpta.Q1a, rpta.Q1b, tmp.arrayL, tmp.arrayR)
#=============================================================

#---------------------------< QUESTION No. 2
# Across the United States, which types of events have the greatest economic
# consequences?
rpta.Q2        <- arrange(join(PROPDMG.sum, CROPDMG.sum), EVTYPE)
rpta.Q2[, 2:3] <- rpta.Q2[, 2:3] / 1e9
names(rpta.Q2) <- c("EVTYPE", "Property", "Crop")
rpta.Q2$Total  <- rpta.Q2$Property + rpta.Q2$Crop

rpta.Q2a            <- rpta.Q2[with(rpta.Q2, order(-Property)), ]
row.names(rpta.Q2a) <- rpta.Q2a[, 1]
rpta.Q2a[, 1]       <- NULL

rpta.Q2b            <- rpta.Q2[with(rpta.Q2, order(-Crop)), ]
row.names(rpta.Q2b) <- rpta.Q2b[, 1]
rpta.Q2b[, 1]       <- NULL

rpta.Q2t            <- rpta.Q2[with(rpta.Q2, order(-Total)), ]
row.names(rpta.Q2t) <- rpta.Q2t[, 1]
rpta.Q2t[, 1]       <- NULL
rpta.Q2t


#---------------  Tabular display  ---------------
head(rpta.Q2a, 10)[, 1]
head(rpta.Q2b, 10)[, 2]
head(rpta.Q2t, 10)
#---------------  Graphic display  ---------------
tmp.array <- head(rpta.Q2t, 5)
tmp.array <- t(tmp.array)
par(mfrow = c(1, 1))
barplot(tmp.array[3,], beside = TRUE, col = c("red"), ylab="in billions of Dollars")
title(main = "Decreasing Order of Ecomic Lost", font.main = 2)
#-----
# par(mfrow = c(2, 1))   # <======   NO FUNCIONÓ COMO YO QUERÍA **************
# tmp.array <- head(rpta.Q2a, 10)[, 1]
# names(tmp.array) <- row.names(head(rpta.Q2a))
# barplot(tmp.array, beside = TRUE, col = c("lightblue"))
# title(main = "Decreasing Order of Properties Damage", font.main = 2)
# #-----
# tmp.array <- head(rpta.Q2b, 10)[, 2]
# names(tmp.array) <- row.names(head(rpta.Q2b))
# barplot(tmp.array, beside = TRUE, col = c("lavender"))
# title(main = "Decreasing Order of Crop Damage", font.main = 2)

rm(CROPDMG.sum, PROPDMG.sum, rpta.Q2, rpta.Q2a, rpta.Q2b, rpta.Q2t, tmp.array)

#---------------------------< Report
# Consider writing your report as if it were to be read by a government or
# municipal manager who might be responsible for preparing for severe weather
# events and will need to prioritize resources for different types of events. 
# However, there is no need to make any specific recommendations in your report.

#---------------------------< Requirements
# For this assignment you will need some specific tools
# . RStudio: You will need RStudio to publish your completed analysis document
# to RPubs. You can also use RStudio to edit/write your analysis.
# . knitr: You will need the knitr package in order to compile your R Markdown
# document and convert it to HTML.

#---------------------------< Document Layout
# . Language: Your document should be written in English.
# . Title: Your document should have a title that briefly summarizes your data
# analysis.
# . Synopsis: Immediately after the title, there should be a synopsis which
# describes and summarizes your analysis in at most 10 complete sentences.
# . There should be a section titled Data Processing which describes (in words
# and code) how the data were loaded into R and processed for analysis.
# In particular, your analysis must start from the raw CSV file containing the
# data You cannot do any preprocessing outside the document. If preprocessing
# is time-consuming you may consider using the cache = true option for certain
# code chunks.
# . There should be a section titled Results in which your results are presented.
# . You may have other sections in your analysis, but Data Processing and
# Results are required.
# . The analysis document must have at least one figure containing a plot.
# . Your analyis must have no more than three figures Figures may have multiple
# plots in them (i.e.panel plots), but there cannot be more than three figures total.
# . You must show all your code for the work in your analysis document. 
# This may make the document a bit verbose, but that is okay. In general, you
# should ensure that echo = true for every code chunk (this is the default setting in knitr)

#---------------------------< Publishing Your Analysis
# For this assignment you will need to publish your analysis on RPubs.com  
# http://rpubs.com If you do not already have an account, then you will have to
# create a new account. After you have completed writing your analysis in RStudio,
# you can publish it to RPubs by doing the following:
# 1. In RStudio, make sure your R Markdown document ( .Rmd ) document is
# loaded in the editor
# 2. Click the Knit html button in the doc toolbar to preview your document.
# 3. In the preview window, click the publish button.
# 
# Once your document is published to RPubs, you should get a unique URL to
# that document Make a note of this URL as you will need it to submit your assignment.

#---------------------------< Submitting Your Assignment
# In order to submit this assignment, you must copy the RPubs URL for your
# completed data analysis document in to the peer assessment question.












