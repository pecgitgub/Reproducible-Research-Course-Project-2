---
title: "PA2_Template"
author: "Satya"
date: "June 9, 2016"
output: html_document
---


#Reproducible Research Course Project 2

Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.

This project involves exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage.

##Data

The data for this assignment come in the form of a comma-separated-value file compressed via the bzip2 algorithm to reduce its size. You can download the file from the course web site:

Storm Data [47Mb]
There is also some documentation of the database available. Here you will find how some of the variables are constructed/defined.

National Weather Service Storm Data Documentation
National Climatic Data Center Storm Events FAQ
The events in the database start in the year 1950 and end in November 2011. In the earlier years of the database there are generally fewer events recorded, most likely due to a lack of good records. More recent years should be considered more complete.



```r
setwd("c://datascience/RRp")
getwd()
```

```
## [1] "c:/datascience/RRp"
```

Downloading Storm data file, cleaning data and selecting required columns


```r
library(dplyr)
 
Stormdat <- read.csv('Stormdata.csv.bz2', stringsAsFactors = F)
 

Stormdat2 <- Stormdat
names(Stormdat2) <- tolower(names(Stormdat2))

Stormdat3 <- select(tbl_df(Stormdat2), evtype, fatalities:cropdmgexp)
sum(is.na(Stormdat3))
```

```
## [1] 0
```

```r
names(Stormdat3)
```

```
## [1] "evtype"     "fatalities" "injuries"   "propdmg"    "propdmgexp"
## [6] "cropdmg"    "cropdmgexp"
```

Let's look at variables, there are many garbage, need manipulating and cleaning


```r
unique(Stormdat3$propdmgexp)
```

```
##  [1] "K" "M" ""  "B" "m" "+" "0" "5" "6" "?" "4" "2" "3" "h" "7" "H" "-"
## [18] "1" "8"
```

```r
unique(Stormdat3$cropdmgexp)
```

```
## [1] ""  "M" "K" "m" "B" "?" "0" "k" "2"
```

```r
Stormdat3$propdmgexp[(Stormdat3$propdmgexp=='')|(Stormdat3$propdmgexp=='+')|(Stormdat3$propdmgexp=='?')|(Stormdat3$propdmgexp=='-')|(Stormdat3$propdmgexp=='0')|(Stormdat3$propdmgexp=='h')|(Stormdat3$propdmgexp=='H')] <- 0
Stormdat3$propdmgexp[(Stormdat3$propdmgexp=='K')] <- 3
Stormdat3$propdmgexp[(Stormdat3$propdmgexp=='M')|(Stormdat3$propdmgexp=='m')] <- 6
Stormdat3$propdmgexp[(Stormdat3$propdmgexp=='B')] <- 9
Stormdat3$cropdmgexp[(Stormdat3$cropdmgexp=='')|(Stormdat3$cropdmgexp=='?')|(Stormdat3$cropdmgexp=='0')] <- 1
Stormdat3$cropdmgexp[(Stormdat3$cropdmgexp=='K')|(Stormdat3$cropdmgexp=='k')] <- 3
Stormdat3$cropdmgexp[(Stormdat3$cropdmgexp=='M')|(Stormdat3$cropdmgexp=='m')] <- 6
Stormdat3$cropdmgexp[(Stormdat3$cropdmgexp=='B')] <- 9
Stormdat3$propdmgexp <- as.numeric(Stormdat3$propdmgexp)
Stormdat3$cropdmgexp <- as.numeric(Stormdat3$cropdmgexp)
Stormdat3$propdmg <- Stormdat3$propdmg*(10^Stormdat3$propdmgexp)
Stormdat3$cropdmg <- Stormdat3$cropdmg*(10^Stormdat3$cropdmgexp)
```

#evtype

```r
length(unique(Stormdat3$evtype))
```

```
## [1] 985
```

```r
Stormdat3$evtype <- gsub('.*STORM.*', 'STORM', Stormdat3$evtype)
Stormdat3$evtype <- gsub('.*FLOOD.*', 'FLOOD', Stormdat3$evtype)
Stormdat3$evtype <- gsub('.*WIND.*', 'WIND', Stormdat3$evtype)
Stormdat3$evtype <- gsub('.*TORN.*', 'TORNADO', Stormdat3$evtype)
Stormdat3$evtype <- gsub('.*HAIL.*', 'HAIL', Stormdat3$evtype)
Stormdat3$evtype <- gsub('.*HURRICANE.*', 'HURRICANE', Stormdat3$evtype)
Stormdat3$evtype <- gsub('.*RAIN.*', 'RAIN', Stormdat3$evtype)
Stormdat3$evtype <- gsub('.*SNOW.*', 'SNOW', Stormdat3$evtype)
Stormdat3$evtype <- gsub('.*COLD.*', 'COLD', Stormdat3$evtype)
Stormdat3$evtype <- gsub('.*LOW.*TEMPER.*', 'COLD', Stormdat3$evtype)
Stormdat3$evtype <- gsub('.*FROST.*', 'COLD', Stormdat3$evtype)
Stormdat3$evtype <- gsub('.*HIGH.*TEMPER.*', 'HEAT', Stormdat3$evtype)
Stormdat3$evtype <- gsub('.*HEAT.*', 'HEAT', Stormdat3$evtype)
Stormdat3$evtype <- gsub('.*FIRE.*', 'FIRE', Stormdat3$evtype)

length(unique(Stormdat3$evtype))
```

```
## [1] 433
```

Grouping Stormdata by event type and looking at fatalities, injuries and economic damage


```r
Stormdat3 <- group_by(Stormdat3, evtype)
Stormdat4 <- summarise(Stormdat3, all_fatalities=sum(fatalities), all_injuries=sum(injuries),
          all_propdmg=sum(propdmg), all_cropdmg=sum(cropdmg))

table_fatalities <- arrange(select(Stormdat4, evtype, all_fatalities), desc(all_fatalities))[1:10,]
table_fatalities
```

```
## Source: local data frame [10 x 2]
## 
##          evtype all_fatalities
##           (chr)          (dbl)
## 1       TORNADO           5636
## 2          HEAT           3138
## 3         FLOOD           1523
## 4          WIND           1235
## 5     LIGHTNING            816
## 6         STORM            633
## 7   RIP CURRENT            368
## 8     AVALANCHE            224
## 9          COLD            215
## 10 RIP CURRENTS            204
```

```r
par(mar=c(9,5,1,1))
barplot(height = table_fatalities$all_fatalities, names.arg = table_fatalities$evtype, main = 'Fatalities', las=2)
```

![plot of chunk sdatft](figure/sdatft-1.png)

```r
table_injuries <- arrange(select(Stormdat4, evtype, all_injuries), desc(all_injuries))[1:10,]
table_injuries
```

```
## Source: local data frame [10 x 2]
## 
##       evtype all_injuries
##        (chr)        (dbl)
## 1    TORNADO        91407
## 2       HEAT         9154
## 3       WIND         9041
## 4      FLOOD         8601
## 5      STORM         6691
## 6  LIGHTNING         5230
## 7       FIRE         1608
## 8       HAIL         1371
## 9  HURRICANE         1326
## 10      SNOW         1116
```

```r
table_propdmg <- arrange(select(Stormdat4, evtype, all_propdmg), desc(all_propdmg))[1:10,]
table_propdmg
```

```
## Source: local data frame [10 x 2]
## 
##       evtype  all_propdmg
##        (chr)        (dbl)
## 1      FLOOD 168061094835
## 2  HURRICANE  84656180010
## 3      STORM  73263643688
## 4    TORNADO  57003318427
## 5       HAIL  15736043018
## 6       WIND  12450581618
## 7       FIRE   8501628500
## 8       RAIN   3233664190
## 9    DROUGHT   1046106000
## 10      SNOW   1014264750
```

```r
par(mar=c(9,5,1,1))
barplot(height = table_propdmg$all_propdmg, names.arg = table_propdmg$evtype, main = 'Property damage', las=2)
```

![plot of chunk sdatft](figure/sdatft-2.png)

```r
table_cropdmg <- arrange(select(Stormdat4, evtype, all_cropdmg), desc(all_cropdmg))[1:10,]
table_cropdmg
```

```
## Source: local data frame [10 x 2]
## 
##       evtype all_cropdmg
##        (chr)       (dbl)
## 1    DROUGHT 13972566000
## 2      FLOOD 12352059100
## 3      STORM  6406919680
## 4  HURRICANE  5505292800
## 5       HAIL  3046837680
## 6       COLD  2544101500
## 7       WIND  1406229150
## 8       HEAT   904469280
## 9       RAIN   804652800
## 10    FREEZE   446225000
```

```r
par(mar=c(9,5,1,1))
barplot(height = table_cropdmg$all_cropdmg, names.arg = table_cropdmg$evtype, main = 'Crop damage', las=2)
```

![plot of chunk sdatft](figure/sdatft-3.png)

Compare table


```r
compare_table <- matrix(nrow = 10,ncol = 4)
compare_table[,1] <- arrange(Stormdat4, desc(all_fatalities))$evtype[1:10]
compare_table[,2] <- arrange(Stormdat4, desc(all_injuries))$evtype[1:10]
compare_table[,3] <- arrange(Stormdat4, desc(all_propdmg))$evtype[1:10]
compare_table[,4] <- arrange(Stormdat4, desc(all_cropdmg))$evtype[1:10]
colnames(compare_table) <- c('all_fatalities', 'all_injuries','all_propdmg', 'all_cropdmg')
compare_table
```

```
##       all_fatalities all_injuries all_propdmg all_cropdmg
##  [1,] "TORNADO"      "TORNADO"    "FLOOD"     "DROUGHT"  
##  [2,] "HEAT"         "HEAT"       "HURRICANE" "FLOOD"    
##  [3,] "FLOOD"        "WIND"       "STORM"     "STORM"    
##  [4,] "WIND"         "FLOOD"      "TORNADO"   "HURRICANE"
##  [5,] "LIGHTNING"    "STORM"      "HAIL"      "HAIL"     
##  [6,] "STORM"        "LIGHTNING"  "WIND"      "COLD"     
##  [7,] "RIP CURRENT"  "FIRE"       "FIRE"      "WIND"     
##  [8,] "AVALANCHE"    "HAIL"       "RAIN"      "HEAT"     
##  [9,] "COLD"         "HURRICANE"  "DROUGHT"   "RAIN"     
## [10,] "RIP CURRENTS" "SNOW"       "SNOW"      "FREEZE"
```

 
