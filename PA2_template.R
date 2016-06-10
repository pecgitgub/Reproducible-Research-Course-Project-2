library(dplyr)
Downloading Storm data file, cleaning data and selecting required columns
Stormdat <- download.file('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2', 'Stormdata.csv.bz2')
Stormdat <- read.csv('Stormdata.csv.bz2', stringsAsFactors = F)

Stormdat2 <- Stormdat
names(Stormdat2) <- tolower(names(Stormdat2))

Stormdat3 <- select(tbl_df(Stormdat2), evtype, fatalities:cropdmgexp)
sum(is.na(Stormdat3))
names(Stormdat3)
Let’s look at variables, there are many garbage, need manipulating and cleaning
unique(Stormdat3$propdmgexp)
unique(Stormdat3$cropdmgexp)
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

#evtype

length(unique(Stormdat3$evtype))

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
Grouping Stormdata by event type and looking at fatalities, injuries and economic damage
Stormdat3 <- group_by(Stormdat3, evtype)
Stormdat4 <- summarise(Stormdat3, all_fatalities=sum(fatalities), all_injuries=sum(injuries),
          all_propdmg=sum(propdmg), all_cropdmg=sum(cropdmg))

table_fatalities <- arrange(select(Stormdat4, evtype, all_fatalities), desc(all_fatalities))[1:10,]
table_fatalities
par(mar=c(9,5,1,1))
barplot(height = table_fatalities$all_fatalities, names.arg = table_fatalities$evtype, main = 'Fatalities', las=2)


table_injuries <- arrange(select(Stormdat4, evtype, all_injuries), desc(all_injuries))[1:10,]
table_injuries
table_propdmg <- arrange(select(Stormdat4, evtype, all_propdmg), desc(all_propdmg))[1:10,]
table_propdmg
par(mar=c(9,5,1,1))
barplot(height = table_propdmg$all_propdmg, names.arg = table_propdmg$evtype, main = 'Property damage', las=2)


table_cropdmg <- arrange(select(Stormdat4, evtype, all_cropdmg), desc(all_cropdmg))[1:10,]
table_cropdmg
par(mar=c(9,5,1,1))
barplot(height = table_cropdmg$all_cropdmg, names.arg = table_cropdmg$evtype, main = 'Crop damage', las=2)


Pivot table
compare_table <- matrix(nrow = 10,ncol = 4)
compare_table[,1] <- arrange(Stormdat4, desc(all_fatalities))$evtype[1:10]
compare_table[,2] <- arrange(Stormdat4, desc(all_injuries))$evtype[1:10]
compare_table[,3] <- arrange(Stormdat4, desc(all_propdmg))$evtype[1:10]
compare_table[,4] <- arrange(Stormdat4, desc(all_cropdmg))$evtype[1:10]
colnames(compare_table) <- c('all_fatalities', 'all_injuries','all_propdmg', 'all_cropdmg')
compare_table
