library(data.table)
data<- read.csv("C:/Users/USER/Downloads/repdata_data_StormData.csv.bz2", header = TRUE, as.is= TRUE)
head(data)
names(data)
str(data)
sort(table(data$EVTYPE), decreasing = TRUE)[1:200]
impdata <- subset(data, select = c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP"))
impdata






impdata$EVTYPE[grep("HAIL", impdata$EVTYPE, ignore.case = TRUE)] <- "HAIL"
impdata$EVTYPE[grep("HEAT", impdata$EVTYPE, ignore.case = TRUE)] <- "HEAT"
impdata$EVTYPE[grep("FLOOD", impdata$EVTYPE, ignore.case = TRUE)] <- "FLOOD"
impdata$EVTYPE[grep("WIND", impdata$EVTYPE, ignore.case = TRUE)] <- "WIND"
impdata$EVTYPE[grep("STORM", impdata$EVTYPE, ignore.case = TRUE)] <- "STORM"
impdata$EVTYPE[grep("SNOW", impdata$EVTYPE, ignore.case = TRUE)] <- "SNOW"
impdata$EVTYPE[grep("TORNADO", impdata$EVTYPE, ignore.case = TRUE)] <- "TORNADO"
impdata$EVTYPE[grep("WINTER", impdata$EVTYPE, ignore.case = TRUE)] <- "WINTER"
impdata$EVTYPE[grep("RAIN", impdata$EVTYPE, ignore.case = TRUE)] <- "RAIN"



sort(table(impdata$EVTYPE), decreasing = TRUE)


harmfulevents <- aggregate(cbind(FATALITIES, INJURIES) ~ EVTYPE, impdata, sum, na.rm = TRUE)
harm<- harmfulevents[order(-harmfulevents$FATALITIES),][1:10,]
library(ggplot2)
ggplot(harm, aes(x = reorder(EVTYPE, -FATALITIES), y = FATALITIES, fill =as.factor(INJURIES))) +
  geom_bar(stat = "identity")+ scale_fill_brewer(palette = "Spectral") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab(NULL)+
  ggtitle(paste("Top", 10, "most harmful weather events in the US"))

decode.units <- function(d) {switch(d, H = 100, K = 1000, M = 1e+06, 
                                    B = 1e+09, `0` = 1, `1` = 10, `2` = 100, `3` = 1000, 
                                    `4` = 10000, `5` = 1e+05, `6` = 1e+06, `7` = 1e+07, 
                                    `8` = 1e+08, `9` = 1e+09, 0)}
impdata$DMG <- impdata$PROPDMG * sapply(impdata$PROPDMGEXP, decode.units) + 
  impdata$CROPDMG * sapply(impdata$CROPDMGEXP, decode.units)

 harmful1<- aggregate(DMG ~ EVTYPE, impdata, sum, na.rm=TRUE)
  ecoharm<- harmful1[order(-harmful1$DMG),][1:10,]
  ggplot(ecoharm, aes(x = reorder(EVTYPE, -DMG), y = DMG) )+
    geom_bar(stat = "identity" , fill ="green")+ scale_fill_brewer(palette = "Spectral") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab(NULL)+
      ylab("Economic consequence in USD")+
      ggtitle(paste("Top", 10 , "events which have the greatest economic consequences in the US"))

