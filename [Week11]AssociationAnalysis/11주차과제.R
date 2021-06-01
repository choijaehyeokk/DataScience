library(arules)
library(arulesViz)
library(shinythemes)
library(pmml)
library(tidyverse)
library(reshape2)
rm(list=ls())

#=====1번째 목차=====#
df <- read.csv(file = "C:/Users/user/Desktop/3학년2학기/데이터사이언스/과제/11주/airpollution/Measurement_info.csv",header=TRUE,sep = ",",quote = "\"",fill = TRUE, fileEncoding = "UTF-8")
eval_table <- read.csv(file = "C:/Users/user/Desktop/3학년2학기/데이터사이언스/과제/11주/airpollution/Measurement_item_info.csv",header=TRUE,sep = ",",quote = "\"",fill = TRUE, fileEncoding = "UTF-8")
length(eval_table)
#=====2번째 목차=====#
df <- na.omit(df[df$Instrument.status == 0,]) # 결측치 제거
# 데이터 가공
df <- dcast(data = df, Measurement.date + Station.code ~ Item.code, 
            value.var = "Average.value")
# 날짜 나누기
df <- separate(data = df, col = Measurement.date, 
               sep = ' ', into = c("date", "time"))
colnames(df) <- c("date", "time", "station", "SO2", "NO2", "CO", "O3", "PM10", "PM2.5")
df

# 일 평균내기
df_daymean <- aggregate(cbind(SO2, NO2, CO, O3, PM10, PM2.5) ~ date+station, df, FUN = mean)

df_daymean$SO2_eval <- ifelse(df_daymean$SO2 <= eval_table$Good.Blue.[1], 'Good',
                              ifelse(df_daymean$SO2 <= eval_table$Normal.Green.[1],'Normal',
                                     ifelse(df_daymean$SO2 <= eval_table$Bad.Yellow.[1],'Bad',
                                            'Very Bad')))

df_daymean$NO2_eval <- ifelse(df_daymean$NO2 <= eval_table$Good.Blue.[2], 'Good',
                              ifelse(df_daymean$NO2 <= eval_table$Normal.Green.[2],'Normal',
                                     ifelse(df_daymean$NO2 <= eval_table$Bad.Yellow.[2],'Bad',
                                            'Very Bad')))

df_daymean$CO_eval <- ifelse(df_daymean$CO <= eval_table$Good.Blue.[3], 'Good',
                             ifelse(df_daymean$CO <= eval_table$Normal.Green.[3],'Normal',
                                    ifelse(df_daymean$CO <= eval_table$Bad.Yellow.[3],'Bad',
                                           'Very Bad')))

df_daymean$O3_eval <- ifelse(df_daymean$O3 <= eval_table$Good.Blue.[4], 'Good',
                             ifelse(df_daymean$O3 <= eval_table$Normal.Green.[4],'Normal',
                                    ifelse(df_daymean$O3 <= eval_table$Bad.Yellow.[4],'Bad',
                                           'Very Bad')))

df_daymean$PM10_eval <- ifelse(df_daymean$PM10 <= eval_table$Good.Blue.[5], 'Good',
                               ifelse(df_daymean$PM10 <= eval_table$Normal.Green.[5],'Normal',
                                      ifelse(df_daymean$PM10 <= eval_table$Bad.Yellow.[5],'Bad',
                                             'Very Bad')))

df_daymean$PM2.5_eval <- ifelse(df_daymean$PM2.5 <= eval_table$Good.Blue.[6], 'Good',
                                ifelse(df_daymean$PM2.5 <= eval_table$Normal.Green.[6],'Normal',
                                       ifelse(df_daymean$PM2.5 <= eval_table$Bad.Yellow.[6],'Bad',
                                              'Very Bad')))
date <- as.factor(df_daymean$date)
station <- as.factor(df_daymean$station)
so2 <- as.factor(df_daymean$SO2_eval)
no2 <- as.factor(df_daymean$NO2_eval)
co <- as.factor(df_daymean$CO_eval)
o3 <- as.factor(df_daymean$O3_eval)
pm10 <- as.factor(df_daymean$PM10_eval)
pm2.5 <- as.factor(df_daymean$PM2.5_eval)

airpollution<- data.frame(date,station, so2, no2, co, o3, pm10, pm2.5)
airpollution

#=====3번째 목차=====#
airpollution.trans <- as(airpollution,"transactions")
airpollution.trans

class(airpollution.trans)
inspect(head(airpollution.trans, 3))

frequentItems <- eclat (airpollution.trans,
                        parameter = list(supp = 0.021,minlen=2,maxlen = 10)) 

inspect(sort(frequentItems)[1:50])
inspect(head(frequentItems, 3))
summary(frequentItems)

itemFrequencyPlot(airpollution.trans,topN=12, 
                  type="absolute", 
                  main="Item Frequency") 

#=====4번째 목차=====#
rules1 <- apriori (airpollution.trans, 
                  parameter = list(
                    supp = 0.005,
                    conf = 0.1,
                    minlen=2,maxlen=4
                    )) 

subsetRules <- which(rowSums(is.subset(rules1, rules1, proper = T)) > 1) 
length(subsetRules) 
rules1 <- rules1[-subsetRules]
length(rules1)
rules_conf <- sort (rules1, by="support",decreasing = TRUE)
inspect(head(rules_conf,20))

rules_lift <- sort (rules1, by="lift", decreasing=TRUE) 
inspect(head(rules_lift,20))

rules2 <- apriori(airpollution.trans,
          parameter = list (
            supp = 0.01, # min sup
            conf = 0.3,  # min cof
            minlen=2,
            maxlen = 6    # max num of elements
          ))
inspect(head(rules2, 3, by = "confidence"))
length(rules2)

subsetRules <- which(rowSums(is.subset(rules2, rules2, proper = T)) > 1) 
length(subsetRules) 
rules2 <- rules2[-subsetRules]
length(rules2)
inspect(head(rules2, 3, by = "confidence"))

rules3 <- apriori (data=airpollution.trans, 
                  parameter=list (supp=0.001,
                                  conf = 0.08,
                                  minlen = 2,
                                  maxlen = 4), 
                  appearance = list (default="lhs",
                                     rhs="co=Good"), 
                  control = list (verbose=F))

rules_conf <- sort (rules3, by="confidence", decreasing=TRUE) 
inspect(head(rules_conf,20))

inspect(airpollution.trans)

rules2 <- apriori (data=airpollution.trans, 
                  parameter=list (supp=0.001,
                                  conf = 0.05,
                                  minlen=2), 
                  appearance = list(default="rhs",
                                    lhs="station=121"), 
                  control = list (verbose=F))

inspect(rules2)


rules_conf <- sort (rules2, by="confidence", decreasing=TRUE) 
inspect(head(rules_conf,10))

rules_lift <- sort (rules2, by="lift", decreasing=TRUE)
inspect(head(rules_lift,10))

rules_support <- sort (rules2, by="support", decreasing=TRUE)
inspect(head(rules_support,30))

