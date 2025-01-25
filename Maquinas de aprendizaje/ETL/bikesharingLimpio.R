bike<-read.csv("clean_bike_sharing_data.csv", stringsAsFactors = TRUE)
View(bike)

bike$season <- factor(bike$season, ordered = TRUE,levels = c("spring","summer",
                                                               "fall","winter"))
View(bike)
bike$weather <- factor(bike$weather, ordered = TRUE,
                       levels = c("clr_part_cloud","mist_cloudy","lt_rain_snow","hvy_rain_snow"))

library(lubridate)
bike$datetime <- ymd_hms(bike$datetime)
str(bike)
mean(bike$temp)
sd(bike$temp)
hist(marketing$twitter, main = NULL, col = "blue")