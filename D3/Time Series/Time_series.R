# install.packages("readr")
# install.packages("forecast")
# install.packages("tseries")
# install.packages("ggplot2")
# install.packages("readxl")
# install.packages("httr")
# install.packages("magrittr")
# install.packages("tidyverse")
# install.packages("lubridate")
# install.packages("MASS")
# install.packages("seasonal")
# install.packages("strucchange")
# install.packages("tswge")
# install.packages("Metrics")
# install.packages("lmtest")

library(readr)
library(forecast)
library(tseries)
library(ggplot2)
library(readxl)
library(httr)
library(httpgd)
library(magrittr)
library(tidyverse)
library(lubridate)
library(MASS)
library(seasonal)
library(strucchange)
library(tswge)
library(Metrics)
library(lmtest)


df <- read.csv("./Datasets/cleaned_db.csv")
df$Crash_Date <- as.Date(df$Crash_Date)


# - - - - - - - - MONTHLY ACCIDENTS - - - - - - - - -

monthly_counts <- aggregate(list(count = df$Crash_Month),
                  by = list(year = df$Crash_Year, month = df$Crash_Month),
                  length)

timeseries <- ts(monthly_counts[3], start = c(2013, 1),
                 end = c(2023, 3), frequency = 12)
timeseries

autoplot(timeseries) + ggtitle("Accidents in Austin, Texas") + ylab("Number of accidents")


subset_list <- list()
for (year in 2013:2022) {
  subset <- window(timeseries, start = c(year), end = c(year+1))
  subset_list[[year-2012]] <- subset
}
subset_list[[11]] <- window(timeseries, start = c(2023))

lapply(subset_list, function(subset) autoplot(subset) + ggtitle("Accidents in Austin, Texas") + ylab("Number of accidents"))


ggseasonplot(timeseries, main = "Seasonal Plot: Stackoverflow Questions Count")

# - - - - - - - - MONTHLY ACCIDENTS - - - - - - - - -


# - - - - - - - - TRANSFORMING DATASET - - - - - - - - -

start_date <- as.Date("2013-01-01")
end_date <- as.Date("2023-03-10")
dates <- seq(start_date, end_date, by = "day")
time_series_df <- data.frame(Date = dates)
time_series_df[, 2:25] <- 0
colnames(time_series_df) <- c("Dates", c(seq(0, 23)))

for (i in 1:nrow(df)) {
  date <- df[i, "Crash_Date"]
  hour <- df[i, "Crash_Time"] %/% 100 + 2

  time_series_df[time_series_df$Date == date, hour] <- time_series_df[time_series_df$Date == date, hour] + 1
}

# - - - - - - - - TRANSFORMING DATASET - - - - - - - - -


# - - - - - - - - HOURLY ACCIDENTS - - - - - - - - -

df2 <- time_series_df

hourly <- data.frame(colSums(df2[, -c(1, 2)]))
hourly

hour_timeseries <- ts(hourly, start = c(0), end = c(24), frequency = 1)
hour_timeseries
autoplot(hour_timeseries) + ggtitle("Accidents in Austin, Texas") + ylab("Number of accidents")

# - - - - - - - - HOURLY ACCIDENTS - - - - - - - - -


# - - - - - - - - DAILY ACCIDENTS - - - - - - - - -

daily <- data.frame(Sum = rowSums(df2[, -c(1, 2)]))

day_timeseries <- ts(daily, start = c(2013), frequency = 365)

autoplot(day_timeseries) + ggtitle("Accidents in Austin, Texas") + ylab("Number of accidents")


subset_list <- list()
for (year in 2013:2022) {
  subset <- window(day_timeseries, start = c(year), end = c(year+1))
  subset_list[[year-2012]] <- subset
}
subset_list[[11]] <- window(day_timeseries, start = c(2023))

lapply(subset_list, function(subset) autoplot(subset) + ggtitle("Accidents in Austin, Texas") + ylab("Number of accidents"))

ggseasonplot(day_timeseries, main = "Seasonal Plot: Stackoverflow Questions Count")

# - - - - - - - - DAILY ACCIDENTS - - - - - - - - -
