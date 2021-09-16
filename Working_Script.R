library(tidyverse)
library(lubridate)
hist(date, steps, data = activity)


avg_steps_by_interval <- activity %>%
        group_by(interval) %>%
        summarize(avg_steps = mean(steps, na.rm=TRUE))

with(avg_steps_by_interval, plot(interval, avg_steps))
    
max_avg_steps_by_interval <- activity %>%
        group_by(interval) %>%
        summarize(avg_steps = mean(steps, na.rm=TRUE)) %>%
        slice_max(order_by = avg_steps)

activity$julian <- yday(activity$date)

total_steps_day <- activity %>%
        group_by(julian) %>%
        summarize(daily_step_total=sum(steps, na.rm=TRUE))

hist(total_steps_day$daily_step_total, breaks = 15)

activity$week_day <- wday(activity$date, label = TRUE)
