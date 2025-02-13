---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
    code_folding: hide
---



## Loading and preprocessing the data
The data is ingested by first downloading the .zip file from the url provided in the assignment. The first time the code is run the file is downloaded and unzipped in to separate folder "/activity_data". If the download and extraction has already been done, the file is not downloaded again.

```r
dataset_url <- paste0("https://d396qusza40orc.cloudfront.net/",
                      "repdata%2Fdata%2Factivity.zip")
downloaded_zipfile <- "./activity.zip"

if (!file.exists(downloaded_zipfile)) {
        download.file(dataset_url, destfile = downloaded_zipfile, 
                      method = "curl", mode = "wb")
}
unzipped_file_folder <- "./activity_data"

if (!file.exists(unzipped_file_folder)) {
        unzip(downloaded_zipfile, exdir = unzipped_file_folder,
              unzip = "internal")
}
```
With the data file successfully downloaded, the 'activity.csv' file is read into the 'activity' variable.

```r
activity <- read.csv(file = paste0(unzipped_file_folder, "/", "activity.csv"))
```
One of the first checks performed was to access the prevalence of 'NAs' in the step data.

```
##   overall_frac_NA total_NAs
## 1       0.1311475      2304
```
The 'activity' dataframe contains a total of 2304 NAs that accounts for ~13% of all of the rows of step data. We can look a little closer at the impact of these NAs by checking how they are distributed across the 5-minute time intervals.


```r
(NA_by_interval <- activity %>%
        group_by(interval) %>%
        summarize(frac_NA = mean(is.na(steps)), count_NA = sum(is.na(steps))))
```

```
## # A tibble: 288 × 3
##    interval frac_NA count_NA
##       <int>   <dbl>    <int>
##  1        0   0.131        8
##  2        5   0.131        8
##  3       10   0.131        8
##  4       15   0.131        8
##  5       20   0.131        8
##  6       25   0.131        8
##  7       30   0.131        8
##  8       35   0.131        8
##  9       40   0.131        8
## 10       45   0.131        8
## # … with 278 more rows
```
From the first ten rows of intervals we can see that each interval has eight NAs. With 288 5-minute intervals and 8 NAs per intervals that gives the 2304 total number of NAs we found above. It looks like the NAs are uniformly spread across the intervals and do not need any additional consideration.

## What is mean total number of steps taken per day?
The mean total number of daily steps is calculated by grouping the activity dataframe by the 'date' variable and summarizing the sum of the daily steps with the NA values removed from the sum.

```r
total_steps_per_day <- activity %>%
        group_by(date) %>%
        summarize(daily_step_total=sum(steps, na.rm = TRUE))
```
With the 'total_steps_per_day' (this excludes NA values), we can create a histogram to get an idea of how the number of total daily steps varies. Note: I tried a few different bin widths and decided that 15 bins did an adequate job of showing the overall pattern of variation.

```r
hist(total_steps_per_day$daily_step_total,
     breaks = 15,
     main = "Total Steps per Day",
     xlab = "Total Steps per Day (NAs removed)")
```

![](PA1_template_files/figure-html/histogram_of_total_steps_per_day_w/o_NAs-1.png)<!-- -->

Two important statistics for this data are the mean and median daily step count.

```r
(daily_step_stats <-total_steps_per_day %>%
        summarize(mean_daily_steps = mean(daily_step_total),
                  median_daily_steps = median(daily_step_total)))
```

```
## # A tibble: 1 × 2
##   mean_daily_steps median_daily_steps
##              <dbl>              <int>
## 1            9354.              10395
```
The mean (9354 steps) is less than the median (10395 steps), but not so much as to raise concerns about the step data being significantly skewed.

## What is the average daily activity pattern?
We now want to turn our attention to how the step count varies across the 288 5-minute intervals in each day. This can be accomplished by grouping the activity dataframe by 'interval' and then creating summary values of the total and average steps per interval across all 61 days in the dataframe. In this analysis, the NA values are excluded.

```r
avg_steps_by_interval <- activity %>%
        group_by(interval) %>%
        summarize(tot_steps = sum(steps, na.rm=TRUE),
                  avg_steps = mean(steps, na.rm=TRUE))
```
The overall pattern of average steps per 5-minute interval can be seen in the following time series plot.

```r
with(avg_steps_by_interval, plot(interval, avg_steps,
        type = "l",
        main = "Average Steps per 5-minute Interval",
        sub = "(NAs removed)",
        xlab = "5-min Interval",
        ylab = "Average Step Count"))
```

![](PA1_template_files/figure-html/time_series_plot_of_avg_steps_per_interval_w/o_NAs-1.png)<!-- -->

The maximum average step count in an interval is calculated by finding the row with the highest average step count. (again noting that we have excluded the NAs in this view of the data)

```r
max_avg_steps_by_interval <- activity %>%
        group_by(interval) %>%
        summarize(avg_steps = mean(steps, na.rm=TRUE)) %>%
        slice_max(order_by = avg_steps)
```
The highest average step count (206 steps) is found in the interval 835. It looks like a lot of the daily step total occurs in the morning wthe peak around 8:35am.

## Imputing missing values
Since ~13% of the step count data is missing, we can consider replacing the NA values with a meaningful substitute value. We start by creating a new dataframe to include imputed step counts.

```r
imputed_activity <- activity %>% add_column(imputed_steps = NA)
```
For this analysis, I chose to replace the missing values with the average step counts calculated for each 5-minute interval (with the NAs removed). This seems reasonable since the average step count for each interval was quite variable. If the original step count for an interval was missing, the average step count for that interval replaced the NA in the 'imputed_steps' variable. If there was a step count value in the original 'steps' variable it was used in the 'imputed_steps' variable.

```r
for (i in 1:nrow(imputed_activity)) {
        if (is.na(imputed_activity$steps[i])) {
                imputed_activity$imputed_steps[i] <-
                        avg_steps_by_interval$avg_steps[activity$interval[i] ==
                                                avg_steps_by_interval$interval]
        }
        else {
                imputed_activity$imputed_steps[i] <- imputed_activity$steps[i]
        }
}

rm(i)
```
With the imputed steps we can now calculate the total daily step count by summarizing the new data by 'date'.

```r
total_steps_per_day_imputed <- imputed_activity %>%
        group_by(date) %>%
        summarize(daily_step_total_imputed=sum(imputed_steps))
```
As shown in the following histogram, using the imputed step counts, the frequency in the first bin has gone down since there are fewer zeros in the imputed step data.

```r
hist(total_steps_per_day_imputed$daily_step_total_imputed,breaks = 15,
     main = "Total Steps per Day",
     xlab = "Total Steps per Day (NAs replaced with interval mean)")
```

![](PA1_template_files/figure-html/histogram_of_daily_step_total_using_imputed_step_data-1.png)<!-- -->

We can now also calculate the mean and median daily step counts using the imputed step data.

```r
daily_step_stats_imputed <-total_steps_per_day_imputed %>%
        summarize(mean_daily_steps_imputed = mean(daily_step_total_imputed),
                  median_daily_steps_imputed = median(daily_step_total_imputed))
```
The mean (10766 steps) is now equal to the median (10766 steps). It would be interesting to see if this a genaral result when the average value is used to replace an NA. The mean (median) value for the imputed data set is larger than the result with the NAs excluded. This is not suprising since we replaced the NAs with the averages. 

## Are there differences in activity patterns between weekdays and weekends?
The final step in this analysis is to assess whether there are differences in the step-related activity patterns between weekdays (Monday to Friday) and weekends (Saturday and Sunday); using the imputed step data. This involves first creating a 'week_day' variable that relates the date to the day of the week. Since there are more weekdays than weekend days, I initialized the 'type_of_day' variable with the value 'Weekday'. Where the day of the week was a weekend day, the 'type_of_day' variable was updated to 'Weekend'.


```r
imputed_activity <- imputed_activity %>%
        mutate(week_day = weekdays(as.Date(date, "%Y-%m-%d")))

imputed_activity <- imputed_activity %>% add_column(type_of_day = "Weekday")

for (i in 1:nrow(imputed_activity)) {
        if (imputed_activity$week_day[i] %in% c("Saturday", "Sunday")) {
                imputed_activity$type_of_day[i] <- "Weekend"
        }
}

rm(i)
```
Since we want to compare the step-related activity patterns between weekdays and weekends, we can calculate the average steps per 5-minute interval for the weekday subset of the 'imputed-activity' and the weekend subset of the 'imputed_activity' dataframe. These two subsets are recombined (row-wise) to provide a single dataframe for creating time series plot dor each type of day.

```r
imputed_avg_steps_by_interval_weekday <- imputed_activity %>%
        filter(type_of_day=="Weekday") %>%
        group_by(interval) %>%
        summarize(tot_steps = sum(imputed_steps),
                  avg_steps = mean(imputed_steps)) %>%
        mutate(type_of_day = "Weekday")

imputed_avg_steps_by_interval_weekend <- imputed_activity %>%
        filter(type_of_day=="Weekend") %>%
        group_by(interval) %>%
        summarize(tot_steps = sum(imputed_steps),
                  avg_steps = mean(imputed_steps)) %>%
        mutate(type_of_day = "Weekend")

imputed_avg_steps_by_interval <- rbind(imputed_avg_steps_by_interval_weekday,
                                       imputed_avg_steps_by_interval_weekend)
```
A graphical comparison of the step-related activity patterns between weekdays and weekends is shown in the following figure with one panel for the 'Weekday' data and another for the 'Weekend' data.

```r
ggplot(imputed_avg_steps_by_interval, aes(interval, avg_steps)) +
        geom_line() + facet_grid(type_of_day ~ .) +
        ggtitle("Average Steps per 5-minute Interval") +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(x = "5-minute Interval",
             y = "Average Step Count",
             caption = "(NAs replaced with interval mean)")
```

![](PA1_template_files/figure-html/two_panel_time_series_plot_of_avg_interval_step_data_for_Weekdays_and_Weekends-1.png)<!-- -->

One quick observation is that the spike in activity between 8:00 and 10:00am on weekdays is not as prominent in the weekend data and there seem to be more high step count intervals overall.
