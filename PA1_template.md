---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

### Loading and preprocessing the data

``` r
#Load dataset
Data <- read.csv(unz("activity.zip", "activity.csv"))

#Preprocess by converting data types
Data$steps <- as.numeric(Data$steps)
Data$date <- as.Date(Data$date)
```

### What is mean total number of steps taken per day?

``` r
# Find number of steps per day
daily_steps <- Data %>%
  group_by(date) %>%
  summarise(daily_steps = sum(steps, na.rm = TRUE))

# Calculate mean and median steps per day
mean_steps <- round(mean(daily_steps$daily_steps, na.rm = TRUE, digits = 0))
median_steps <- median(daily_steps$daily_steps, na.rm = TRUE)
```
The average number of steps per day was 9354 steps per day. The median number of steps per day was 10395 steps per day.

The histogram showing the total number of steps / day is seen below.

``` r
# Create a histogram showing of daily steps
ggplot(daily_steps, aes(x= daily_steps)) +
        geom_histogram(bins = 60, color="black", fill="lightblue", na.rm = TRUE)+
        xlab("Number of steps") +
        ylab("Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

### What is the average daily activity pattern?
A time series plot showing the average daily activity pattern is included below.


``` r
# Calculate average steps per interval per day
mean_steps_by_interval <- Data %>% 
        group_by(interval) %>% 
        summarise(mean_steps_by_interval = mean(steps, na.rm = TRUE))

# Plotting the time series
g <- ggplot(mean_steps_by_interval, aes(x = interval, y = mean_steps_by_interval)) +
  geom_line(color = "lightblue") + 
  labs(
    title = "Average Steps Taken Per Interval",
    x = "5-minute Interval",
    y = "Average Number of Steps"
  ) +
  theme_minimal()

print(g)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


``` r
#Find the 5 minute interval across all days which has he maximum number of steps.
max_step_interval <- Data %>%
  arrange(desc(steps)) %>%
  slice(1)  # Get the first row, which has the maximum steps

max_step_interval
```

```
##   steps       date interval
## 1   806 2012-11-27      615
```

The five minute interval with the maximum number of steps was 806 steps, which occured 27.11.2012 at interval 615. 

### Imputing missing values

``` r
#Find and report the total number of missing values
n_missing <- sum(is.na(Data$steps))
```

The total number of missing values in the dataset was 2304. 


``` r
#Replace each missing value with the global mean (for simplicity).
global_mean <- mean(Data$steps, na.rm = TRUE)

imputed_data <- Data %>%
  mutate(steps = ifelse(is.na(steps), global_mean, steps))
```

A new dataset with imputed data is created using the code above, and saved as imputed_data.


``` r
#Recalculate mean and median values to check for differences.
daily_steps_imputed <- imputed_data %>%
  group_by(date) %>%
  summarise(daily_steps_imputed = sum(steps, na.rm = TRUE))

# Calculate mean and median steps per day
mean_steps_imputed <- round(mean(daily_steps_imputed$daily_steps_imputed, na.rm = TRUE, digits = 0))
median_steps_imputed <- round(median(daily_steps_imputed$daily_steps_imputed, na.rm = TRUE))

#check for differences:
mean_diff <- round(mean_steps_imputed - mean_steps)
median_diff <- round(median_steps_imputed - median_steps)
```

Using imputed values, the new mean and median was 10766 and 10766. The difference in mean and median was 1412 and 371 steps per day, respectively. The histogram featuring the imputed means is included below.


``` r
# Create a histogram showing of daily steps
ggplot(daily_steps_imputed, aes(x= daily_steps_imputed)) +
        geom_histogram(bins = 60, color="black", fill="lightblue", na.rm = TRUE)+
        xlab("Number of steps imputed") +
        ylab("Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

### Are there differences in activity patterns between weekdays and weekends?

A panel plot showing the differences in activity pattern between weekdays and weekends is included below. The plots shows slight differences in activity in weekends compared to weekdays. 


``` r
# Create factor variable with two levels: weekday and weekend
Data <- Data %>% 
  mutate(
    day_type = ifelse(
      weekdays(as.Date(date)) %in% c("Saturday", "Sunday"),
      "weekend",
      "weekday"
    )
  ) %>%
  mutate(day_type = factor(day_type, levels = c("weekday", "weekend")))

# Calculate average steps for each interval by day type
average_steps <- Data %>%
  group_by(interval, day_type) %>%
  summarise(mean_steps = mean(steps, na.rm = TRUE), .groups = 'drop')

# Plotting the time series
ggplot(average_steps, aes(x = interval, y = mean_steps)) +
  geom_line(color = "lightblue") +
  labs(
    title = "Average Steps per Interval: Weekdays vs Weekends",
    x = "5-minute Interval",
    y = "Average Number of Steps"
  ) +
  facet_wrap(~day_type, ncol = 1) +  # Panels stacked vertically
  theme_minimal()
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
