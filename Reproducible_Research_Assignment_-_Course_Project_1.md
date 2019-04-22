Introduction
------------

It is now possible to collect a large amount of data about personal
movement using activity monitoring devices such as a Fitbit, Nike
Fuelband, or Jawbone Up. These type of devices are part of the
"quantified self" movement - a group of enthusiasts who take
measurements about themselves regularly to improve their health, to find
patterns in their behavior, or because they are tech geeks. But these
data remain under-utilized both because the raw data are hard to obtain
and there is a lack of statistical methods and software for processing
and interpreting the data.

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012 and
include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

-   Dataset: [Activity monitoring
    data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)
    \[52K\]

The variables included in this dataset are:

-   **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as **<span style="color:red">NA</span>**)

-   **date**: The date on which the measurement was taken in YYYY-MM-DD
    format

-   **interval**: Identifier for the 5-minute interval in which
    measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there
are a total of 17,568 observations in this dataset.

Question 1: Loading and preprocessing the data
----------------------------------------------

The first step in order to analyse the data, is to load all necessary R
packages as well as the dataset.

    knitr::opts_chunk$set(echo = TRUE,
                          fig.path = 'figure/')


    library(here)

    ## here() starts at C:/Users/pekst/Documents/Coursera/R

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(ggplot2)
    library(DMwR)

    ## Warning: package 'DMwR' was built under R version 3.5.3

    ## Loading required package: lattice

    ## Loading required package: grid

    library(RColorBrewer)

    debug_mode <- FALSE

    ##--Load the data
    activity_data_raw <- read.csv(here::here(file.path("data", "activity.csv")))

The second step is to identify which data contained in the dataset
requires additional preparation/transformation. Checking the data for
**NA\`s** can also be done at this point.

Verifying the class of each column in the dataset is a good starting
point. (Ensuring that date fields in particular are of the correct class
is important for any analyses pivoted around dates, days, times etc.) We
can identify the class of each column contained in the dataset using the
`lapply` function:

    lapply(activity_data_raw, class)

    ## $steps
    ## [1] "integer"
    ## 
    ## $date
    ## [1] "factor"
    ## 
    ## $interval
    ## [1] "integer"

    colSums(is.na(activity_data_raw))

    ##    steps     date interval 
    ##     2304        0        0

From the above result, we can see that the date field is of class
`factor` and that the steps field contains **2304** NA\`s.

We can now use the dplyr function `mutate` to convert the date field to
class `Date` and the `na.omit` function to remove all rows containing
NA\`s from the dataset.

    ##--Prepare data for analysis
    activity_data <- activity_data_raw %>%
        ##--Convert date from factor to date format
        mutate(date = as.Date(date, "%Y-%m-%d")) %>% 
        ##Remove NA`s
        na.omit()

    lapply(activity_data, class)

    ## $steps
    ## [1] "integer"
    ## 
    ## $date
    ## [1] "Date"
    ## 
    ## $interval
    ## [1] "integer"

    colSums(is.na(activity_data))

    ##    steps     date interval 
    ##        0        0        0

Question 2: What is mean total number of steps taken per day?
-------------------------------------------------------------

In order to view the data per day, the dataset needs to be transformed
to summarise steps per day:

    daily_data <- activity_data %>%
        select(-interval) %>% 
        group_by(date) %>% 
        summarise(daily_steps = sum(steps))

Now that we have a daily dataset, we can proceed to calculating the mean
and median steps. We can also create a histogram of the total steps per
day:

    ##--Calculate mean and median of daily steps
    daily_mean <- format(round(mean(daily_data$daily_steps), digits = 2), big.mark = "'")
    daily_median <- format(round(median(daily_data$daily_steps), digits = 2), big.mark = "'")

    ##--Plot histogram
    ggplot(daily_data, aes(daily_data$daily_steps)) + 
        geom_histogram(aes(fill = ..count..), 
                       breaks = c(seq(0, 25000, length.out = 6)),
                       col = "black",
                       size = 1.3,
                       alpha = 0.9) +
        ##--Add reference line for mean and median
        geom_vline(aes(xintercept = median(daily_data$daily_steps), colour = "green") , 
                   linetype = "solid", 
                   size = 1) +
        geom_vline(aes(xintercept = mean(daily_data$daily_steps), colour = "red"), 
                   linetype = "twodash", 
                   size = 1) +
        labs(title = "Histogram of total number of steps per day") +
        xlab("Total steps per day") +
        ylab("Count") +
        ylim(0,35) +
        scale_color_manual(name = "Statistics", 
                           values = c("green", "red"), 
                           labels = c(paste("Median =", daily_median, "steps"),
                                      paste("Mean =", daily_mean, "steps"))) +
        theme(panel.grid.major = element_line(colour = "black", linetype = "dashed"), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black"),
              plot.title = element_text(hjust = 0.5))

<img src="figure/Question 2 - Mean median and histogram-1.png" width="100%" />

The above histogram illustrates the total number of steps taken per day.
The median number of steps taken per day is **10'765** and the mean
number of steps taken per day is **10'766.19**

Question 3: What is the average daily activity pattern?
-------------------------------------------------------

To answer this question we need to summarise the data across all days to
show the mean number of steps per 5-minute interval. After that we are
able to calculate the interval containing the maximum mean steps and
visualize all this data by constructing a time series plot:

    ##--Prepare data for analysis
    interval_data <- activity_data %>%
        group_by(interval) %>% 
        summarise(mean_steps = mean(steps))

    ##--Calculate the interval containing the maximum steps
    max_steps_interval <- interval_data %>% 
        filter(mean_steps == max(mean_steps)) %>% 
        .$interval

    # Plot average number of steps by 5-minute interval
    ggplot(interval_data, aes(x = interval_data$interval, y = interval_data$mean_steps)) + 
        geom_line(color = "#5891FD", size = 0.8) +
        geom_vline(aes(xintercept = max_steps_interval, colour = "#33B900"), 
                   linetype = "twodash", 
                   size = 0.8) +
        labs(title = "Average Daily Activity Pattern", 
             y = "Average Number of Steps", 
             x = "5min Intervals") +
        scale_color_manual(name = "statistics", 
                           values = c("#33B900"), 
                           labels = c(paste("Max = ", "Interval ", max_steps_interval, sep = ""))) +
    stat_smooth(color = "#FC4E07", 
                fill = "#FFAB91",
                method = "loess") +
    theme(plot.title = element_text(hjust = 0.5))

<img src="figure/Question 3 - Max steps and time series plot-1.png" width="100%" />

The above time series plot shows the average number of steps taken per
5-minute interval across all days in the dataset.

From the plot we can see that the interval = **835** contains the
maximum number of steps. This means that on average the maximum number
of daily steps are taken between 8:35am and 8:40am.

Question 4: Imputing missing values
-----------------------------------

Before we devise a strategy to deal with NA values in the dataset, let's
first analyse how many intervals within the dataset contain NA's

    ##--Check all columns for NA`s
    colSums(is.na(activity_data_raw))

    ##    steps     date interval 
    ##     2304        0        0

    ##--Calculate total amount of rows in dataset
    df_rows <- nrow(activity_data_raw)
    print(paste("The data contains", format(df_rows, big.mark = "'"), "rows in total"))

    ## [1] "The data contains 17'568 rows in total"

    ##--Calculate amount of rows conataining NA's
    na_rows <- length(which(is.na(activity_data_raw)))
    print(paste("The data contains", format(na_rows, big.mark = "'"), "rows with NA values"))

    ## [1] "The data contains 2'304 rows with NA values"

    ##--Calculate percentage NA`s
    percentage_na <- round(na_rows/df_rows*100, digits = 2)
    print(paste(percentage_na, "% of the data is NA", sep = ""))

    ## [1] "13.11% of the data is NA"

From the above analysis, we see that **2'304** of the **17'568** rows
contain NA's.

Based on the fact that **13.11%** of the data is NA, I recommend using
the k Nearest Neighbours method to replace the NA values with a weighted
average of the k nearest neighbours (I have chosen to set k = 5% of the
total days, this ensures that only the closest neighbours are used)

    daily_data_incl_NA <- activity_data %>% 
        select(-interval) %>% 
        group_by(date) %>% 
        summarise(daily_steps = sum(steps))

    ##--Calculate 5% of total days to use as k-value
    total_days <- as.numeric(length(daily_data_incl_NA$date))
    k_value <- total_days*0.05

    ##--Use knn to predict NA`s with k = total days in dataset
    imputed_activity_data <- knnImputation(activity_data_raw, k = k_value, meth = "weighAvg") %>% 
        mutate(steps = round(steps, digits = 0))

    ##--Test to ensure no NA`s remain
    if(length(which(is.na(imputed_activity_data))) == 0) {
        print("NA Replacement Successful")
    } else {
        print(paste("ERROR: NA Replacement Unsuccessful,", length(which(is.na(imputed_activity_data))), "NA`s Remain"))
    }

    ## [1] "NA Replacement Successful"

    if(debug_mode) {
        ##--Create df with NA and replacement values for visual comaparison
        original_steps <- activity_data %>%
            select(steps) %>% 
            rename(original_steps = steps)
        
        compare_na_replace <- cbind(imputed_activity_data, original_steps) %>% 
            select(date,
                   interval,
                   steps,
                   original_steps)
    }

We have now created a new dataset that is equal to the original dataset
but with the missing data filled in. Using this new dataset we can make
a histogram of the total number of steps taken each day and calculate
and report the mean and median total number of steps taken per day.

    ##--Prepare data for analysis
    imputed_daily_data <- imputed_activity_data %>%
        ##--Convert date from factor to date format
        mutate(date = as.Date(date, "%Y-%m-%d")) %>%
        select(-interval) %>% 
        group_by(date) %>% 
        summarise(daily_steps = sum(steps))

    ##--Calculate mean and median of daily steps
    imputed_daily_mean <- format(round(mean(imputed_daily_data$daily_steps), digits = 2), big.mark = "'")
    imputed_daily_median <- format(round(median(imputed_daily_data$daily_steps), digits = 2), big.mark = "'")

    ##--Plot histogram
    ggplot(imputed_daily_data, aes(imputed_daily_data$daily_steps)) + 
        geom_histogram(aes(fill = ..count..), 
                       breaks = c(seq(0, 25000, length.out = 6)),
                       col = "black",
                       size = 1.3,
                       alpha = 0.9) +
        ##--Add reference line for mean and median
        geom_vline(aes(xintercept = median(imputed_daily_data$daily_steps), colour = "green") , 
                   linetype = "solid", 
                   size = 1) +
        geom_vline(aes(xintercept = mean(imputed_daily_data$daily_steps), colour = "red"), 
                   linetype = "twodash", 
                   size = 1) +
        labs(title = "Histogram of total number of steps per day (NA`s replaced") +
        xlab("Total steps per day") +
        ylab("Count") +
        ylim(0,35) +
        scale_color_manual(name = "statistics", 
                           values = c("green", "red"), 
                           labels = c(paste("Median =", imputed_daily_median),
                                      paste("Mean =", imputed_daily_mean))) +
        theme(panel.grid.major = element_line(colour = "black", linetype = "dashed"), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black"),
              plot.title = element_text(hjust = 0.5))

<img src="figure/Question 4  - Imputed values steps per day histogram-1.png" width="100%" />

The above histogram illustrates the total number of steps taken per day
when the missing values are replaced using k Nearest Neighbours. With
replaced NA values the median number of steps taken per day is
**10'600** and the mean number of steps taken per day is **10'529.26**,
compared to **10'765** and **10'766.19** respectively when the NA values
are simply removed.

As we can see, both the median and the mean have reduced after missing
values were imputed, which in my opinion is a more accurate
representation. The reason for this is that omitting NA values will in
most cases create bias, especially when examining the median and mean.

Question 5: Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------------------

In order to analyse the differences in activity patterns between
weekdays and weekends, we first need to create a variable to identify
which dates are weekdays and which are weekends:

    imputed_activity_data <- imputed_activity_data %>%
        ##--Convert date from factor to date format
        mutate(date = as.Date(date, "%Y-%m-%d"),
               weekday_name = weekdays(date),
               type = ifelse(weekday_name %in% c("Saturday", "Sunday"), 
                             "Weekend", 
                             "Weekday") %>%
                   factor(ordered = FALSE,
                          levels = c("Weekend",
                                     "Weekday"))) 

We can now eplore the differences between the weekday and weekend
activity patterns using a panel plot containing time series plots for
weekday and weekend activity:

    daily_imputed_activity_data <- imputed_activity_data %>% 
        group_by(interval, type) %>% 
        summarise(mean_steps = mean(steps))

    # Plot Average steps across weekday/weekend vs 5-min interval Time Series
    ggplot(daily_imputed_activity_data, aes(x = interval, y = mean_steps, color = type)) + 
        geom_line() + 
        facet_wrap(~type, ncol = 1, nrow=2) +
        labs(title = "Average Number of Steps Taken vs 5-min Interval on Weekday/Weekend", 
             y = "Average Number of Steps", 
             x = "5-min Interval Times Series")

<img src="figure/Question 5 - Imputed values weekdays vs weekends panel plot-1.png" width="100%" />
