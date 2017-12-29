

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Loading and preprocessing the data
```{r echo = TRUE}
 data <- read.csv("activity.csv")
```


## What is mean total number of steps taken per day?
```{r echo=TRUE, message=TRUE, include=TRUE}
total_steps <- tapply(data$steps, data$date, FUN = sum, na.rm = TRUE)
hist(total_steps, breaks = 30, xlab = "total number of steps taken each day", col ="yellow" , border = "black")
```
```{r echo=TRUE, message=TRUE, include=TRUE}
mean(total_steps, na.rm = TRUE)
```

```{r echo=TRUE, message=TRUE, include=TRUE}
median(total_steps, na.rm = TRUE)
```


## What is the average daily activity pattern?
```{r echo = TRUE}
library(ggplot2)
averages <- aggregate(x = list(steps = data$steps), by = list(interval = data$interval), FUN = mean, na.rm = TRUE)
ggplot(data = averages, aes(x = interval, y = steps)) + geom_line(col = "red", lwd = 1) + xlab("5-minute interval") + 
    ylab("average number of steps taken in a day")
```

## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r echo = TRUE}
averages[which.max(averages$steps), ]
```

## Inputing missing values
```{r echo = TRUE}
missing <- is.na(data$steps)
summary(missing)
```



```{r echo = TRUE}
# Replace each missing value with the mean value of its 5-minute interval
fill.value <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps)) 
        filled <- c(steps) else filled <- (averages[averages$interval == interval, "steps"])
    return(filled)
}
filled.data <- data
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)
```
```{r echo = TRUE}
total_steps <- tapply(filled.data$steps, filled.data$date, FUN = sum)
qplot(total_steps, binwidth = 1000, xlab = "total number of steps taken each day", bg = "red")
```

```{r echo = TRUE}
mean(total_steps)

```

```{r echo = TRUE}
median(total_steps)
```


Mean and median values are higher after imputing missing data. The reason is
that in the original data, there are some days with `steps` values `NA` for 
any `interval`. The total number of steps taken in such days are set to 0s by
default. However, after replacing missing `steps` values with the mean `steps`
of associated `interval` value, these 0 values are removed from the histogram
of total number of steps taken each day.

## Are there differences in activity patterns between weekdays and weekends?

```{r echo = TRUE}
weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) 
        return("weekday") else if (day %in% c("Saturday", "Sunday")) 
        return("weekend") else stop("invalid date")
}
filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN = weekday.or.weekend)
```

```{r echo = TRUE}
averages <- aggregate(steps ~ interval + day, data = filled.data, mean)
ggplot(averages, aes(interval, steps)) + geom_line(col = "green" , lwd = 1) + facet_grid(day ~ .) + 
    xlab("5-minute interval") + ylab("Number of steps")
```
