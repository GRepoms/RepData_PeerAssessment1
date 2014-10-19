# Reproducible Research: Peer Assessment 1
 

##Loading and preprocessing the data

######Read acitvity data  
 
dataset <- read.csv("activity.csv")  
head(dataset)      
colnames(dataset)  
data$month <- as.numeric(format(data$date, "%m"))  
exclude_nas <- na.omit(dataset)  
rownames(exclude_nas) <- 1:nrow(exclude_nas)  
head(exclude_nas)  
dim(exclude_nas)  

##What is mean total number of steps taken per day?
 
total_steps <- aggregate(exclude_nas$steps, list(Date = exclude_nas$date), FUN = "sum")$x  
 
mean(total_steps)
 
median(total_steps)
 
library(ggplot2)  
qplot(total_steps, binwidth=1000,xlab="total number of steps taken each day")  
 
##What is the average daily activity pattern?
 
averages <- aggregate(x=list(steps=exclude_nas$steps), by=list(interval=exclude_nas$interval),  
                      FUN=mean)  
ggplot(data=averages, aes(x=interval, y=steps)) +  
  geom_line() +  
  xlab("5-minute interval") +  
  ylab("average number of steps taken")  
 


##Imputing missing values
 
sum(is.na(dataset))
 
######Replace each missing value with the mean value of its 5-minute interval
 
fill_value <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps))
    filled <- c(steps)
  else
    filled <- (averages[averages$interval==interval, "steps"])
  return(filled)
}
filled_data <- dataset  

filled_data$steps <- mapply(fill_value, filled_data$steps, filled_data$interval)  
 
######Now, using the filled data set,make a histogram of the total number of steps taken each day and calculate the mean ######and   median total number of steps.
 
total_steps <- tapply(filled_data$steps, filled_data$date, FUN=sum)
qplot(total_steps, binwidth=1000, xlab="total number of steps taken each day")  
mean(total_steps)
median(total_steps)
 
##Are there differences in activity patterns between weekdays and weekends?
 
head(filled_data)  
weekday.or.weekend <- function(date) {  
  day <- weekdays(date)  
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))  
    return("weekday")  
  else if (day %in% c("Saturday", "Sunday"))  
    return("weekend")  
  else  
    stop("invalid date")  
}
filled_data$date <- as.Date(filled_data$date)  
filled_data$day <- sapply(filled_data$date, FUN=weekday.or.weekend)  

averages <- aggregate(steps ~ interval + day, data=filled_data, mean)  
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +  
  xlab("5-minute interval") + ylab("Number of steps")  
  
 
