D <- read.csv("DST_BIL54.csv")
str(D)
print(D)
# See the help
# ?strftime
D$time <- as.POSIXct(paste0(D$time,"-01"), "%Y-%m-%d", tz="UTC")
D$time
class(D$time)

## Year to month for each of them
D$year <- 1900 + as.POSIXlt(D$time)$year + as.POSIXlt(D$time)$mon / 12

## Make the output variable a floating point (i.e.\ decimal number)
D$total <- as.numeric(D$total) / 1E6

## Divide intro train and test set
teststart <- as.POSIXct("2024-01-01", tz="UTC")
Dtrain <- D[D$time < teststart, ]
Dtest <- D[D$time >= teststart, ]

# 1.1. Make a time variable, x, such that 2018-Jan has x0 = 2018, 2018-Feb has x1 = 2018 + 1/12,
# 2018-Mar has x2 = 2018 + 2/12 etc. and plot the training data versus x.
print(Dtrain$time)
# x <- Dtrain['year']  # This keeps it as a data frame
x <- Dtrain$year # # Extracts it as a numeric vector

x11()
plot(x,Dtrain$hydrogen)


x11()  # Open a new plot window

# Define x and remove it from the dataframe to plot only numeric columns
x <- Dtrain$year
y_data <- Dtrain[, !names(Dtrain) %in% c("year","time", "total")]  # Remove some of the columns

# Plot all columns against x
matplot(x, y_data, type = "l", lty = 1, col = 1:ncol(y_data), 
        xlab = "Year", ylab = "# Vehicles", main = "Number of Motor Driven Veichles Over Time")

# Add legend using column names
legend("topright", legend = colnames(y_data), col = 1:ncol(y_data), lty = 1, cex = 0.8)

