
# PLOTTING DATA

### Read training data
#! Perhaps you need to set the working directory!?
#setwd("/home/pbac/g/course02417/2025/assignment1")
D <- read.csv("DST_BIL54.csv")
str(D)

# See the help
#?strftime
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

y = Dtrain$total

N = length(y)

x <- c(rep(0, N))

for (j in 1:N) {
            x[j] = 2018 + (j-1) * 1/12
        }

png(file="1_total_v_time.png")
plot(x, y)
dev.off()

# DESCRIBE THE TIME SERIES
# Overall somewhat linear trend. Seemingly seasonal dip.