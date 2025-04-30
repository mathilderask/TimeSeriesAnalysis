lagdf <- function(x, lag) {
  # Lag x, i.e. delay x lag steps
  if (lag > 0) {
    x[(lag+1):nrow(x), ] <- x[1:(nrow(x)-lag), ]
    x[1:lag, ] <- NA
  }else if(lag < 0) {
    # Lag x "ahead in time"
    x[1:(nrow(x)-abs(lag)), ] <- x[(abs(lag)+1):nrow(x), ]
    x[(nrow(x)-abs(lag)+1):nrow(x), ] <- NA
  }
  return(x)
}

# Test
#lagdf(data.frame(x=1:10,y=10:1), 2)
#lagdf(data.frame(x=1:10,y=10:1), -2)
