x <- readline("Write a sequence of numbers from 0 to 9\n")
x <- as.numeric(strsplit(x, " ")[[1]])
acf(x)