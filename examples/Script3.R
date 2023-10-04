N <- 50
p <- 0.45
con <- NULL
for(i in 1:10) {con <- c(con, percolate(N, p, draw=F))}
