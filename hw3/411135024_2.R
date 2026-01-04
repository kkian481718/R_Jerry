counts <- c(30, 42, 55, 43, 30)
breaks_seq <- seq(30, 180, by = 30)
midpoints <- seq(45, 165, by = 30)
data_approx <- rep(midpoints, counts)

par(mfrow = c(2, 1))

hist(data_approx, breaks = breaks_seq, right = FALSE, main = "Histogram and Frequency Polygon", xlab = "Minutes", ylab = "Frequency", col = "lightblue", ylim = c(0, 60))
lines(c(15, midpoints, 195), c(0, counts, 0), type = "o", col = "red", lwd = 2)

cum_counts <- c(0, cumsum(counts))
plot(breaks_seq, cum_counts, type = "o", main = "Cumulative Frequency Polygon", xlab = "Minutes", ylab = "Cumulative Frequency", col = "blue", lwd = 2, xaxt = "n")
axis(1, at = breaks_seq)
grid()