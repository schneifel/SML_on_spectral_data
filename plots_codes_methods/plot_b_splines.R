# Load necessary package
library(splines)

# Define the range for x
x <- seq(0, 11, length.out = 200)
# Define the positions and labels for the x-axis
x_positions <- seq(1, 10, by = 1)  # Positions at each integer
x_labels <- paste0("x", x_positions)

# Define multiple sets of knots
knots_1 <- c(1, 2, 3, 6)
knots_2 <- c(5, 6, 7, 8, 9, 10)
knots_2.2 <- c(7, 8, 9, 10)
knots_3 <- c(1, 2, 3, 4)              # For bspline_3 (degree 2)
knots_4 <- c(5, 6, 7, 8, 9, 10)              # Modified knots for bspline_4 (degree 2)
knots_5 <- c(6, 7, 8, 9)
knots_6 <- c(7, 8, 9, 10)

# Generate B-spline bases for each set of knots for degree 1
bspline_1 <- bs(x, knots = knots_1, degree = 1, intercept = TRUE)
bspline_2 <- bs(x, knots = knots_2, degree = 1, intercept = TRUE)
bspline_2.2 <- bs(x, knots = knots_2.2, degree = 1, intercept = TRUE)

# Generate B-spline bases for each set of knots for degree 2
bspline_3 <- bs(x, knots = knots_3, degree = 2, intercept = TRUE)
bspline_4 <- bs(x, knots = knots_4, degree = 2, intercept = TRUE)
bspline_5 <- bs(x, knots = knots_5, degree = 2, intercept = TRUE) 
bspline_6 <- bs(x, knots = knots_6, degree = 2, intercept = TRUE)

par(mfrow = c(2,1), mar = c(3, 1, 2, 1))
# Plot the B-spline basis functions for each set of knots
plot(x, bspline_1[, 3], type = "l", col = "hotpink", lwd = 2,
     xlim = c(0, 11), ylim = c(0, 1),
     main = "Linear B-Splines",
     cex.main = 1.3,
     xaxt = "n", yaxt = "n",
     xlab = "", ylab = "")

axis(1, at = x_positions, labels = x_labels, cex.axis = 1.3)

# Add B-spline basis functions from the second set of knots
lines(x, bspline_2[, 4], col = "hotpink", lwd = 2)
lines(x, bspline_2.2[, 3], col = "hotpink", lwd = 2)
lines(x, bspline_2.2[, 4], col = "hotpink", lwd = 2)

# Add vertical dashed lines for each set of knots
abline(v = knots_1, col = "azure4", lty = 2)
abline(v = knots_2.2, col = "azure4", lty = 2)


# Plot the B-spline basis functions for each set of knots
plot(x, bspline_3[, 4], type = "l", col = "seagreen3", lwd = 2,
     xlim = c(0, 11), ylim = c(0, 1),
     main = "Quadratic B-Splines",
     cex.main = 1.3,
     xaxt = "n", yaxt = "n",
     xlab = "", ylab = "")

axis(1, at = x_positions, labels = x_labels, cex.axis = 1.3)


# Add B-spline basis functions from the second set of knots
lines(x, bspline_4[, 4], col = "seagreen3", lwd = 2)
lines(x, bspline_5[, 4], col = "seagreen3", lwd = 2)
lines(x, bspline_6[, 4], col = "seagreen3", lwd = 2)

# Add vertical dashed lines for each set of knots
abline(v = knots_3, col = "azure4", lty = 2)
abline(v = knots_4, col = "azure4", lty = 2)



