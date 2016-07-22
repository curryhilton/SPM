
mu <- 10
sd <- 1
L <- 3
n <- 5
A <- L/(sqrt(n))
cl <- mu
uclx <- cl + A*sd
lclx <- max(cl - A*sd,0)

x <- rnorm(50, mean = 10, sd = 1)
m <- seq(1:length(x))

LCL <- paste("LCL =", round(lclx, 2))
CL <- paste("Center Line", round(cl, 2))
UCL <- paste("UCL =", round(uclx, 2))
sub <- paste("Subgroup Size =", length(m))
stdev <- paste("Standard Deviation =", sd)
count <- paste("Violations =", length(which(x > uclx | x < lclx)))

par(bg="lightsteelblue2", mar = c(10, 3, 2, 3))
plot(m, x, xlab = "Subgroups", ylab = "X-bar", ylim = c(lclx - sd, uclx + sd))
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "white")
abline(h = cl, lwd = 2)
abline(h = uclx, col = "red", lty = 2)
abline(h = lclx, col = "red", lty = 2)
points(x, pch = 20, type = "b", col = ifelse(x > uclx | x < lclx, "red", "black"))
mtext(LCL, at = m[1] + 1, side = 1, line = 5, adj = 0, font = 2)
mtext(CL, at = m[1] + 1, side = 1, line = 6, adj = 0, font = 2)
mtext(UCL, at = m[1] + 1, side = 1, line = 7, adj = 0, font = 2)
mtext(stdev, at = m[length(m)] - 1, side = 1, line = 5, adj = 1, font = 2)
mtext(sub, at = m[length(m)] - 1, side = 1, line = 6, adj = 1, font = 2)
mtext(count, at = m[length(m)] - 1, side = 1, line = 7, adj = 1, font = 2)

