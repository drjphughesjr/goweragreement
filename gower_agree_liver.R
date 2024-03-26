
l1.dist = function(x, y, range)
{
    d = as.numeric(abs(x - y) / range)
    d
}

row.dist = function(x, dist, range)
{
    d = NULL
    n = length(x)
    for (i in 1:(n - 1))
        for (j in (i + 1):n)
            d = c(d, dist(x[i], x[j], range))
    d = d[! is.na(d)]
    d
}

delta.est = function(data, dist, range)
{
    n = nrow(data)
    probs = numeric(n)
    for (i in 1:n)
        probs[i] = 1 - mean(row.dist(data[i, ], dist, range))
    probs = probs[! is.na(probs)]
    mean(probs)
}

delta.probs = function(data, dist, range)
{
    n = nrow(data)
    probs = numeric(n)
    for (i in 1:n)
        probs[i] = 1 - mean(row.dist(data[i, ], dist, range))
    probs = probs[! is.na(probs)]
    probs
}


liver = read.csv("liver.csv", header = TRUE)
n = nrow(liver)

# overall

delta.est(liver, l1.dist, 4)

probs = delta.probs(liver, l1.dist, 4)
lo = mean(probs) - 2 * sd(probs) / sqrt(n)
hi = mean(probs) + 2 * sd(probs) / sqrt(n)
ci.clt = c(lo, hi)
b = 10000
mu.bb = numeric(b)
for (k in 1:b)
{
    u = runif(n - 1)
    g = diff(c(0, sort(u), 1))
    mu.bb[k] = g %*% probs
}
alpha = pnorm(sqrt(n / (n - 1)) * qt(0.025, df = n - 1))
ci = quantile(mu.bb, c(alpha, 1 - alpha))
mean(mu.bb)

dev.new()
hist(mu.bb, prob = TRUE)
lines(density(mu.bb), col = "blue")
abline(v = ci, col = "red")

# intra 1

delta.est(liver[, 1:2], l1.dist, 4)

probs = delta.probs(liver[, 1:2], l1.dist, 4)
lo = mean(probs) - 2 * sd(probs) / sqrt(n)
hi = mean(probs) + 2 * sd(probs) / sqrt(n)
ci.clt = c(lo, hi)
b = 10000
mu.bb = numeric(b)
for (k in 1:b)
{
    u = runif(n - 1)
    g = diff(c(0, sort(u), 1))
    mu.bb[k] = g %*% probs
}
alpha = pnorm(sqrt(n / (n - 1)) * qt(0.025, df = n - 1))
ci = quantile(mu.bb, c(alpha, 1 - alpha))
mean(mu.bb)

dev.new()
hist(mu.bb, prob = TRUE)
lines(density(mu.bb), col = "blue")
abline(v = ci, col = "red")

# intra 2

delta.est(liver[, 3:4], l1.dist, 4)

probs = delta.probs(liver[, 3:4], l1.dist, 4)
lo = mean(probs) - 2 * sd(probs) / sqrt(n)
hi = mean(probs) + 2 * sd(probs) / sqrt(n)
ci.clt = c(lo, hi)
b = 10000
mu.bb = numeric(b)
for (k in 1:b)
{
    u = runif(n - 1)
    g = diff(c(0, sort(u), 1))
    mu.bb[k] = g %*% probs
}
alpha = pnorm(sqrt(n / (n - 1)) * qt(0.025, df = n - 1))
ci = quantile(mu.bb, c(alpha, 1 - alpha))
mean(mu.bb)
median(mu.bb)

dev.new()
hist(mu.bb, prob = TRUE, xlab = expression(mu[g]), ylim = c(0, 70), breaks = 30, main = "")
lines(density(mu.bb), lwd = 3)
abline(v = ci, lwd = 3, lty = 3)

dev.new()
pdf(file = "rad2.pdf")
par(mai = c(1, 0.9, 0.1, 0.2))
par(mar = c(6, 7, 1, 0.5))
par(mgp = c(4, 1, 0))
par(cex.lab = 2.5)
par(cex.axis = 1.75)
hist(mu.bb, prob = TRUE, xlab = expression(mu[g]), ylim = c(0, 70), breaks = 30, main = "")
lines(density(mu.bb), lwd = 3)
abline(v = ci, lwd = 3, lty = 2)
dev.off()

# max metric

l1.dist = function(x, y)
{
  d = as.numeric(abs(x - y))
  d
}

row.dist = function(x, dist, range)
{
  d = NULL
  n = length(x)
  for (i in 1:(n - 1))
    for (j in (i + 1):n)
      d = c(d, dist(x[i], x[j]))
  d = d[! is.na(d)]
  d = max(d) / range
}

delta.est = function(data, dist, range)
{
  n = nrow(data)
  probs = numeric(n)
  for (i in 1:n)
    probs[i] = 1 - row.dist(data[i, ], dist, range)
  probs = probs[! is.na(probs)]
  mean(probs)
}

delta.probs = function(data, dist, range)
{
  n = nrow(data)
  probs = numeric(n)
  for (i in 1:n)
    probs[i] = 1 - row.dist(data[i, ], dist, range)
  probs = probs[! is.na(probs)]
  probs
}

liver = read.csv("liver.csv", header = TRUE)
n = nrow(liver)

# overall

delta.est(liver, l1.dist, 4)

probs = delta.probs(liver, l1.dist, 4)
lo = mean(probs) - 2 * sd(probs) / sqrt(n)
hi = mean(probs) + 2 * sd(probs) / sqrt(n)
ci.clt = c(lo, hi)
b = 10000
mu.bb = numeric(b)
for (k in 1:b)
{
  u = runif(n - 1)
  g = diff(c(0, sort(u), 1))
  mu.bb[k] = g %*% probs
}
alpha = pnorm(sqrt(n / (n - 1)) * qt(0.025, df = n - 1))
ci = quantile(mu.bb, c(alpha, 1 - alpha))
mean(mu.bb)

dev.new()
hist(mu.bb, prob = TRUE, xlab = expression(mu[g]), ylim = c(0, 30), breaks = 30, main = "")
lines(density(mu.bb), lwd = 3)
abline(v = ci, lwd = 3, lty = 3)

dev.new()
pdf(file = "rad_max.pdf")
par(mai = c(1, 0.9, 0.1, 0.2))
par(mar = c(6, 7, 1, 0.5))
par(mgp = c(4, 1, 0))
par(cex.lab = 2.5)
par(cex.axis = 1.75)
hist(mu.bb, prob = TRUE, xlab = expression(mu[g]), breaks = 30, main = "")
lines(density(mu.bb), lwd = 3)
abline(v = ci, lwd = 3, lty = 2)
dev.off()

