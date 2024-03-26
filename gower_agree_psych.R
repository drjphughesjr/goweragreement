
discrete.dist = function(x, y)
{
    d = as.numeric(x != y)
    d
}

row.dist = function(x, dist)
{
    d = NULL
    n = length(x)
    for (i in 1:(n - 1))
        for (j in (i + 1):n)
            d = c(d, dist(x[i], x[j]))
    d = d[! is.na(d)]
    d
}

delta.est = function(data, dist)
{
    n = nrow(data)
    probs = numeric(n)
    for (i in 1:n)
        probs[i] = 1 - mean(row.dist(data[i, ], dist))
    probs = probs[! is.na(probs)]
    mean(probs)
}

delta.probs = function(data, dist)
{
    n = nrow(data)
    probs = numeric(n)
    for (i in 1:n)
        probs[i] = 1 - mean(row.dist(data[i, ], dist))
    probs = probs[! is.na(probs)]
    probs
}

library(irr)

data(diagnoses)
psych = matrix(0, nrow(diagnoses), ncol(diagnoses))
psych[diagnoses == "1. Depression"] = 1
psych[diagnoses == "2. Personality Disorder"] = 2
psych[diagnoses == "3. Schizophrenia"] = 3
psych[diagnoses == "4. Neurosis"] = 4
psych[diagnoses == "5. Other"] = 5
n = nrow(psych)

delta.est(psych, discrete.dist)

probs = delta.probs(psych, discrete.dist)
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

shapiro.test(mu.bb[1:5000])
qqnorm(scale(mu.bb), pch = 20)
abline(0, 1, col = "blue")

dev.new()
pdf(file = "psych.pdf")
par(mai = c(1, 0.9, 0.1, 0.2))
par(mar = c(6, 7, 1, 0.5))
par(mgp = c(4, 1, 0))
par(cex.lab = 2.5)
par(cex.axis = 1.75)
hist(mu.bb, prob = TRUE, xlab = expression(mu[g]), breaks = 30, main = "")
lines(density(mu.bb), lwd = 3)
curve(dnorm(x, mean(mu.bb), sd(mu.bb)), lwd = 3, lty = 2, add = TRUE)
abline(v = ci, lwd = 3, lty = 2)
dev.off()

library(krippendorffsalpha)

fit = krippendorffs.alpha(psych, level = "nominal", control = list(parallel = FALSE))
summary(fit)

# Krippendorff data

data = matrix(c(1,2,3,3,2,1,4,1,2,NA,NA,NA,
                1,2,3,3,2,2,4,1,2,5,NA,3,
                NA,3,3,3,2,3,4,2,2,5,1,NA,
                1,2,3,3,2,4,4,1,2,5,1,NA), 12, 4)
data = data[-12, ]

discrete.dist = function(x, y)
{
    d = as.numeric(x != y)
    d
}

row.dist = function(x, dist)
{
    d = NULL
    n = length(x)
    for (i in 1:(n - 1))
        for (j in (i + 1):n)
            d = c(d, dist(x[i], x[j]))
    d = d[! is.na(d)]
    d
}

delta.est = function(data, dist)
{
    n = nrow(data)
    probs = numeric(n)
    for (i in 1:n)
        probs[i] = 1 - mean(row.dist(data[i, ], dist))
    probs = probs[! is.na(probs)]
    mean(probs)
}

delta.probs = function(data, dist)
{
    n = nrow(data)
    probs = numeric(n)
    for (i in 1:n)
        probs[i] = 1 - mean(row.dist(data[i, ], dist))
    probs = probs[! is.na(probs)]
    probs
}

delta.est(data, discrete.dist)

n = nrow(data)
probs = delta.probs(data, discrete.dist)
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

fit = krippendorffs.alpha(data, level = "nominal", control = list(parallel = FALSE))
summary(fit)



