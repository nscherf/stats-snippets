library(pwr)
# for documentation and examples see https://www.statmethods.net/stats/power.html

compute_effect_size <- function(mean_1, mean_2, sd_1, sd_2, n_1, n_2) {
  pooled_sd <- sqrt(((n_1-1)*(sd_1)^2) + ((n_2-1)*(sd_2)^2))/(n_1+n_2-2)
  d <- (mean_1 - mean_2)/pooled_sd
}

sim_effect_size <- function(mean_1, mean_2, sd_1, sd_2, n_1, n_2) {
  group_1 <- rnorm(n_1, mean=mean_1, sd=sd_1)
  group_2 <- rnorm(n_2, mean=mean_2, sd=sd_2)
  pooled_sd <- sqrt(((n_1-1)*(sd(group_1)^2) + (n_2-1)*(sd(group_2)^2))/(n_1+n_2-2))
  d <- (mean(group_1) - mean(group_2))/pooled_sd
}

# let's test with some more realistic estimates
d <- compute_effect_size(0.163, 0.187, 0.0084, 0.0084, 100, 100)
d <- compute_effect_size(0.163, 0.187, 0.05, 0.05, 20, 20)
print(d)

d<-(0.163-0.187)/0.012

pwr.t.test(d=d, sig.level = 0.05, power = 0.8, type = "two.sample")
