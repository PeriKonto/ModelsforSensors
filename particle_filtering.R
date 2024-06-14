# A toy example of particle filtering for sensor data

# initial value for x:
x[1] ~ dnorm(x_init, sx_init)

for (t in 2:T) {
  
  # hidden state: models implements constant rate of change
  x[t] ~ dnorm[x[t-1], sx]
  
  # observation:
  y[t] ~ dnorm(x[t], sy)

}


library(dplyr)
library(ggplot2)
cb_palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                "#0072B2", "#D55E00", "#CC79A7", "grey80")
ggplot2::theme_set(theme_bw())

set.seed(456)

# start with prior for x

T <-50
x_true <- rep(NA, T)
obs <- rep(NA, T)

sx <- 2.8
sy <- 3.2

x_true[1] <- rnorm(1, 0, 1)
obs[1] <- rnorm(1, x_true[1], sy)

for (t in seq(2, T)) {
  x_true[t] <- rnorm(1, x_true[t-1], sx)
  obs[t] <- rnorm(1, x_true[t], sy)
}
#
T <- length(obs)
N <- 100

# create x and weight matrices
x <- matrix(nrow =  N, ncol = T)
weights <- matrix(nrow =  N, ncol = T)
# intial (at t=1):
# draw X from prior distribution
x[, 1] <- rnorm(N, 0, sx)
# calculate weights, i.e. probability of evidence given sample from X
weights[, 1] <- dnorm(obs[1], x[, 1], sy)
# normalise weights 
weights[, 1] <- weights[, 1]/sum(weights[, 1])

# weighted resampling with replacement. This ensures that X will converge to the true distribution
x[, 1] <- sample(x[, 1], replace = TRUE, size = N, prob = weights[, 1]) 

for (t in seq(2, T)) {
  # predict x_{t} from previous time step x_{t-1}
  # based on process (transition) model
  x[, t] <- rnorm(N, x[, t-1], sx)
  # calculate  and normalise weights
  weights[, t] <- dnorm(obs[t], x[, t], sy)
  weights[, t] <- weights[, t]/sum(weights[, t])
  # weighted resampling with replacement
  x[, t] <- sample(x[, t], replace = TRUE, size = N, prob = weights[, t]) 
}

x_means <- apply(x, 2, mean)
x_quantiles <- apply(x, 2, function(x) quantile(x, probs = c(0.025, 0.975)))
df <- data_frame(t = seq(1, T),
                 mean = x_means,
                 lower = x_quantiles[1, ],
                 upper = x_quantiles[2, ],
                 x_true = x_true,
                 observations = obs)


plot_filtering_estimates <- function(df) {
  p <- ggplot(data = df, aes(x = t)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3,
                fill = cb_palette[length(cb_palette)]) +
    geom_line(aes(y = x_true), colour = cb_palette[7], alpha = 0.9,
              linetype = "dotdash", size = 1.2) +
    geom_line(aes(y = mean), colour = cb_palette[6], size = 1.4) +
    geom_point(aes(y = observations), colour = cb_palette[1],
               size = 3, shape = 15, alpha = 0.6) +
    geom_line(aes(y = observations), colour = cb_palette[1], size = 1.0,
              alpha = 0.2) +
    ylab(expression(paste("Latent state: ", X))) + xlab("Time")
  print(p)
}

plot_filtering_estimates(df)