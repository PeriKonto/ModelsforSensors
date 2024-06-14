# number of samples to generate
n <- 500

# regressors with intercept
X <- cbind(rep(1, n), rnorm(n))

# true state
beta <- c(0.1, 1)

# observations wit normal error
y <- exp(-(X %*% beta)) + rnorm(n, sd=1)

# plot data, non linear!
plot( 1 + X[,2], y)

# state transition matrix is the identity:
stateTransition <- diag(c(1,1))

# 2 states
n_state <- ncol(stateTransition)

# init variable to store the states in loop
state <- matrix(0, ncol=n_state, nrow=n)

# process noise is the identity
#processNoise <- diag(c(1,1))
processNoise <- diag(c(0, 0))
# measurement noise is 1
measurementNoise <- 1
S <- diag(rep(1, n_state))
m <- 0
# iterate
for(i in 2:n){

  # predict
  m_ <- stateTransition %*% state[i-1, ]
  S_ <- stateTransition %*% S %*% t(stateTransition) + processNoise

  # update equations, drop = false preseves the structure of the matrix, otherwise R would
  # turn it into a vector
  v <- y[i] - exp(-(X[i,,drop=FALSE] %*% m_))

  # Jakobian of the model
  H <- -X[i,,drop=FALSE] * exp(-(X[i,,drop=FALSE] %*% m_))[1]

  # computations according to model
  S <- H  %*% S_ %*% t(H) + measurementNoise
  K <- S_ %*% t(H) %*% solve(S)
  m <- m_ + K %*% v
  #S <- S_ - K %*% S %*% t(K)
  S <- S_ - K %*% H %*% S_
  # save state
  state[i,] <- m
}

# generate predictions according to estimated state
pred <- numeric(n)
for(i in 1:n){
  pred[i] <- exp(-(X[i,]%*%state[i,]))
}

# plot results
plot(y)
lines(pred, col='red')

plot( 1 + X[,2], pred, col='red')
