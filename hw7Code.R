# Parameters --------------------------------------------------------------
n   = 5000        # number of steps in a path
mu  = .1
sig = .3
T   = 1
S0  = 100
dt  = T/n
times = 1:(n+1)
r = .01
K = #TODO

gMean = (mu - (sig ** 2)/2) * dt
gSd   = sig * sqrt(dt)

# Delta Hedging -----------------------------------------------------------

# Path Generation ---------------------------------------------------------
#   Function to simulate a binomial tree path
#   Return an array of length n+1 that includes S0 and...
#   ... n random path steps

binPath = function( n,        #  the number of random steps
                    mu,       #  the expected return, in percent/year
                    sig,      #  the vol, in percent/year
                    S0,       #  the starting price
                    T) {       #  simulate up to this time
  
  moves = rnorm(n, mean = gMean, sd = gSd)
  
  S_path    = 1:(n+1)          # initialize a path of length n+1
  S_path[1] = S0               # the first value is the starting value
  # make the path
  S_Old     = S0
  i = 1
  for ( k in moves){
    i = i++
    mvmt = exp(k)
    s_New = S_Old * mvmt
    S_path[i] = S_New       # record the next step in the path
    S_Old = S_New             # what was new will be old in the next ...
    # ... trip through the loop
  }
  return( S_path )
}

# Hedging -----------------------------------------------------------------

putDelta <- function(S_path,
                  sig,
                  t,
                  K,
                  r, ) {
  for (k in S_path) #TODO
  d = (1/(sig * sqrt(T - t))) * (log(St/K) + (r + ((sig ** 2) / 2)) * (T - t))
  delta = dnorm(d, mean = gMean, sd = gSd)
  return(delta)
}


# Plotting ----------------------------------------------------------------


