#   Function to simulate a binomial tree path
#   Return an array of length n+1 that includes S0 and...
#   ... n random path steps

binPath = function( n,        #  the number of random steps
                    mu,       #  the expected return, in percent/year
                    sig,      #  the vol, in percent/year
                    S0,       #  the starting price
                    T) {       #  simulate up to this time
  
  dt = T/n       
  u  = 1 + sig*sqrt(dt)
  d  = 1 - sig*sqrt(dt)
  pu = ( 1 + ( mu*sqrt(dt)/sig) )/2  # formula from class
  pd = ( 1 - ( mu*sqrt(dt)/sig) )/2
  
  S_path    = 1:(n+1)          # initialize a path of length n+1
  S_path[1] = S0               # the first value is the starting value
  # make the path
  S_Old     = S0
  for ( k in 1:n){
    U = runif(1)    # note, u and U are different
    if ( U < pu ) {
      S_New = u*S_Old        # S -> uS with probability pu
    } else {
      S_New = d*S_Old        # S -> dS with probability pd = 1-pu
    }
    S_path[k+1] = S_New       # record the next step in the path
    S_Old = S_New             # what was new will be old in the next ...
    # ... trip through the loop
  }
  return( S_path )
}