# Imports -----------------------------------------------------------------
library(tibble)
library(tidyr)
library(dplyr)
library(magrittr)
library(purrr)

# Parameters --------------------------------------------------------------
mu  = .1
sig = .3
T   = .5
S0  = 100
times = 1:(n+1)
r = .02
K = S0
spr = 1

gMean = NULL
gSd   = NULL
set.seed(5)

# Delta Hedging -----------------------------------------------------------
	# Path Generation ---------------------------------------------------------
		#   Function to simulate a binomial tree path
		#   Return an array of length n+1 that includes S0 and...
		#   ... n random path steps
		
		binPath = function(n        #  the number of random steps
											 ) {
		  
		  moves = rnorm(n, mean = gMean, sd = gSd)
		  
		  S_path    = 1:(n+1)          # initialize a path of length n+1
		  S_path[1] = S0               # the first value is the starting value
		  # make the path
		  S_Old     = S0
		  i = 1
		  for ( k in moves){
		    i = i + 1
		    mvmt = exp(k)
		    S_New = S_Old * mvmt
		    S_path[i] = S_New       # record the next step in the path
		    S_Old = S_New             # what was new will be old in the next ...
		    # ... trip through the loop
		  }
		  
		  return( S_path )
		}
		
	# Preliminary -------------------------------------------------------------
		
	
	# Hedging -----------------------------------------------------------------
		putBlSch <- function(inVec #Input vector with 1st elem = t, 2nd = St
												 ) {
			t <- inVec[[1]]
			St <- inVec[[2]]
			price_t = NULL
			if (t < T) {
				d1 = (1/(sig * sqrt(T - t))) * (log(St/K) + (r + ((sig ** 2) / 2)) * (T - t))
				d2 = d1 - (sig * sqrt(T - t))
				
				Nd1 = dnorm( -d1, mean = gMean, sd = gSd)
				Nd2 = dnorm( -d2, mean = gMean, sd = gSd)
				
				price_t = Nd2 * (K * exp( (-r * (T - t)) )) - Nd1 * St
			}
			else {
				price_t = max((St - K), 0)
			}

			return(price_t)
		}
		
		putDelta <- function(inVec #Same procedure as previous function
												 ) {
			t <- inVec[[1]]
			St <- inVec[[2]]
		  d = (1/(sig * sqrt(T - t))) * (log(St/K) + (r + ((sig ** 2) / 2)) * (T - t))
		  delta_t = dnorm(d, mean = gMean, sd = gSd) - 1 #For puts
		  return(delta_t)
		}
		
		putReplicator <- function(n) {
			tVals   =  1:(n + 1)
			dt = T/n
			for (i in (n):0) {
				if (i == n) tVals[1] = 0
				else tVals[n - i + 1] = T - (i * dt)
			}
			
			assign('gMean', (mu - (sig ** 2)/2) * dt, envir = .GlobalEnv)
			assign('gSd'  , sig * sqrt(dt), envir = .GlobalEnv)
			
			preValues  <- tibble(t = tVals, St = binPath(n))
			values <- preValues %>% rowwise %>% do( X = as_data_frame(.)) %>% ungroup
			values <-  mutate(values, 
											 price_t  = map(X, putBlSch),
											 delta_t  = map(X, putDelta)
											 ) %>% unnest
			values <- mutate(values,
											 dDelta_t = delta_t - lag(delta_t, default = first(delta_t)))
			
			C0 = as.numeric(values[1, 'price_t']) - (as.numeric(values[1, 'delta_t']) * as.numeric(values[1, 'St']))
			values = mutate(values,
											C_t = C0 * (t + 1))
			values = mutate(values,
											C_t     = ((exp(r * t) * lag(C_t, default = first(C_t))) - (dDelta_t * St)),
											folio_t = C_t + (delta_t * St),
											Q       = price_t - folio_t)
			return(values)
		}
		
		putQGetter <- function() {
			nVals  = c(10, 50, 100, 200, 300, 400)
			
			names  = c(toString(nVals[1]), toString(nVals[2]), toString(nVals[3]), 
								 toString(nVals[4]), toString(nVals[5]), toString(nVals[6]))
			rList = vector('list', length(names))
			names(rList) <- names
			
			Qs = 1:100
			for (i in nVals) {
				print(i)
				for (j in 1:100) {
					values = putReplicator(i)
					if (values[i + 1, 'price_t'] != 0)	Qs[j] = as.numeric(values[i + 1, 'Q'])
					else Qs[j] = NA
				}
				rList[[toString(i)]] = Qs
			}
			return(rList)
		}
	
	# Plotting ----------------------------------------------------------------
	dh = putQGetter()
		
	num = 1
	hist(dh[[num]],
			 probability = TRUE,
			 breaks = 50,
			 plot = TRUE,
			 xlab = "Q Values",
			 main = paste0("Replication Error with n = ", names(dh[num]), " periods"))

	

# Bid-Ask Spread ----------------------------------------------------------

	putReplicatorSpread <- function(n) {
		tVals   =  1:(n + 1)
		dt = T/n
		for (i in (n):0) {
			if (i == n) tVals[1] = 0
			else tVals[n - i + 1] = T - (i * dt)
		}
		
		assign('gMean', (mu - (sig ** 2)/2) * dt, envir = .GlobalEnv)
		assign('gSd'  , sig * sqrt(dt), envir = .GlobalEnv)
		
		preValues  <- tibble(t = tVals, St = binPath(n))
		values <- preValues %>% rowwise %>% do( X = as_data_frame(.)) %>% ungroup
		values <-  mutate(values, 
											price_t  = map(X, putBlSch),
											delta_t  = map(X, putDelta)
		) %>% unnest
		values <- mutate(values,
										 dDelta_t = delta_t - lag(delta_t, default = first(delta_t)))
		
		C0 = as.numeric(values[1, 'price_t']) - (as.numeric(values[1, 'delta_t']) * as.numeric(values[1, 'St']))
		values = mutate(values,
										C_t = C0 * (t + 1))
		values = mutate(values,
										C_t     = 
											if_else(dDelta_t >=0, 
														 ((exp(r * t) * lag(C_t, default = first(C_t))) - (dDelta_t * (St + (.5 * spr)))),
														 ((exp(r * t) * lag(C_t, default = first(C_t))) - (dDelta_t * (St - (.5 * spr))))),
										folio_t = C_t + (delta_t * St),
										Q       = price_t - folio_t)
		return(values)
	}
	
	putQGetterSpread <- function() {
		nVals  = c(10, 50, 100, 200, 300, 400)
		
		names  = c(toString(nVals[1]), toString(nVals[2]), toString(nVals[3]), 
							 toString(nVals[4]), toString(nVals[5]), toString(nVals[6]))
		rList = vector('list', length(names))
		names(rList) <- names
		
		Qs = 1:100
		for (i in nVals) {
			print(i)
			for (j in 1:100) {
				values = putReplicatorSpread(i)
				if (values[i + 1, 'price_t'] != 0)	Qs[j] = as.numeric(values[i + 1, 'Q'])
				else Qs[j] = NA
			}
			rList[[toString(i)]] = Qs
		}
		return(rList)
	}
	
	dhSpread = putQGetterSpread()
	
	num = 6
	hist(dhSpread[[num]],
			 probability = TRUE,
			 breaks = 50,
			 plot = TRUE,
			 xlab = "Q Values With Spread",
			 main = paste0("Replication Error with n = ", names(dhSpread[num]), " periods"))
	

# No-Transaction Region ---------------------------------------------------

	
	putReplicatorNTZ <- function(n) {
		tVals   =  1:(n + 1)
		dt = T/n
		for (i in (n):0) {
			if (i == n) tVals[1] = 0
			else tVals[n - i + 1] = T - (i * dt)
		}
		
		assign('gMean', (mu - (sig ** 2)/2) * dt, envir = .GlobalEnv)
		assign('gSd'  , sig * sqrt(dt), envir = .GlobalEnv)
		
		h <- .2
		
		preValues  <- tibble(t = tVals, St = binPath(n))
		values <- preValues %>% rowwise %>% do( X = as_data_frame(.)) %>% ungroup
		values <-  mutate(values, 
											price_t  = map(X, putBlSch),
											delta_t  = map(X, putDelta)
		) %>% unnest
		values <- mutate(values,
										 dDelta_t = delta_t - lag(delta_t, default = first(delta_t)))
		
		C0 = as.numeric(values[1, 'price_t']) - (as.numeric(values[1, 'delta_t']) * as.numeric(values[1, 'St']))
		values = mutate(values,
										C_t = C0 * (t + 1))
		values = mutate(values,
										C_t     = 
											if_else(abs(dDelta_t) > h, 
															if_else(dDelta_t >=0, 
																		((exp(r * t) * lag(C_t, default = first(C_t))) - (dDelta_t * (St + (.5 * spr)))),
																		((exp(r * t) * lag(C_t, default = first(C_t))) - (dDelta_t * (St - (.5 * spr))))),
															lag(C_t, default = first(C_t))),
										folio_t = C_t + (delta_t * St),
										Q       = price_t - folio_t)
		return(values)
	}
	
	putQGetterNTZ <- function() {
		nVals  = c(10, 50, 100, 200, 300, 400)
		
		names  = c(toString(nVals[1]), toString(nVals[2]), toString(nVals[3]), 
							 toString(nVals[4]), toString(nVals[5]), toString(nVals[6]))
		rList = vector('list', length(names))
		names(rList) <- names
		
		Qs = 1:100
		for (i in nVals) {
			print(i)
			for (j in 1:100) {
				values = putReplicatorSpread(i)
				if (values[i + 1, 'price_t'] != 0)	Qs[j] = as.numeric(values[i + 1, 'Q'])
				else Qs[j] = NA
			}
			rList[[toString(i)]] = Qs
		}
		return(rList)
	}
	
	dhNTZ = putQGetterSpread()
	
	num = 6
	hist(dhNTZ[[num]],
			 probability = TRUE,
			 breaks = 50,
			 plot = TRUE,
			 xlab = "Q Values With Spread, h = .2",
			 main = paste0("Replication Error with n = ", names(dhNTZ[num]), " periods"))
	
	
	