CallOption_recursive_function <- function(t,N,S1,p,a,b,r,k,j){
  if(j != (N+1)){
    S_a = S1
    S_b = S1
    S_a[j] = (1+a)
    S_b[j] = (1+b)
    return (CallOption_recursive_function(t,N,S_a,p,a,b,r,k,(j+1)) + CallOption_recursive_function(t,N,S_b,p,a,b,r,k,(j+1)))
  } else {
    occurrencesof_b = sum(S1[(t+1):N] == (1+b))
    for (i in t:(N-1)){
      S1[i+1] = S1[i]*S1[i+1]
    }
    # Now we have S1=(S_0,...,S_N) linked to a generic string (a,a,b,b,a,b,...) and we apply f((S_0,...,S_N))
    return ((p^(occurrencesof_b))*((1-p)^(N-t-occurrencesof_b))*max((sum(S1)/N - k),0))
  }
}


PutOption_recursive_function <- function(t,N,S1,p,a,b,r,k,j){
  if(j != (N+1)){
    S_a = S1
    S_b = S1
    S_a[j] = 1+a
    S_b[j] = 1+b
    return (PutOption_recursive_function(t,N,S_a,p,a,b,r,k,(j+1)) + PutOption_recursive_function(t,N,S_b,p,a,b,r,k,(j+1)))
  } else {
    occurrencesof_b = sum(S1[(t+1):N] == (1+b))
    for (i in t:(N-1)){
      S1[i+1] = S1[i]*S1[i+1]
    }
    # Now we have S1(S_0,...,S_N) linked to a generic string (a,a,b,b,a,b,...) and we apply f((S_0,...,S_N))
    return ((p^(occurrencesof_b))*((1-p)^(N-t-occurrencesof_b))*max((k - sum(S1)/(N)),0))
  }
}

ECallOption_pricingfunction_det <- function(t,N,S,p,a,b,r,k){
  #' ECallOption_pricingfunction_det
  #' Give the price of an Asian Call Option - Deterministic Formula p. 46
  #' @param t The time we want the of the option
  #' @param N Instants of time the strategy lasts
  #' @param S S=(S_0,...,S_t) vector of the prices at time t
  #' @param p Up probability
  #' @param a Down Interest
  #' @param b Up interest
  #' @param r Risk-free asset interest
  #' @param k Strike price
  #'
  #' @return The price of the Asian Call Option
  #' @export
  j = t + 1
  S1 = c(S,zeros((N-t),1))
  E = CallOption_recursive_function(t,N,S1,p,a,b,r,k,j)
  price = (E)/((1+r)^(N-t))
}

EPutOption_pricingfunction_det <- function(t,N,S,p,a,b,r,k){
  #' EPutOption_pricingfunction_det
  #' Give the price of an Asian Put Option - Deterministic Formula p. 46
  #' @param t The time we want the of the option
  #' @param N Instants of time the strategy lasts
  #' @param S S=(S_0,...,S_t) vector of the prices at time t
  #' @param p Up probability
  #' @param a Down Interest
  #' @param b Up interest
  #' @param r Risk-free asset interest
  #' @param k Strike price
  #'
  #' @return The price of the Asian Put Option
  #' @export
  j = t + 1
  S1 = c(S,zeros((N-t),1))
  E = PutOption_recursive_function(t,N,S1,p,a,b,r,k,j)
  price = (E)/((1+r)^(N-t))
}

ECallOption_hedgingfunction_det <- function(t,N,S,p,a,b,pi_0,r,k){
  #' ECallOption_hedgingfunction_det
  #' Calculate the portfolio allocation hedging the Asian Call Option - Formula p. 47 Proposition 3.2 - Deterministic
  #' @param t The time we want the of the option
  #' @param N Instants of time the strategy lasts
  #' @param S S = (S_0,...,S_(t-1))
  #' @param p Up probability
  #' @param a Down Interest
  #' @param b Up interest
  #' @param pi_0 Price of Risk-less assset at time t=0
  #' @param r Risk-free asset interest
  #' @param k Strike price
  #'
  #' @return Eps and Eta rispectively allocation of Risk-asset and Free-risk asset
  #' @export
  chi_1 = (ECallOption_pricingfunction_det(t,N,c(S,(1+b)*S[t-1]),p,a,b,r,k) - ECallOption_pricingfunction_det(t,N,c(S,(1+a)*S[t-1]),p,a,b,r,k))
  eps = chi_1/(S[t-1]*(b-a))
  chi_2 = (ECallOption_pricingfunction_det((t-1),N,S,p,a,b,r,k)-eps*S[t-1])
  eta = chi_2/(pi_0*((1+r)^(t-1)))
  return(c(eta,eps))
}

EPutOption_hedgingfunction_det <- function(t,N,S,p,a,b,pi_0,r,k){
  #' EPutOption_hedgingfunction_det
  #' Calculate the portfolio allocation hedging the Asian Put Option - Formula p. 47 Proposition 3.2 - Deterministic
  #' @param t The time we want the of the option
  #' @param N Instants of time the strategy lasts
  #' @param S S = (S_0,...,S_(t-1))
  #' @param p Up probability
  #' @param a Down Interest
  #' @param b Up interest
  #' @param pi_0 Price of Risk-less assset at time t=0
  #' @param r Risk-free asset interest
  #' @param k Strike price
  #'
  #' @return Eps and Eta rispectively allocation of Risk-asset and Free-risk asset
  #' @export
  chi_1 = (EPutOption_pricingfunction_det(t,N,c(S,(1+b)*S[t-1]),p,a,b,r,k) - EPutOption_pricingfunction_det(t,N,c(S,(1+a)*S[t-1]),p,a,b,r,k))
  eps = chi_1/(S[t-1]*(b-a))
  chi_2 = (EPutOption_pricingfunction_det((t-1),N,S,p,a,b,r,k)-eps*S[t-1])
  eta = chi_2/(pi_0*((1+r)^(t-1)))
  return(c(eta,eps))
}
