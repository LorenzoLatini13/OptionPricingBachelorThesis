VCallOption_pricingfunction_rand <- function(t,N,S,p,a,b,r,k,M){
  #' Title  VCAllOption_pricingfunction_rand
  #' Give the price of an European Call Option - Montecarlo Formula p. 46
  #' @param t The time we want the of the option
  #' @param N Instants of time the strategy lasts
  #' @param S S_t, price of the risk asset at time t
  #' @param p Up probability
  #' @param a Down Interest
  #' @param b Up interest
  #' @param r Risk-free asset interest
  #' @param k Strike price
  #' @param M Number of independent variables in Montecarlo
  #'
  #' @return The price of the European Call Option
  #' @export
  recursive_sum = 0
  for (j in 1:M) {
    binomial_vector = rbinom((N-t),1,p)
    random_string = (1+b)*binomial_vector + (1+a)*(ones((N-t),1) - binomial_vector)
    recursive_sum = recursive_sum + max((S*prod(random_string) - k),0)
  }
  E = recursive_sum/M
  price = E/((1+r)^(N-t))
}

VPutOption_pricingfunction_rand <- function(t,N,S,p,a,b,r,k,M){
  #' Title  VPutOption_pricingfunction_rand
  #' Give the price of an European Put Option - Montecarlo Formula p. 46
  #' @param t The time we want the of the option
  #' @param N Instants of time the strategy lasts
  #' @param S S_t, price of the risk asset at time t
  #' @param p Up probability
  #' @param a Down Interest
  #' @param b Up interest
  #' @param r Risk-free asset interest
  #' @param k Strike price
  #' @param M Number of independent variables in Montecarlo
  #'
  #' @return The price of the European Put Option
  #' @export
  recursive_sum = 0
  for (j in 1:M) {
    binomial_vector = rbinom((N-t),1,p)
    random_string = (1+b)*binomial_vector + (1+a)*(ones((N-t),1) - binomial_vector)
    recursive_sum = recursive_sum + max((k - S*prod(random_string)),0)
  }
  E = recursive_sum/M
  price = E/((1+r)^(N-t))
}

VCallOption_hedgingfunction_rand <- function(t,N,S,p,a,b,r,k,pi_0,M){
  #' VCallOption_hedgingfunction_rand
  #' Calculate the portfolio allocation hedging the European Call Option - Formula p. 47 Proposition 3.2 - Montecarlo
  #' @param t The time we want the of the option
  #' @param N Instants of time the strategy lasts
  #' @param S S = S_(t-1)
  #' @param p Up probability
  #' @param a Down Interest
  #' @param b Up interest
  #' @param r Risk-free asset interest
  #' @param k Strike price
  #' @param pi_0 Price of Risk-less assset at time t=0
  #' @param M Number of independent variables in Montecarlo
  #'
  #' @return Eps and Eta rispectively allocation of Risk-asset and Free-risk asset
  #' @export
  chi_1 = VCallOption_pricingfunction_rand(t,N,(1+b)*S,p,a,b,r,k,M) - VCallOption_pricingfunction_rand(t,N,(1+a)*S,p,a,b,r,k,M)
  eps = chi_1/(S*(b-a))
  chi_2 = VCallOption_pricingfunction_rand((t-1),N,S,p,a,b,r,k,M)-eps*S
  eta = chi_2/(pi_0*((1+r)^(t-1)))
  return(c(eta,eps))
}

VPutOption_hedgingfunction_rand <- function(t,N,S,p,a,b,r,k,pi_0,M){
  #' VPutOption_hedgingfunction_rand
  #' Calculate the portfolio allocation hedging the European Put Option - Formula p. 47 Proposition 3.2 - Montecarlo
  #' @param t The time we want the of the option
  #' @param N Instants of time the strategy lasts
  #' @param S S = S_(t-1)
  #' @param p Up probability
  #' @param a Down Interest
  #' @param b Up interest
  #' @param r Risk-free asset interest
  #' @param k Strike price
  #' @param pi_0 Price of Risk-less assset at time t=0
  #' @param M Number of independent variables in Montecarlo
  #'
  #' @return Eps and Eta rispectively allocation of Risk-asset and Free-risk asset
  #' @export
  chi_1 = (VPutOption_pricingfunction_rand(t,N,(1+b)*S,p,a,b,r,k,M) - VPutOption_pricingfunction_rand(t,N,(1+a)*S,p,a,b,r,k,M))
  eps = chi_1/(S*(b-a))
  chi_2 = VPutOption_pricingfunction_rand((t-1),N,S,p,a,b,r,k,M)-eps*S
  eta = chi_2/(pi_0*((1+r)^(t-1)))
  return(c(eta,eps))
}
