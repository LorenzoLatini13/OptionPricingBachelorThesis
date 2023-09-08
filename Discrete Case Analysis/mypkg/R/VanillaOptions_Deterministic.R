VCallOption_pricingfunction_det <- function(t,N,S,p,a,b,r,k){
  #' VPutOption_pricingfunction_det
  #' Give the price of an European Call Option - Deterministic Formula p. 46
  #' @param t The time we want the of the option
  #' @param N Instants of time the strategy lasts
  #' @param S S_t, price of the risk asset at time t
  #' @param p Up probability
  #' @param a Down Interest
  #' @param b Up interest
  #' @param r Risk-free asset interest
  #' @param k Strike price
  #'
  #' @return The price of the European Call Option
  #' @export
  recursive_sum = 0
   for(j in 0:(N-t)){
     recursive_sum = recursive_sum + choose((N-t),j)*(p^j)*((1-p)^(N-t-j))*max((S*((1+b)^j)*((1+a)^(N-t-j))-k),0)
   }
   price_t = (recursive_sum)/((1+r)^(N-t))
}

VPutOption_pricingfunction_det <- function(t,N,S,p,a,b,r,k){
  #' VCallOption_pricingfunction_det
  #' Give the price of an European Put Option - Deterministic Formula p. 46
  #' @param t The time we want the of the option
  #' @param N Instants of time the strategy lasts
  #' @param S S_t, price of the risk asset at time t
  #' @param p Up probability
  #' @param a Down Interest
  #' @param b Up interest
  #' @param r Risk-free asset interest
  #' @param k Strike price
  #'
  #' @return The price of the European Put Option
  #' @export
  recursive_sum = 0
  for(j in 0:(N-t)){
    recursive_sum = recursive_sum + choose((N-t),j)*(p^j)*((1-p)^(N-t-j))*max((k - S*((1+b)^j)*((1+a)^(N-t-j))),0)
  }
  price_t = (recursive_sum)/((1+r)^(N-t))
}


VCallOption_hedgingfunction_det <- function(t,N,S,p,a,b,r,k,pi_0){
  #' VCallOption_hedgingfunction_det
  #' Calculate the portfolio allocation hedging the European Call Option - Formula p. 47 Proposition 3.2
  #' @param t The time we want the of the option
  #' @param N Instants of time the strategy lasts
  #' @param S S = S_(t-1)
  #' @param p Up probability
  #' @param a Down Interest
  #' @param b Up interest
  #' @param r Risk-free asset interest
  #' @param k Strike price
  #' @param pi_0 Price of Risk-less assset at time t=0
  #'
  #' @return Eps and Eta rispectively allocation of Risk-asset and Free-risk asset
  #' @export
  chi_1 = VCallOption_pricingfunction_det(t,N,(1+b)*S,p,a,b,r,k) - VCallOption_pricingfunction_det(t,N,(1+a)*S,p,a,b,r,k)
  eps = chi_1/(S*(b-a))
  chi_2 = VCallOption_pricingfunction_det((t-1),N,S,p,a,b,r,k)-eps*S
  eta = chi_2/(pi_0*((1+r)^(t-1)))
  return(c(eta,eps))
}

VPutOption_hedgingfunction_det <- function(t,N,S,p,a,b,r,k,pi_0){
  #' VPutOption_hedgingfunction_det
  #' Calculate the portfolio allocation hedging the European Put Option - Formula p. 47 Proposition 3.2
  #' @param t The time we want the of the option
  #' @param N Instants of time the strategy lasts
  #' @param S S = S_(t-1)
  #' @param p Up probability
  #' @param a Down Interest
  #' @param b Up interest
  #' @param r Risk-free asset interest
  #' @param k Strike price
  #' @param pi_0 Price of Risk-less asset at time t=0
  #'
  #' @return Eps and Eta rispectively allocation of Risk-asset and Free-risk asset
  #' @export
  chi_1 = VPutOption_pricingfunction_det(t,N,(1+b)*S,p,a,b,r,k) - VPutOption_pricingfunction_det(t,N,(1+a)*S,p,a,b,r,k)
  eps = chi_1/(S*(b-a))
  chi_2 = VPutOption_pricingfunction_det((t-1),N,S,p,a,b,r,k)-eps*S
  eta = chi_2/(pi_0*((1+r)^(t-1)))
  return(c(eta,eps))
}
