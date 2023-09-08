ECallOption_pricingfunction_rand <- function(t,N,S,p,a,b,r,k,M){
  #' Title  ECallOption_pricingfunction_rand
  #' Give the price of an Asian Call Option - Montecarlo Formula
  #' @param t The time we want the of the option
  #' @param N Instants of time the strategy lasts
  #' @param S S = (S_0,...,S_t)
  #' @param p Up probability
  #' @param a Down Interest
  #' @param b Up interest
  #' @param r Risk-free asset interest
  #' @param k Strike price
  #' @param M Number of independent variables in Montecarlo
  #'
  #' @return The price of the Asia Call Option
  #' @export
  recursive_sum = 0
  for (j in 1:M) {
    binomial_vector = rbinom((N-t),1,p)
    random_string = (1+b)*binomial_vector + (1+a)*(ones((N-t),1) - binomial_vector)
    # Creating the j^th (S_0,...,S_t,S_t+1,...,S_N)
    S1 = c(S,random_string)
    for (i in t:(N-1)){
      S1[i+1]= S1[i]*S1[i+1]
    }
    recursive_sum = recursive_sum + max((sum(S1)/(N) - k),0)
  }
  E = (recursive_sum)/M
  price = (E)/((1+r)^(N-t))
}

EPutOption_pricingfunction_rand <- function(t,N,S,p,a,b,r,k,M){
  #' Title  EPutOption_pricingfunction_rand
  #' Give the price of an Asian Put Option - Montecarlo Formula
  #' @param t The time we want the of the option
  #' @param N Instants of time the strategy lasts
  #' @param S S = (S_0,...,S_t)
  #' @param p Up probability
  #' @param a Down Interest
  #' @param b Up interest
  #' @param r Risk-free asset interest
  #' @param k Strike price
  #' @param M Number of independent variables in Montecarlo
  #'
  #' @return The price of the Asia Put Option
  #' @export
  recursive_sum = 0
  for (j in 1:M) {
    binomial_vector = rbinom((N-t),1,p)
    random_string = (1+b)*binomial_vector + (1+a)*(ones((N-t),1) - binomial_vector)
    # Creating the j^th (S_0,...,S_t,S_t+1,...,S_N)
    S1 = c(S,random_string)
    for (i in t:(N-1)){
      S1[i+1]= S1[i]*S1[i+1]
    }
    recursive_sum = recursive_sum + max((k - (sum(S1)/(N))),0)
  }
  E = (recursive_sum)/M
  price = (E)/((1+r)^(N-t))
}

ECallOption_hedgingfunction_rand <- function(t,N,S,p,a,b,pi_0,r,k,M){
  #' ECallOption_hedgingfunction_det
  #' Calculate the portfolio allocation hedging the Asian Call Option - Formula p. 47 Proposition 3.2 - Random
  #' @param t The time we want the of the option
  #' @param N Instants of time the strategy lasts
  #' @param S S = (S_0,...,S_(t-1))
  #' @param p Up probability
  #' @param a Down Interest
  #' @param b Up interest
  #' @param pi_0 Price of Risk-less assset at time t=0
  #' @param r Risk-free asset interest
  #' @param k Strike price
  #' @param M Number of independent variables in Montecarlo
  #'
  #' @return Eps and Eta rispectively allocation of Risk-asset and Free-risk asset
  #' @export
  chi_1 = (ECallOption_pricingfunction_rand(t,N,c(S,(1+b)*S[t-1]),p,a,b,r,k,M) - ECallOption_pricingfunction_rand(t,N,c(S,(1+a)*S[t-1]),p,a,b,r,k,M))
  eps = chi_1/(S[t-1]*(b-a))
  chi_2 = (ECallOption_pricingfunction_rand((t-1),N,S,p,a,b,r,k,M)-eps*S[t-1])
  eta = chi_2/(pi_0*((1+r)^(t-1)))
  return(c(eta,eps))
}
EPutOption_hedgingfunction_rand <- function(t,N,S,p,a,b,pi_0,r,k,M){
  #' ECallOption_hedgingfunction_det
  #' Calculate the portfolio allocation hedging the Asian Put Option - Formula p. 47 Proposition 3.2 - Random
  #' @param t The time we want the of the option
  #' @param N Instants of time the strategy lasts
  #' @param S S = (S_0,...,S_(t-1))
  #' @param p Up probability
  #' @param a Down Interest
  #' @param b Up interest
  #' @param pi_0 Price of Risk-less assset at time t=0
  #' @param r Risk-free asset interest
  #' @param k Strike price
  #' @param M Number of independent variables in Montecarlo
  #'
  #' @return Eps and Eta rispectively allocation of Risk-asset and Free-risk asset
  #' @export
  chi_1 = (EPutOption_pricingfunction_rand(t,N,c(S,(1+b)*S[t-1]),p,a,b,r,k,M) - EPutOption_pricingfunction_rand(t,N,c(S,(1+a)*S[t-1]),p,a,b,r,k,M))
  eps = chi_1/(S[t-1]*(b-a))
  chi_2 = (EPutOption_pricingfunction_rand((t-1),N,S,p,a,b,r,k,M)-eps*S[t-1])
  eta = chi_2/(pi_0*((1+r)^(t-1)))
  return(c(eta,eps))
}
