% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/VanillaOptions_Random.R
\name{VPutOption_pricingfunction_rand}
\alias{VPutOption_pricingfunction_rand}
\title{Title  VPutOption_pricingfunction_rand
Give the price of an European Put Option - Montecarlo Formula p. 46}
\usage{
VPutOption_pricingfunction_rand(t, N, S, p, a, b, r, k, M)
}
\arguments{
\item{t}{The time we want the of the option}

\item{N}{Instants of time the strategy lasts}

\item{S}{S_t, price of the risk asset at time t}

\item{p}{Up probability}

\item{a}{Down Interest}

\item{b}{Up interest}

\item{r}{Risk-free asset interest}

\item{k}{Strike price}

\item{M}{Number of independent variables in Montecarlo}
}
\value{
The price of the European Put Option
}
\description{
Title  VPutOption_pricingfunction_rand
Give the price of an European Put Option - Montecarlo Formula p. 46
}
