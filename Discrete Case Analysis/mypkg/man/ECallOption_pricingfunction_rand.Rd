% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EOption_Random.R
\name{ECallOption_pricingfunction_rand}
\alias{ECallOption_pricingfunction_rand}
\title{Title  ECallOption_pricingfunction_rand
Give the price of an Asian Call Option - Montecarlo Formula}
\usage{
ECallOption_pricingfunction_rand(t, N, S, p, a, b, r, k, M)
}
\arguments{
\item{t}{The time we want the of the option}

\item{N}{Instants of time the strategy lasts}

\item{S}{S = (S_0,...,S_t)}

\item{p}{Up probability}

\item{a}{Down Interest}

\item{b}{Up interest}

\item{r}{Risk-free asset interest}

\item{k}{Strike price}

\item{M}{Number of independent variables in Montecarlo}
}
\value{
The price of the Asia Call Option
}
\description{
Title  ECallOption_pricingfunction_rand
Give the price of an Asian Call Option - Montecarlo Formula
}
