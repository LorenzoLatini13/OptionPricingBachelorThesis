% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EOption_Random.R
\name{ECallOption_hedgingfunction_rand}
\alias{ECallOption_hedgingfunction_rand}
\title{ECallOption_hedgingfunction_det
Calculate the portfolio allocation hedging the Asian Call Option - Formula p. 47 Proposition 3.2 - Random}
\usage{
ECallOption_hedgingfunction_rand(t, N, S, p, a, b, pi_0, r, k, M)
}
\arguments{
\item{t}{The time we want the of the option}

\item{N}{Instants of time the strategy lasts}

\item{S}{S = (S_0,...,S_(t-1))}

\item{p}{Up probability}

\item{a}{Down Interest}

\item{b}{Up interest}

\item{pi_0}{Price of Risk-less assset at time t=0}

\item{r}{Risk-free asset interest}

\item{k}{Strike price}

\item{M}{Number of independent variables in Montecarlo}
}
\value{
Eps and Eta rispectively allocation of Risk-asset and Free-risk asset
}
\description{
ECallOption_hedgingfunction_det
Calculate the portfolio allocation hedging the Asian Call Option - Formula p. 47 Proposition 3.2 - Random
}
