% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reestimate_map.R
\name{reest_rf}
\alias{reest_rf}
\title{Reestimate the recombination fractions in a genetic map}
\usage{
reest_rf(
  input.map,
  input.mat = NULL,
  tol = 0.01,
  phase.config = "all",
  method = c("hmm", "ols"),
  weight = TRUE,
  verbose = TRUE,
  high.prec = FALSE,
  max.rf.to.break.EM = 0.5
)
}
\arguments{
\item{input.map}{An object of class \code{mappoly.map}}

\item{input.mat}{An object of class \code{mappoly.rf.matrix}}

\item{tol}{tolerance for determining convergence (default = 10e-03)}

\item{phase.config}{which phase configuration should be used. "best" (default) 
will choose the maximum likelihood configuration}

\item{method}{indicates whether to use \code{'hmm'} (Hidden Markov Models) 
or \code{'ols'} (Ordinary Least Squares) to reestimate the recombination fractions}

\item{weight}{if \code{TRUE} (default), it uses the LOD scores to perform a weighted
regression when the Ordinary Least Saquares is chosen}

\item{verbose}{if \code{TRUE} (default), current progress is shown; if
\code{FALSE}, no output is produced}

\item{high.prec}{logical. If \code{TRUE} uses high precision 
(long double) numbers in the HMM procedure implemented in C++,
which can take a long time to perform (default = FALSE)}

\item{max.rf.to.break.EM}{for internal use only.}
}
\value{
a numeric vector of size \code{m} indicating which
    homologous in h2 represents the homologous in h1. If there is
    no correspondence, i.e. different homologous, it returns NA for
    that homologous
}
\description{
This function reestimates the recombination fractions between all markers in a given map.
}
\references{
Mollinari, M., and Garcia, A.  A. F. (2019) Linkage
    analysis and haplotype phasing in experimental autopolyploid
    populations with high ploidy level using hidden Markov
    models, _G3: Genes, Genomes, Genetics_. 
    \url{https://doi.org/10.1534/g3.119.400378}
}
