% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/poly_cross_simulate.R
\name{poly_cross_simulate}
\alias{poly_cross_simulate}
\title{Simulate an autopolyploid full-sib population}
\usage{
poly_cross_simulate(
  m,
  rf.vec,
  n.mrk,
  n.ind,
  hom.allele,
  draw = FALSE,
  file = "output.pdf",
  seed = NULL,
  width = 12,
  height = 6,
  prob.P = NULL,
  prob.Q = NULL
)
}
\arguments{
\item{m}{ploidy level. Must be an even number}

\item{rf.vec}{vector containing the recombination fractions between
adjacent markers. If a single recombination fraction is
provided, it is repeated \eqn{n.mrk-1} times}

\item{n.mrk}{number of markers}

\item{n.ind}{number of individuals in the offspring}

\item{hom.allele}{a list containing the linkage phase information for both parents}

\item{draw}{if \code{TRUE}, draws a graphical representation of the
parental map, including the linkage phase configuration, in a
pdf output (default = FALSE)}

\item{file}{name of the output file. It is ignored if
\code{draw = TRUE}}

\item{seed}{random number generator seed (default = NULL)}

\item{width}{the width of the graphics region in inches (default = 12)}

\item{height}{the height of the graphics region in inches (default = 6)}

\item{prob.P}{a vector indicating the proportion of preferential
pairing in parent P (currently ignored)}

\item{prob.Q}{a vector indicating the proportion of preferential
pairing in parent Q (currently ignored)}
}
\value{
an object of class \code{mappoly.data}. See
    \code{\link[mappoly]{read_geno}} for more information
}
\description{
Simulate an autopolyploid full-sib population with one or two 
informative parents under random chromosome segregation.
}
\details{
\code{hom.allele.p} and \code{hom.allele.q} are lists of vectors
 containing linkage phase configurations. Each vector contains the
 numbers of the homologous chromosomes in which the alleles are
 located. For instance, a vector containing \eqn{(1,3,4)} means that
 the marker has three doses located in the chromosomes 1, 3 and 4. For
 zero doses, use 0.
}
\examples{
    h.temp<-sim_homologous(m=6, n.mrk=20, max.d=3, max.ph=3, seed=123)
    fake.poly.dat<-poly_cross_simulate(m=6, rf.vec=.05, n.mrk=20,
                                  n.ind=200, h.temp, seed=123)
                                  
}
\references{
Mollinari, M., and Garcia, A.  A. F. (2019) Linkage
    analysis and haplotype phasing in experimental autopolyploid
    populations with high ploidy level using hidden Markov
    models, _G3: Genes, Genomes, Genetics_. 
    \url{https://doi.org/10.1534/g3.119.400378}
}
\author{
Marcelo Mollinari, \email{mmollin@ncsu.edu}
}
