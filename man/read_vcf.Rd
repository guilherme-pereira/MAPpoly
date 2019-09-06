% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_mappoly_vcf.R
\name{read_vcf}
\alias{read_vcf}
\title{Data Input VCF}
\usage{
read_vcf(file.in, filter.non.conforming = TRUE, parent.1, parent.2,
  ploidy = NA, thresh.line = 0.05, update.prob = FALSE,
  output = NULL)
}
\arguments{
\item{file.in}{name of input file which contains the data to
be read.}

\item{filter.non.conforming}{if \code{TRUE} (default) exclude samples with non 
expected genotypes under random chromosome pairing and no double reduction}

\item{parent.1}{name of parent 1}

\item{parent.2}{name of parent 2}

\item{ploidy}{the species ploidy (optional, it will be automatically detected)}

\item{thresh.line}{threshold used for p-values on segregation test}

\item{update.prob}{Logical. Should MAPpoly update genotype probabilities based on HMM? (default = FALSE)}

\item{output}{if \code{update.prob = TRUE}, defines the output of the updated .vcf file}

\item{...}{currently ignored}
}
\value{
An object of class \code{mappoly.data} which contains a
    list with the following components:
    \item{m}{ploidy level}
    \item{n.ind}{number individuals}
    \item{n.mrk}{total number of markers}
    \item{ind.names}{the names of the individuals}
    \item{mrk.names}{the names of the markers}
    \item{dosage.p}{a vector containing the dosage in
      parent P for all \code{n.mrk} markers}
    \item{dosage.q}{a vector containing the dosage in
      parent Q for all \code{n.mrk} markers}
    \item{sequence}{a vector indicating which sequence each marker
      belongs. Zero indicates that the marker was not assigned to any
      sequence.}
    \item{sequence.pos}{Physical position of the markers into the
      sequence}
    \item{geno.dose}{a matrix containing the dosage for each markers (rows) 
      for each individual (columns). Missing data are represented by 
      \code{ploidy_level + 1}.}
    \item{input.file}{Full path to input file, used when \code{update.prob = TRUE}}
}
\description{
Reads an external VCF file. This function accepts version 4.0 and higher, and creates an object of class \code{mappoly.data}
}
\details{
This function can handle .vcf files of version 4.0 and higher. The ploidy can be automatically detected, but you
should inform it to check mismatches. All individual and marker names will be kept as they are in the .vcf file.
}
\examples{
\dontrun{
    mydata <- read_vcf(hexasubset, parent.1 = "P1", parent.2 = "P2")
    print(mydata, detailed = TRUE)
}
}
\references{
Mollinari, M., and Garcia, A.  A. F. (2019) Linkage
    analysis and haplotype phasing in experimental autopolyploid
    populations with high ploidy level using hidden Markov
    models, _G3: Genes, Genomes, Genetics_. 
    \url{https://doi.org/10.1534/g3.119.400378}
}
\author{
Gabriel Gesteira, \email{gabrielgesteira@usp.br}
}