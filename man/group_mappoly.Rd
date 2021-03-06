% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/group.R
\name{group_mappoly}
\alias{group_mappoly}
\alias{print.mappoly.group}
\alias{plot.mappoly.group}
\title{Assign markers to linkage groups}
\usage{
group_mappoly(
  input.mat,
  expected.groups = NULL,
  inter = TRUE,
  comp.mat = FALSE,
  verbose = TRUE
)

\method{print}{mappoly.group}(x, detailed = TRUE, ...)

\method{plot}{mappoly.group}(x, ...)
}
\arguments{
\item{input.mat}{an object of class \code{mappoly.rf.matrix}}

\item{expected.groups}{when available, inform the number of expected 
linkage groups (i.e. chromosomes) for the species}

\item{inter}{if \code{TRUE} (default), plots a dendrogram highlighting the
expected groups before continue}

\item{comp.mat}{if \code{TRUE}, shows a comparison between the reference
based and the linkage based grouping, if the sequence information is
available (default = FALSE)}

\item{verbose}{logical. If \code{TRUE} (default), current progress is shown;
if \code{FALSE}, no output is produced}

\item{x}{an object of class \code{onemap.segreg.test}}

\item{detailed}{logical. If \code{TRUE} (default) the markers in each
linkage group are printed}

\item{...}{currently ignored}
}
\value{
Returns an object of class \code{mappoly.group}, which is a list
    containing the following components:
    \item{data.name}{the referred dataset name}
    \item{hc.snp}{a list containing information related to 
    the UPGMA grouping method}
    \item{expected.groups}{the number of expected linkage groups}
    \item{groups.snp}{the groups to which each of the markers belong}
    \item{seq.vs.grouped.snp}{comparison between the genomic group information
    (when available) and the groups provided by \code{group_mappoly}}
    \item{chisq.pval.thres}{the threshold used on the segregation test when reading the dataset}
    \item{chisq.pval}{the p-values associated with the segregation test for all markers in the sequence}
}
\description{
Identifies linkage groups of markers using the results of two-point
(pairwise) analysis.
}
\examples{
 \dontrun{
    data(hexafake)
    all.mrk <- make_seq_mappoly(hexafake, 'all')
    red.mrk <- elim_redundant(all.mrk)
    unique.mrks <- make_seq_mappoly(red.mrk)
    counts <- cache_counts_twopt(unique.mrks, cached = TRUE)
    all.pairs <- est_pairwise_rf(input.seq = unique.mrks,
                                 count.cache = counts,
                                 n.clusters = 16,
                                 verbose=TRUE)

    ## Full recombination fraction matrix
    mat.full<-rf_list_to_matrix(input.twopt=all.pairs)
    plot(mat.full)

    lgs <- group_mappoly(input.mat = mat.full,
                         input.seq = unique.mrks,
                         expected.groups = 3,
                         inter = TRUE,
                         comp.mat = TRUE, #this data has physical information
                         verbose = TRUE)
    lgs
    plot(lgs)
    lg1 <- make_seq_mappoly(lgs, 1)
    lg2 <- make_seq_mappoly(lgs, 2)
    lg3 <- make_seq_mappoly(lgs, 3)

    ##Plot matrices
    m1<-make_mat_mappoly(input.seq = lg1, input.mat = mat.full)
    m2<-make_mat_mappoly(input.seq = lg2, input.mat = mat.full)
    m3<-make_mat_mappoly(input.seq = lg3, input.mat = mat.full)
    op<-par(mfrow = c(1,3), pty = "s")
    plot(m1, main.text = "LG1")
    plot(m2, main.text = "LG2")
    plot(m3, main.text = "LG3")
    par(op)
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
Marcelo Mollinari, \email{mmollin@ncsu.edu}
}
\keyword{internal}
