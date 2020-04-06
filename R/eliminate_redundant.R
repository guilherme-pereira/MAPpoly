#' Eliminate redundant markers
#'
#' Eliminate markers with identical dosage infomation throughout individuals.
#'
#' @param input.seq an object of class \code{mappoly.sequence}
#'
#' @param data name of the dataset that contains sequence markers (optional, default = NULL)
#' 
#' @param ... currently ignored
#'
#' @return An object of class \code{mappoly.unique.seq} which
#'     is a list  containing the following components:
#'     \item{unique.seq}{an object of class \code{mappoly.sequence}
#'           with the redundant markers removed}
#'     \item{kept}{a vector containing the name of the informative markers}
#'     \item{eliminated}{a vector containing the name of the non-informative (eliminated) markers}
#'
#' @examples
#'   \dontrun{
#'     all.mrk<-make_seq_mappoly(tetra.solcap.geno.dist, 'all')
#'     red.mrk<-elim_redundant(all.mrk)
#'     plot(red.mrk)
#'     unique.mrks<-make_seq_mappoly(red.mrk)
#'    }
#'    
#' @author Marcelo Mollinari, \email{mmollin@ncsu.edu}, with minor modifications by Gabriel Gesteira, \email{gabrielgesteira@usp.br}
#'
#' @references
#'     Mollinari, M., and Garcia, A.  A. F. (2019) Linkage
#'     analysis and haplotype phasing in experimental autopolyploid
#'     populations with high ploidy level using hidden Markov
#'     models, _G3: Genes, Genomes, Genetics_. 
#'     \url{https://doi.org/10.1534/g3.119.400378} 
#'
#' @export elim_redundant
elim_redundant<-function(input.seq)
{
  x<-get(input.seq$data.name, pos = 1)
  dat_temp <- unique(x$geno.dose[input.seq$seq.mrk.names, ], dimnames = TRUE)
  output.seq <- make_seq_mappoly(x, rownames(dat_temp),data.name = input.seq$data.name)
  elim<-setdiff(input.seq$seq.mrk.names, output.seq$seq.mrk.names)
  n1<-apply(dat_temp, 1, paste, collapse="")
  n2<-apply(x$geno.dose[elim, ], 1, paste, collapse="")
  elim.out <- data.frame(kept = rownames(dat_temp)[match(n2,n1)], elim = elim)
  structure(list(unique.seq = output.seq, kept = output.seq$seq.mrk.names,
                 elim.correspondence =  elim.out),
            class = "mappoly.unique.seq")
}

#' @rdname elim_redundant
#' @keywords internal
#' @export
plot.mappoly.unique.seq<-function(x, ...)
{
  slc <- c(nrow(x$elim.correspondence), length(x$kept))
  lbls <- c("eliminated", "kept")
  pct <- round(slc/sum(slc)*100)
  pct <- paste0(slc, " (", pct, "%)")
  lbls <- paste(lbls, pct, sep = "\n")
  pie(slc, labels = lbls)
}

#' @rdname elim_redundant
#' @keywords internal
#' @export
print.mappoly.unique.seq<-function(x, ...)
{
  print(x$unique.seq)
  cat("------------\n")
  cat("Eliminated markers: ", nrow(x$elim.correspondence), "\n")
}

