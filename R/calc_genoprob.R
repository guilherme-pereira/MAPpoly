#' Compute genotype conditional probabilities
#'
#' Conditional genotype probabilities are calculated for each marker
#' position and each individual given a map. In this version,
#' the probabilities are not calculated between markers.
#'
#' @param input.map An object of class \code{mappoly.map}
#' 
#' @param step 	Maximum distance (in cM) between positions at which 
#'              the genotype probabilities are calculated, though for 
#'              step = 0, probabilities are calculated only at the 
#'              marker locations.
#' 
#' @param phase.config which phase configuration should be used. "best" (default) 
#'                     will choose the phase configuration associated with the
#'                     maximum likelihood
#'
#' @param verbose if \code{TRUE} (default), current progress is shown; if
#'     \code{FALSE}, no output is produced
#'     
#' @param ... currently ignored
#'
#' @return An object of class 'mappoly.genoprob' which has two elements: a tridimensional
#' array containing the probabilities of all possible genotypes for each individual
#' in each marker position; and the marker sequence with it's recombination frequencies
#'
#' @examples
#'  \dontrun{
#'     data(hexafake)
#'     mrk.subset<-make_seq_mappoly(hexafake, 1:100)
#'     red.mrk<-elim_redundant(mrk.subset)
#'     unique.mrks<-make_seq_mappoly(red.mrk)
#'     counts.web<-cache_counts_twopt(unique.mrks, cached = TRUE)
#'     subset.pairs<-est_pairwise_rf(input.seq = unique.mrks,
#'                                   count.cache = counts.web,
#'                                   n.clusters = 16,
#'                                   verbose=TRUE)
#'     system.time(
#'     subset.map <- est_rf_hmm_sequential(input.seq = unique.mrks,
#'                                         thres.twopt = 5,
#'                                         thres.hmm = 3,
#'                                         extend.tail = 50,
#'                                         tol = 0.1,
#'                                         tol.final = 10e-3,
#'                                         twopt = subset.pairs,
#'                                         verbose = TRUE))
#'    probs<-calc_genoprob(input.map = subset.map,
#'                         verbose = TRUE)
#'    image(t(probs$probs[,,1]), col = rev(heat.colors(100)))
#'  }
#' 
#' @author Marcelo Mollinari, \email{mmollin@ncsu.edu}
#'
#' @references
#'     Mollinari, M., and Garcia, A.  A. F. (2019) Linkage
#'     analysis and haplotype phasing in experimental autopolyploid
#'     populations with high ploidy level using hidden Markov
#'     models, _G3: Genes, Genomes, Genetics_. 
#'     \url{https://doi.org/10.1534/g3.119.400378} 
#'
#' @export calc_genoprob
#'
calc_genoprob<-function(input.map, step = 0,  phase.config = "best", verbose = TRUE)
{
  if (!inherits(input.map, "mappoly.map")) {
    stop(deparse(substitute(input.map)), " is not an object of class 'mappoly.map'")
  }
  ## choosing the linkage phase configuration
  LOD.conf <- get_LOD(input.map, sorted = FALSE)
  if(phase.config == "best") {
    i.lpc <- which.min(LOD.conf)
  } else if (phase.config > length(LOD.conf)) {
    stop("invalid linkage phase configuration")
  } else i.lpc <- phase.config
  m<-input.map$info$m
  n.ind<-get(input.map$info$data.name, pos=1)$n.ind
  ## This is used to calculate genoprobs for 'add_marker' function
  if(length(input.map$info$mrk.names)==2 & input.map$info$mrk.names[1] == input.map$info$mrk.names[2]){
    D<-get(input.map$info$data.name, pos=1)$geno.dose[input.map$maps[[1]]$seq.num,]
    map.pseudo <- create_map(input.map, step, phase.config = i.lpc)
    mrknames<-names(map.pseudo)
    n.mrk<-length(map.pseudo)
    indnames<-colnames(D)
    dp <- get(input.map$info$data.name)$dosage.p[input.map$maps[[i.lpc]]$seq.num]
    dq <- get(input.map$info$data.name)$dosage.q[input.map$maps[[i.lpc]]$seq.num]
    phP <- lapply(input.map$maps[[i.lpc]]$seq.ph$P, function(x) x-1)
    phQ <- lapply(input.map$maps[[i.lpc]]$seq.ph$Q, function(x) x-1)
    seq.rf.pseudo <- input.map$maps[[i.lpc]]$seq.rf
  }
  else {
    Dtemp<-get(input.map$info$data.name, pos=1)$geno.dose[input.map$info$mrk.names,]
    map.pseudo <- create_map(input.map, step, phase.config = i.lpc)
    mrknames<-names(map.pseudo)
    n.mrk<-length(map.pseudo)
    indnames<-colnames(Dtemp)
    D<-matrix(m+1, nrow = length(map.pseudo), ncol = ncol(Dtemp), 
              dimnames = list(mrknames, indnames))
    D[rownames(Dtemp), ] <- as.matrix(Dtemp)
    dptemp <- get(input.map$info$data.name)$dosage.p[input.map$maps[[i.lpc]]$seq.num]
    dqtemp <- get(input.map$info$data.name)$dosage.q[input.map$maps[[i.lpc]]$seq.num]
    dq<-dp<-rep(m/2, length(mrknames))
    names(dp)<-names(dq)<-mrknames
    dp[names(dptemp)]<-dptemp
    dq[names(dqtemp)]<-dqtemp
    phPtemp <- lapply(input.map$maps[[i.lpc]]$seq.ph$P, function(x) x-1)
    phQtemp <- lapply(input.map$maps[[i.lpc]]$seq.ph$Q, function(x) x-1)
    phP <- phQ <- vector("list", n.mrk)
    for(i in 1:length(phP)){
      phP[[i]] <- phQ[[i]] <- c(0:(m/2 - 1))
    }
    names(phP) <- names(phQ) <- mrknames
    phP[rownames(Dtemp)] <- phPtemp
    phQ[rownames(Dtemp)] <- phQtemp
    seq.rf.pseudo <- mf_h(diff(map.pseudo))
  }
  for (j in 1:nrow(D))
    D[j, D[j, ] == input.map$info$m + 1] <- dp[j] + dq[j] + 1 + as.numeric(dp[j]==0 || dq[j]==0)
  res.temp<-.Call("calc_genoprob",
                  m,
                  t(D),
                  phP,
                  phQ,
                  seq.rf.pseudo,
                  as.numeric(rep(0, choose(m, m/2)^2 * n.mrk * n.ind)),
                  verbose=verbose,
                  PACKAGE = "mappoly")
  if(verbose) cat("\n")
  dim(res.temp[[1]])<-c(choose(m,m/2)^2,n.mrk,n.ind)
  dimnames(res.temp[[1]])<-list(kronecker(apply(combn(letters[1:m],m/2),2, paste, collapse=""),
                                          apply(combn(letters[(m+1):(2*m)],m/2),2, paste, collapse=""), paste, sep=":"),
                                  mrknames, indnames)
  structure(list(probs = res.temp[[1]], map = map.pseudo), class="mappoly.genoprob")
}

#' @rdname calc_genoprob
#' @keywords internal
#' @export
print.mappoly.genoprob <- function(x, ...) {
  cat("  This is an object of class 'mappoly.genoprob'")
  cat("\n  -----------------------------------------------------")
  ## printing summary
cat("\n  No. genotypic classes:                    ", dim(x$probs)[1], "\n")
  cat("  No. positions:                            ", dim(x$probs)[2], "\n")
  cat("  No. individuals:                          ", dim(x$probs)[3], "\n")
  cat("  -----------------------------------------------------\n")
}

#' Create a marker with pseudomarkers
#'
#' @param void interfunction to be documented
#' @keywords internal
#' @export
create_map<-function(input.map, step = 0,
                     phase.config = "best")
{
  ## choosing the linkage phase configuration
  LOD.conf <- get_LOD(input.map)
  if(phase.config == "best") {
    i.lpc <- which.min(LOD.conf)
  } else if (phase.config > length(LOD.conf)) {
    stop("invalid linkage phase configuration")
  } else i.lpc <- phase.config
  mrknames <- get(input.map$info$data.name, pos=1)$mrk.names[input.map$maps[[i.lpc]]$seq.num]
  if(length(unique(input.map$maps[[1]]$seq.num)) == 1){
    a<-rep(0,input.map$info$n.mrk)
    names(a)<-mrknames
    return(a)
  }
  map <- c(0, cumsum(imf_h(input.map$maps[[i.lpc]]$seq.rf)))
  names(map)<-mrknames
  if(round(step, 1) == 0)
    return(map)
  minloc <- min(map)
  map <- map-minloc
  a <- seq(floor(min(map)), max(map), by = step)
  a <- a[is.na(match(a,map))]
  names(a) <- paste("loc",a,sep = "_")
  return(sort(c(a,map))+minloc)
}

