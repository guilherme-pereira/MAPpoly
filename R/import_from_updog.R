#' Import from updog
#'
#' Read objects with information related to genotype calling in polyploids.
#' Currently this function supports output objects created with the
#' \code{updog} (output of \code{multidog} function) package.
#' This function creates an object of class \code{mappoly.data}
#'
#' @param object the name of the object of class \code{multidog}
#'     
#' @param prob.thres probability threshold to associate a marker call to a 
#'     dosage. Markers with maximum genotype probability smaller than 'prob.thres' 
#'     are considered as missing data for the dosage calling purposes
#'     
#' @param filter.non.conforming if \code{TRUE} (default) exclude samples with non 
#'     expected genotypes under random chromosome pairing and no double reduction 
#'
#' @return An object of class \code{mappoly.data} which contains a
#'     list with the following components:
#'     \item{m}{ploidy level}
#'     \item{n.ind}{number individuals}
#'     \item{n.mrk}{total number of markers}
#'     \item{ind.names}{the names of the individuals}
#'     \item{mrk.names}{the names of the markers}
#'     \item{dosage.p}{a vector containing the dosage in
#'       parent P for all \code{n.mrk} markers}
#'     \item{dosage.q}{a vector containing the dosage in
#'       parent Q for all \code{n.mrk} markers}
#'     \item{sequence}{a vector indicating which sequence each marker
#'       belongs. Zero indicates that the marker was not assigned to any
#'       sequence}
#'     \item{sequence.pos}{physical position of the markers into the
#'       sequence}
#'     \item{prob.thres}{probability threshold to associate a marker call to a 
#'     dosage. Markers with maximum genotype probability smaller than 'prob.thres' 
#'     were considered as missing data in the 'geno.dose' matrix}
#'     \item{geno.dose}{a matrix containing the dosage for each markers (rows) 
#'       for each individual (columns). Missing data are represented by 
#'       \code{ploidy_level + 1}}
#'     \item{geno}{a data.frame 
#'       containing the probability distribution for each combination of
#'       marker and offspring. The first two columns represent the marker
#'       and the offspring, respectively. The remaining elements represent
#'       the probability associated to each one of the possible
#'       dosages. Missing data are converted from \code{NA} to the expected
#'       segregation ratio using function \code{\link[mappoly]{segreg_poly}}}
#'     \item{n.phen}{number of phenotypic traits}
#'     \item{phen}{a matrix containing the phenotypic data. The rows
#'                 corespond to the trais and the columns correspond
#'                 to the individuals}
#'     \item{chisq.pval}{a vector containing p-values related to the chi-squared 
#'     test of mendelian segregation performed for all markers}
#'     
#' @examples
#' \dontrun{
#' library("updog")
#' data("uitdewilligen")
#' mout = multidog(refmat = t(uitdewilligen$refmat), 
#'                 sizemat = t(uitdewilligen$sizemat), 
#'                 ploidy = uitdewilligen$ploidy, 
#'                 model = "f1",
#'                 p1_id = colnames(t(uitdewilligen$sizemat))[1],
#'                 p2_id = colnames(t(uitdewilligen$sizemat))[2],
#'                 nc = 4)
#' mydata = import_from_updog(mout)
#'}
#'
#' @author Gabriel Gesteira, \email{gabrielgesteira@usp.br}
#'
#' @references
#'     Mollinari, M., and Garcia, A.  A. F. (2019) Linkage
#'     analysis and haplotype phasing in experimental autopolyploid
#'     populations with high ploidy level using hidden Markov
#'     models, _G3: Genes, Genomes, Genetics_. 
#'     \url{https://doi.org/10.1534/g3.119.400378}
#'     
#' @export import_from_updog
#' 
import_from_updog = function(object, prob.thres = NULL, filter.non.conforming = FALSE){
  # Case 1: updog
  if (class(object) == "multidog"){
    m = object$snpdf$ploidy[1]
    dosage.p = object$snpdf$p1geno
    dosage.q = object$snpdf$p2geno
    if (is.null(prob.thres)) prob.thres = 0.95
    # Selecting conforming markers
    dp = abs(abs(dosage.p-(m/2))-(m/2))
    dq = abs(abs(dosage.q-(m/2))-(m/2))
    id = dp+dq!=0
    dosage.p = dosage.p[id]
    dosage.q = dosage.q[id]
    mrk.names = unique(as.character(object$snpdf$snp))[id]
    mrk.sel = which(object$inddf$snp %in% mrk.names)
    geno = data.frame(mrk = as.character(object$inddf$snp)[mrk.sel], ind = as.character(object$inddf$ind)[mrk.sel], stringsAsFactors = FALSE)
    for (i in 0:m){
      geno[[paste0(i)]] = object$inddf[[paste0('Pr_', i)]][mrk.sel]
    }
    ind.names = unique(geno$ind)
    n.ind = length(ind.names)
    n.mrk = length(mrk.names)
    sequence = NULL
    sequence.pos = NULL
    ## dosage info
    if(filter.non.conforming)
      geno.dose = matrix(NA,1,1)
    else{
      geno.dose = dist_prob_to_class(geno = geno, prob.thres = prob.thres)
      geno.dose[is.na(geno.dose)] = m + 1
    }
    nphen = 0
    phen = NULL
    res<-structure(list(m = m,
                        n.ind = n.ind,
                        n.mrk = sum(id),
                        ind.names = ind.names,
                        mrk.names = mrk.names,
                        dosage.p = dosage.p,
                        dosage.q = dosage.q,
                        sequence = sequence,
                        sequence.pos = sequence.pos,
                        prob.thres = prob.thres,
                        geno = geno,
                        geno.dose = geno.dose,
                        nphen = nphen,
                        phen = phen,
                        chisq.pval = NULL),
                   class = "mappoly.data")
    
    if(filter.non.conforming){
      cat("    Filtering non-conforming markers.\n    ...")
      res<-filter_non_conforming_classes(res)
      cat("\n    Performing chi-square test.\n    ...")
      ##Computing chi-square p.values
      Ds <- array(NA, dim = c(m+1, m+1, m+1))
      for(i in 0:m)
        for(j in 0:m)
          Ds[i+1,j+1,] <- segreg_poly(m = m, dP = i, dQ = j)
      Dpop<-cbind(res$dosage.p, res$dosage.q)
      M<-t(apply(Dpop, 1, function(x) Ds[x[1]+1, x[2]+1,]))
      dimnames(M)<-list(res$mrk.names, c(0:m))
      M<-cbind(M, res$geno.dose)
      res$chisq.pval<-apply(M, 1, mrk_chisq_test, m = m)
      cat("\n    Done.\n")
    }
    return(res)
  }
  # # Case 2: polyRAD
  # else if (class(object) == 'RADdata'){
  #   outfile = paste0(getwd(), '/import_temp')
  #   polyRAD::Export_MAPpoly(object, file = outfile)
  #   res = read_geno_dist(outfile)
  #   return(res)
  # }
  else stop("You must provide an object of class 'multidog'(from package 'updog') in order to continue importing data. Please read the documentation and try again.")
}