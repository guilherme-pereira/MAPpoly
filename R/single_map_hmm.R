#' Multipoint analysis using Hidden Markov Models (single phase)
#'
#' @param void interfunction to be documented
#' @keywords internal
#' @examples
#'   \dontrun{
#'     seq.all.mrk <- make_seq_mappoly(hexafake, 'all')
#'     id <- get_genomic_order(seq.all.mrk)
#'     counts <- cache_counts_twopt(seq.all.mrk, cached=TRUE)
#'     seq5 <- make_seq_mappoly(hexafake, rownames(id)[1:5])
#'     twopt<-est_pairwise_rf(seq5, counts)
#'     
#'     ## Using the first 10 markers 
#'     l5 <- ls_linkage_phases(input.seq = seq5, thres = 5, twopt = twopt)
#'     plot(l5)
#'     
#'     ## Evaluating 9 linkage phase configurations using HMM
#'     maps1 <- vector("list", length(l5$config.to.test))
#'     for(i in 1:length(maps1))
#'     maps1[[i]] <- est_rf_hmm_single(seq5, l5$config.to.test[[i]], 
#'                                    tol = 10e-3, verbose = TRUE, 
#'                                    high.prec = FALSE)
#'    (best<-which.max(sapply(maps1, function(x) x$loglike)))
#'    dist1<-round(cumsum(c(0, imf_h(maps1[[best]]$seq.rf))),2)
#'    
#'    ## Same thing using automatic search
#'    maps2<-est_rf_hmm(input.seq = seq5, twopt = twopt, thres = 5, 
#'                      verbose = TRUE, tol = 10e-3, high.prec = FALSE)
#'    plot(maps2)
#'    dist1
#'  }
#' @author Marcelo Mollinari, \email{mmollin@ncsu.edu}
#' @export est_rf_hmm_single
est_rf_hmm_single<-function(input.seq,
                            input.ph.single,
                            rf.temp = NULL,
                            tol,
                            verbose = FALSE,
                            ret.map.no.rf.estimation = FALSE,
                            high.prec = TRUE,
                            max.rf.to.break.EM = 0.5)
{
  input_classes <- c("mappoly.sequence")
  if (!inherits(input.seq, input_classes[1])) {
    stop(deparse(substitute(input.seq)), " is not an object of class 'mappoly.sequence'")
  }
  if(length(input.seq$seq.num) == 1)
    stop("Input sequence contains only one marker.", call. = FALSE)
  if(is.null(rf.temp))
    rf.temp<-rep(0.001, length(input.seq$seq.num)-1)
  if(!ret.map.no.rf.estimation)
  {
    D<-get(input.seq$data.name, pos=1)$geno.dose[input.seq$seq.num,]
    dp <- get(input.seq$data.name)$dosage.p[input.seq$seq.num]
    dq <- get(input.seq$data.name)$dosage.q[input.seq$seq.num]
    for (j in 1:nrow(D))
      D[j, D[j, ] == input.seq$m + 1] <- dp[j] + dq[j] + 1 + as.numeric(dp[j]==0 || dq[j]==0)

    if(high.prec)
    {
      res.temp<-.Call("est_map_hmm_highprec",
                      input.seq$m,
                      t(D),
                      lapply(input.ph.single$P, function(x) x-1),
                      lapply(input.ph.single$Q, function(x) x-1),
                      rf.temp,
                      verbose=verbose,
                      max.rf.to.break.EM,
                      tol,
                      PACKAGE = "mappoly")
    } else{
      res.temp<-.Call("est_map_hmm",
                      input.seq$m,
                      t(D),
                      lapply(input.ph.single$P, function(x) x-1),
                      lapply(input.ph.single$Q, function(x) x-1),
                      rf.temp,
                      verbose=verbose,
                      max.rf.to.break.EM,
                      tol,
                      PACKAGE = "mappoly")
    }
  } else res.temp<-list(1, rf.temp)
  map<-list(seq.num=input.seq$seq.num,
            seq.rf=res.temp[[2]],
            seq.ph=input.ph.single,
            loglike=res.temp[[1]])
  map
}
