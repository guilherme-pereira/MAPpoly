# context("Get submap")
# test_that("sub-map extracted correctly", {
#   ##### Tetraploid
#   counts <- get_cache_two_pts_from_web(4)
#   sub.map1<-get_submap(solcap.dose.map[[1]], 
#                        count.cache = counts,
#                        mrk.pos = c(1,3,5,7,9,11), 
#                        reestimate.phase = TRUE, 
#                        tol.final = 10e-4)
#   expect_is(sub.map1, "mappoly.map")
#   expect_output(str(sub.map1$info), "List of 13")
#   expect_equivalent(sub.map1$maps[[1]]$seq.rf, 
#                     c(0.0367743795, 0.0032793958, 
#                       0.0177094932, 0.0126662760,
#                       0.0003633027))
# })