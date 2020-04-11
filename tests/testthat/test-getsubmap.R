context("Get submap")
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
#   sub.map2<-get_submap(solcap.dose.map[[1]],
#                        count.cache = counts,
#                        mrk.pos = c(1,3,5,7,9,11),
#                        tol.final = 10e-4)
#   expect_equal(sub.map1, sub.map2)
#   sub.map3<-get_submap(solcap.dose.map[[1]],
#                        count.cache = counts,
#                        mrk.pos = c(1,3,5,7,9,11),
#                        tol.final = 10e-4, 
#                        reestimate.rf = FALSE)
#   expect_equal(sub.map1$info, sub.map3$info)
#   ##### Hexaaploid
#   counts <- get_cache_two_pts_from_web(6)
#   sub.map1<-get_submap(maps.hexafake[[1]],
#                        count.cache = counts,
#                        mrk.pos = c(1,3,5,7,9,11),
#                        reestimate.phase = TRUE,
#                        tol.final = 10e-4)
#   expect_is(sub.map1, "mappoly.map")
#   expect_output(str(sub.map1$info), "List of 13")
#   expect_equivalent(sub.map1$maps[[1]]$seq.rf,
#                     c(0.0003204066, 0.0002654942, 
#                       0.0002754820, 0.0004649893, 
#                       0.0008550641))
#   sub.map2<-get_submap(maps.hexafake[[1]],
#                        count.cache = counts,
#                        mrk.pos = c(1,3,5,7,9,11),
#                        tol.final = 10e-4)
#   expect_is(sub.map2, "mappoly.map")
#   expect_output(str(sub.map2$info), "List of 13")
#   expect_equivalent(sub.map2$maps[[1]]$seq.rf,
#                     c(0.0003204066, 0.0002654942, 
#                       0.0002754820, 0.0004649893, 
#                       0.0008550641))
#   #expect_equivalent(sub.map1, sub.map2)
#   sub.map3<-get_submap(maps.hexafake[[1]],
#                        count.cache = counts,
#                        mrk.pos = c(1,3,5,7,9,11),
#                        tol.final = 10e-4, 
#                        reestimate.rf = FALSE)
#   expect_is(sub.map2, "mappoly.map")
#   expect_output(str(sub.map2$info), "List of 13")
#   #expect_equal(sub.map1$info, sub.map3$info)
# })