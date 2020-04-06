context("Read dataset")
test_that("Data loaded correctly when using 'read_geno'", {
  ## Tetraploid
  expect_equal(check_data_sanity(tetra.solcap), 0)
  tetra.file<-system.file('extdata', 'tetra_solcap_geno', package = 'mappoly')
  tetra.solcap.new<-read_geno(file.in  = tetra.file)
  expect_equal(check_data_sanity(tetra.solcap.new), 0)
  expect_equivalent(as.numeric(as.matrix(tetra.solcap.new$geno.dose))[1:20], 
                    c(4, 5, 3, 2, 3, 2, 5, 5, 1, 3, 
                      1, 4, 2, 1, 3, 2, 0, 3, 2, 2))
  expect_equal(tetra.solcap, tetra.solcap.new)
  ##### Hexaploid
  expect_equal(check_data_sanity(hexafake), 0)
  hexa.file<-system.file('extdata', 'hexafake_geno', package = 'mappoly')
  hexafake.new<-read_geno(file.in  = hexa.file)
  expect_equal(check_data_sanity(hexafake.new), 0)
  expect_equivalent(as.numeric(as.matrix(hexafake.new$geno.dose))[1:20], 
                    c(1, 0, 2, 1, 0, 2, 2, 1, 3, 2, 
                      2, 2, 0, 1, 2, 1, 2, 2, 2, 0))
  expect_equal(hexafake, hexafake.new)
})

test_that("Data loaded correctly when using 'read_geno_dist'", {
  ## Tetraploid
  expect_equal(check_data_sanity(tetra.solcap.geno.dist), 0)
  tetra.file <- system.file('extdata', 'tetra_solcap_geno_dist.bz2', package = 'mappoly')
  tetra.dat.dist <- read_geno_dist(file.in  = tetra.file)
  expect_equal(check_data_sanity(tetra.dat.dist), 0)
  expect_equivalent(as.numeric(as.matrix(tetra.dat.dist$geno.dose))[1:20], 
                    c(4, 5, 3, 2, 3, 2, 5, 5, 1, 3, 
                      1, 4, 2, 1, 3, 2, 0, 3, 2, 2))
  expect_equivalent(tetra.solcap.geno.dist, tetra.dat.dist)
  
  ##### Hexaploid
  expect_equal(check_data_sanity(hexafake.geno.dist), 0)
  hexa.file <- system.file('extdata', 'hexafake_geno_dist.bz2', package = 'mappoly')
  hexa.dat.dist <- read_geno_dist(file.in  = hexa.file)
  expect_equal(check_data_sanity(hexa.dat.dist), 0)
  id <- colnames(hexafake$geno.dose)
  M1 <- as.matrix(hexafake$geno.dose[1:5, 1:5])
  M2 <- as.matrix(hexa.dat.dist$geno.dose[,id][1:5, 1:5])
  expect_equivalent(M1, M2)
  expect_equivalent(hexafake.geno.dist, hexa.dat.dist)
})

