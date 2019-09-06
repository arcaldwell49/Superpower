#devtools::build()

#Run the code below to ensure the package can be installed and passes all tests.

#Possibly remove objects from environment rm(list = ls(all = TRUE))
#Also detach all packages to ensure tests will run w/o issue on Travis CI
devtools::install()
library(ANOVApower)
devtools::test()
#devtools::check()
covr::package_coverage()




