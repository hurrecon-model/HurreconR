library(HurreconR)
library(testthat)

local_edition(3)

# get hurrecon path
hur_path <- system.file("", package="HurreconR", mustWork=TRUE)

# copy input values to R temporary directory
tdir <- tempdir()

dir.create(paste0(tdir, '/input'))
dir.create(paste0(tdir, '/site'))
dir.create(paste0(tdir, '/region'))

file.copy(paste0(hur_path, '/input/ids.csv'), paste0(tdir, '/input/ids.csv'))
file.copy(paste0(hur_path, '/input/land_water.tif'), paste0(tdir, '/input/land_water.tif'))
file.copy(paste0(hur_path, '/input/parameters.csv'), paste0(tdir, '/input/parameters.csv'))
file.copy(paste0(hur_path, '/input/sites.csv'), paste0(tdir, '/input/sites.csv'))
file.copy(paste0(hur_path, '/input/tracks.csv'), paste0(tdir, '/input/tracks.csv'))

# test hurrecon_model_site
test_that("hurrecon_model_site", {
	hurrecon_model_site(hur_id="AL1935-03", site="Miami FL", msg=FALSE, hur_path=tdir)
	output_file <- paste0(tdir, "/site/AL1935-03_Miami_FL.csv")
	announce_snapshot_file(name="hurrecon_model_site")
	expect_snapshot_value(output_file, style="serialize", cran=FALSE, tolerance=0.001)
})

# test hurrecon_model_region
test_that("hurrecon_model_region", {
	hurrecon_model_region(hur_id="AL1935-03", msg=FALSE, hur_path=tdir)
	output_file <- paste0(tdir, "/region/AL1935-03.tif")
	announce_snapshot_file(name="hurrecon_model_region")
	expect_snapshot_value(output_file, style="serialize", cran=FALSE)
})

