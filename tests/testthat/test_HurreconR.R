library(HurreconR)
library(testthat)

local_edition(3)

# get hurrecon path
hur_path <- system.file("", package="HurreconR", mustWork=TRUE)

# get expected values
model_site_expected <- system.file("site", "AL1935-03_Miami_FL.csv", package="HurreconR", mustWork=TRUE)
model_region_expected <- system.file("region", "AL1935-03.tif", package="HurreconR", mustWork=TRUE)

# copy expected values to R temporary directory
tdir <- tempdir()
dir.create(paste0(tdir, '/input'))
dir.create(paste0(tdir, '/site'))
dir.create(paste0(tdir, '/region'))

file.copy(paste0(hur_path, '/input/ids.csv'), paste0(tdir, '/input/ids.csv'))
file.copy(paste0(hur_path, '/input/land_water.tif'), paste0(tdir, '/input/land_water.tif'))
file.copy(paste0(hur_path, '/input/parameters.csv'), paste0(tdir, '/input/parameters.csv'))
file.copy(paste0(hur_path, '/input/sites.csv'), paste0(tdir, '/input/sites.csv'))
file.copy(paste0(hur_path, '/input/tracks.csv'), paste0(tdir, '/input/tracks.csv'))

# get new values
hurrecon_model_site(hur_id="AL1935-03", site_name="Miami FL", time_step=60, hur_path=tdir)
model_site_new <- paste0(tdir, '/site/AL1935-03_Miami_FL.csv')

hurrecon_model_region(hur_id="AL1935-03", hur_path=tdir)
model_region_new <- paste0(tdir, '/region/AL1935-03.tif')

# test hurrecon_model_site
test_that("hurrecon_model_site", {
	expect_snapshot_file(model_site_new, model_site_expected, cran=FALSE)
})

# test hurrecon_summarize_site
test_that("hurrecon_summarize_site", {
	expect_snapshot_value(hurrecon_summarize_site(hur_id="AL1935-03", site_name="Miami FL", console=FALSE, hur_path=tdir), 
		style="serialize", cran=FALSE)
})

# test hurrecon_summarize_land_water
test_that("hurrecon_summarize_site", {
	expect_snapshot_value(hurrecon_summarize_land_water(console=FALSE, hur_path=tdir), style="serialize", cran=FALSE)
})

# test hurrecon_model_region
test_that("hurrecon_model_region", {
	expect_snapshot_file(model_region_new, model_region_expected, cran=FALSE)
})

