library(HurreconR)
library(testthat)

# Test hurrecon_model_site
test.expected <- system.file("site", "AL1935-03_Miami_FL.csv", package="HurreconR", mustWork=TRUE)
hur_path <- system.file("", package="HurreconR", mustWork=TRUE)
expect_snapshot_value(hurrecon_model_site(hur_id="AL1935-03", site_name="Miami FL", time_step=60, 
	save=FALSE, console=FALSE, hur_path=hur_path), test.expected, style="serialize", cran=FALSE, 
	tolerance=0.001)

# Test hurrecon_summarize_site
test.expected <- system.file("site", "site_summary.expected", package="HurreconR", mustWork=TRUE)
hur_path <- system.file("", package="HurreconR", mustWork=TRUE)
expect_snapshot_value(hurrecon_summarize_site(hur_id="AL1935-03", site_name="Miami FL", console=FALSE, 
	hur_path=hur_path), test.expected, style="serialize", cran=FALSE)

# Test hurrecon_summarize_land_water
test.expected <- system.file("region", "land_water_summary.expected", package="HurreconR", mustWork=TRUE)
hur_path <- system.file("", package="HurreconR", mustWork=TRUE)
expect_snapshot_value(hurrecon_summarize_land_water(console=FALSE, hur_path=hur_path), test.expected, 
	style="serialize", cran=FALSE)

# Test hurrecon_model_region
test.expected <- system.file("region", "AL1935-03.tif", package="HurreconR", mustWork=TRUE)
hur_path <- system.file("", package="HurreconR", mustWork=TRUE)
expect_snapshot_value(hurrecon_model_region(hur_id="AL1935-03", save=FALSE, console=FALSE,
	hur_path=hur_path), test.expected, style="serialize", cran=FALSE)

