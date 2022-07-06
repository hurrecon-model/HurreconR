## Resubmission
This is a resubmission.

## Updates
The DESCRIPTION file was updated to remove extra spaces 
and to add two references with DOIs.

Messages are now sent to the console with message rather than cat.

Output to the console can be turned off with the console parameter.

The user's par values are restored with the on.exit function.
No changes are made to the user's options or working directory.

Examples, tests, and vignettes do not write to the user's home filespace.

Model functions do not write to the user's home filespace by default.
The user must first specify a path for the current set of model runs
using the hurrecon_set_path function or the optional hur_path parameter.
This path is stored in its own environment (hur_env). If this path is not
set, model functions stop with a message asking the user to set the path.

Output to file can be turned off with the save parameter.
If save is FALSE, intermediate results (if any) are written to the 
R session temporary directory, while final results are returned
but not saved to file.

## R CMD check results
There were no ERRORS or WARNINGS.
There was 1 NOTE: New submission.

## Test environments
* local Windows 10, R 4.2.0
* GitHub Actions: mac, windows, ubuntu

