## Test environments
* local win install,
* rhub  

## R CMD check results
This release is primarily a maintenance and compatibility update.
The refactoring was triggered by new automated checks introduced after 
the previous CRAN release (checks that did not exist at the time of 
the 2023 submission). These newer checks exposed several issues
related to: stricter namespace validation, S3 method documentation completeness,
handling of non-standard evaluation in dplyr pipelines,
example robustness under --run-donttest, vctrs type stability.

No breaking API changes. All public functions retain their existing 
interfaces. Deprecated aliases remain available with clear warnings.

There are two NOTES:

## NOTE 1

installed size is  5.7Mb
     sub-directories of 1Mb or more:
       doc        1.1Mb
       examples   1.9Mb

The size of the package did not increse since the last release on CRAN.  Because of the nature of the package (harmonizing real-life surveys) the
examples are relatively great in size (with permission, we use small subsets of actual Eurobarometer surveys.) This makes unit-testing and documenting with vignettes much easier and more realistic for the user.

## NOTE 2
lines wider than 90 characters:
S3 method for class 'retroharmonize_labelled_spss_survey.retroharmonize_labelled_spss_survey 
due to the syntax of the method definition extends beyond 90 characters.