This update was triggered by new automated checks and stricter validation 
introduced after the previous CRAN release (notably around S3 method 
documentation, example execution, namespace imports, and vctrs/cli 
interactions).

The package codebase was already functional, but recent CRAN and R-hub 
checks exposed a number of issues that were not flagged in earlier (2023) 
environments.

In this release we:
- corrected and completed S3 method documentation and Rd usage sections,
- fixed examples and \donttest{} blocks to run cleanly under --run-donttest,
- removed unused imports and clarified implicit dependencies,
- added targeted unit tests to cover previously untested harmonization and import paths,
- addressed NOTES related to non-standard evaluation and vctrs-based methods.

The functionality of the package is unchanged; this update focuses on
robustness, documentation completeness, and compatibility with current
CRAN policies.

All checks pass locally, via GitHub Actions, and 
on R-hub (Linux, Windows, macOS).

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
The >90 character usage line is auto-generated for an exported S3 method with 
a long class name and cannot be shortened without breaking the public API; 
a shorter class name would hide the inheritance of this class.
retroharmonize_labelled_spss_survey.retroharmonize_labelled_spss_survey 
