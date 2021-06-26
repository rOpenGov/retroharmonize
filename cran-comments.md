## Test environments
* local win install, R 4.1.0
0 errors v | 0 warnings v | 0 notes 

### Github 
* Microsoft Windows Server 2019, window-latest (release)
* ubuntu-20.04 (release)
* ubuntu-20.04 (devel)
* Mac OS X 10.15.7 (release)
0 errors v | 0 warnings v | 0 notes 

### rhub
* Fedora Linux, R-devel, clang, gfortran, on r_hub
* Ubuntu Linux 20.04.1 LTS, R-release, GCC, on r_hub

## R CMD check results
There were no ERRORs or WARNINGs. 


## Submission 0.1.16

This is a minor release with very limited new functionality and a few bug fixes. We implemented some good coding practices and brought the non-standard evaluation in many places in line with the tidyverse 1.0+ dependencies.

The novelty of the package is the new vignette working with Arab Barometer survey files. We are improving the code quality and documentation by applying the software to newer data sources, which we document in the vignettes.  The vignettes are only partly evaluated on CRAN, with a very small subset of the data used.  We do not have the rights to republish the original datasets, and it would be also very impracitcal because of the amount of the data and the file sizes involved.

