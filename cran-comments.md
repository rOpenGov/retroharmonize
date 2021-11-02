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
* 

## R CMD check results
There were no ERRORs or WARNINGs. 

There is one NOTE:

installed size is  5.7Mb
     sub-directories of 1Mb or more:
       doc        1.1Mb
       examples   1.9Mb

The size of the package did not increse since the last release on CRAN.  Because of the nature of the package (harmonizing real-life surveys) the
examples are relatively great in size (with permission, we use small subsets of actual Eurobarometer surveys.) This makes unit-testing and documenting with vignettes much easier and more realistic for the user.