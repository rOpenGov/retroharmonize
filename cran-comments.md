## Test environments
* local R installation on Windows 10, R 4.0.2
* ubuntu 16.04 (on travis-ci), R 4.0.0
* win-builder (Windows Server 2008 R2 SP1, R-release, 32/64 bit on rhub and windows-x86_64-devel on rhub)
* Fedora, Debian, Windows NT on r_hub.

## R CMD check results

0 errors | 0 warnings | 0 note

* This is a re-submission of 0.1.9

We changed all external links in the package documentation where the http:// protocol was used to https://  This affected the Afrobarometer, Eurobarometer vignettes, the README, NEWS and the DESCRIPTION files.


* In 0.1.8 there was an errorneous badge link in the REAMDE.Rmd which is removed:
 From: README.md
       Status: 404
       Message: Not Found
       
* The canonical URL of the CRAN page for a package is
       https://CRAN.R-project.org/package=pkgname
       
https://CRAN.R-project.org/package=labelled was inserted to the labelled_spss_survey.Rmd and .html file.

* In 0.1.8 "You do not need the LICENSE; text, "+ file LICENSE" is only needed for additional restrictions such as attribution requirements." -> This was removed, too.