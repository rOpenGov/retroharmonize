## Test environments
* local R installation on Windows 10, R 4.0.0
* ubuntu 16.04 (on travis-ci), R 4.0.0
* win-builder (Windows Server 2008 R2 SP1, R-release, 32/64 bit, 
Windows Server 2008 R2 SP1, R-devel, 32/64 bit on rhub)
* Fedora, Debian, Windows NT on r_hub.

## R CMD check results

0 errors | 0 warnings | 0 note

A note to the reviewer:

* This is a first release candidate. In many ways, this packge is an
extension of haven and and labelled (from tidyverse) for survey data, and augments the haven class system, which is in itself not complete. During development we found bugs in haven file I/O (reported). To make sure we avoid those pitfalls, in three subdirectories of inst/ there are several files, below the 1000k limit each, but maybe substantial compared to other packages.
* For the same reason, the unit tests take, 83 of them a bit longer than normal to take, because they test a lot of file I/O functionality.
* We hope that in the near future the parallel issues with haven / labelled / retroharmonize can be resolved and a very soon update of the package will not only be more functional but lighter in the burden of inst/ files and unit tests.


