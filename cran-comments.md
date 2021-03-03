## Test environments

* GitHub Actions (ubuntu-16.04): devel, release, oldrel, 3.5, 3.4, 3.3
* GitHub Actions (windows): release, oldrel
* GitHub Actions (macOS): release
* win-builder: devel

## R CMD check results

0 errors | 0 warnings | 0 note

Submission 0.1.13

* We left a \dontrun{} in harmonize_waves() and merge_waves() changed to \donttest{} -> apologies. These examples use files, which take some time to execute, and on local tests they run flawlessly.

However, we still have not exectuted elements in the vignettes.  Our package was designed to work with international survey programs, and currently we show (with the explicit consent of the survey program managers / archivists) how to work with their files.  But we have no right to re-publish those files and because access to the files have different conditions, we cannot offer a programatic, reproducible access to the actual Eurobarometer and Afrobarometer files. (In our next release, we will also include Arab Barometer with similar conditions.) 

We did, however, get permission to re-publish small parts of realistic data to create examples and unit-tests our functions. These examples are not very small by nature themselves. They were originally in the \dontrun{} section and now these are in the \donttest{} category, because they require file I/O operations. In the unit tests, again, with not testing on CRAN, these examples are checked, so they work reliably.

Our vignettes chunks that show a realistic scenario - the user went through the approval to access the data and downloaded it to her whatever working environment, presumably to a folder. 

## Comments are from earlier submissions below


Submission 0.1.12

* Thanks, please replace \dontrun{} by \donttest{} or unwap the examples if they can be executed in less than 5 sec per Rd-file -> DONE

* Please ensure that your functions do not modify (save or delete) the
user's home filespace in your examples/vignettes/tests. That is not
allow by CRAN policies. Please only write/save files if the user has
specified a directory. In your examples/vignettes/tests you can write to tempdir().

Reply: Our vignettes do not write or save files to any directory. To avoid confusion, we created a new example for subset_save_survey() function that uses tempdir()

Our not evaluated examples clearly states in comments that after having visited the GESIS website, gaining approval for the use of the files, we imagine that the user saved them to an imaginary "gesis_dir". Becuase the files are very large, we created a loop utility that reads in all the files from this imaginary directory, and subsets them, and saves them to the users imaginary working_directory, which we modified for more clarity in the non-evaluated example as working_directory <- tempdir()

We wanted to create a realistic example, but it is not fully possible to demonstrate the mass use of GESIS Eurobarometer files, because we have no permission for that. GESIS explicitly asked us to use minimal examples, and in our example code only 'mimic' the procedure.

We added several comments to highligth which code is not evaluated and serves only an illustration that must be run in the user's own environment.

subset_save_survey() is the only function in our package that saves something.  In the unit tests, we created an example when it saves to tempdir(), but because of time constraints, we skip this test on CRAN.


* This is a re-submission of 0.1.10 with hopefully correct URLs to Afrobarometer's website, which, by default, does not use https, but of course, can be browsed via the https protocol.

* This is a re-submission of 0.1.9

We changed all external links in the package documentation where the http:// protocol was used to https://  This affected the Afrobarometer, Eurobarometer vignettes, the README, NEWS and the DESCRIPTION files.

* In 0.1.8 there was an erroneous badge link in the REAMDE.Rmd which is removed:
 From: README.md
       Status: 404
       Message: Not Found
       
* The canonical URL of the CRAN page for a package is
       https://CRAN.R-project.org/package=pkgname
       
https://CRAN.R-project.org/package=labelled was inserted to the labelled_spss_survey.Rmd and .html file.

* In 0.1.8 "You do not need the LICENSE; text, "+ file LICENSE" is only needed for additional restrictions such as attribution requirements." -> This was removed, too.
