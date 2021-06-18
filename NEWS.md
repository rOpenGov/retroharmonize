# retroharmonize 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* Class definition and description.

# retroharmonize 0.1.5
* retroharmonize 0.1.1-0.1.5 are not intended for release, they contain numerous development stages of a new package.

# retroharmonize 0.1.6
* Finalizing the documentation and examples for a release candidate.

# retroharmonize 0.1.7
* Reduce the examples to smaller sizes, re-work the vignettes to work with smaller examples.

# retroharmonize 0.1.8
* Citation information for the [Afrobarometer](https://retroharmonize.satellitereport.com/articles/afrobarometer.html) vignette and disclaimer in [README](https://retroharmonize.satellitereport.com/index.html)
* Harmonize waves correctly handles some exceptions.
* harmonize_values throws a meaningful error when the harmonization list is inconsistent.

# retroharmonize 0.1.9
* This is a re-submission to CRAN after removing two wrong links from the documentation and an unnecessary LICENSE file.

# retrharmonize 0.1.10
* We changed all external links in the package documentation where the http:// protocol was used to https://  This affected the Afrobarometer, Eurobarometer vignettes, the README, NEWS and the DESCRIPTION files.
* This is probably a CRAN updating issue, as the package is there, but we removed the link to on the safe side.
  URL: https://CRAN.R-project.org/package=labelled
    From: inst/doc/labelled_spss_survey.html
    Status: 404
    Message: Not Found
    
# retroharmonize 0.1.11
Further issues with URLs, hopefully all resolved.

# retroharmonize 0.1.12
* In the examples, dontrun{} replaced by donttest{}.  
* Further clarity given in comment to the Eurobarometer vignette that some part of the code is not evaluated, as it shows how to work with a large number of files from GESIS that we have no permission to re-publish.  We show with examples how the user, after gaining permission from GESIS to download the files, and placing it into her working environment can efficiently subset these files.

# retroharmonize 0.1.13
* In the examples that use file operations, dontrun{} replaced by donttest{}. This is the first released version on CRAN.

# retroharmoinze 0.1.14
* harmonize_values now accepts perl-like regex.

# retroharmoinze 0.1.15
* Documentation improvements.
* A new function, harmonize_var_names(), helps to systematically rename variables in a list of surveys. Conventional names to keep are returned by suggest_permanent_names(), and the helper function suggest_var_names() provides a simple wrapper around var_label_normalize() and suggest_permanent_names().
* A new function, subset_waves(), helps to subset a wave of surveys.
* With the help of [goodpractice](https://github.com/mangothecat/goodpractice), some coding good practices are introduced.

# retroharmoinze 0.1.16
* Arab Barometer tutorial vignette.
* Transition to tidyverse 1.0+ with using the rlang .data pronoun, etc.