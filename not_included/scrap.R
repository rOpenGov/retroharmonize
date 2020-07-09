


this <- read_spss ( system.file("examples", "iris.sav", package = "haven"),
                    id = 'iris1')

that <- read_spss ( system.file("examples", "iris.sav", package = "haven"),
                    id = 'iris2')


concatenate(this$Species, that$Species)



rbind ( this, that)

na_labels <- structure(9999, names = "missing")
x <- that$Species
y <- this$Species

concatenate(x , y)

convert_to_labelled_spss ( 1:12)


library(eurobarometer)

data ("ZA6863_sample")

View ( ZA6863_sample )

data ("ZA_sample")
data ( "ZA7576_sample", package= "eurobarometer")

sapply ( ZA6863_sample, class )

x <- ZA6863_sample$p1

converter <- function(x) {

  if (any(
    c("numeric", "character", "haven_labelled_spss") %in% class( x ))
  ) {
    x
  } else {
    convert_to_labelled_spss (x)
  }

}

tested <- ZA6863_sample %>%
  dplyr::mutate_all ( .f = converter )
tested2 <- ZA7576_sample  %>%
  dplyr::mutate_all ( .f = converter )

aa <- tested %>%
  select ( any_of ( names(tested2 )))

bb <- tested2 %>%
  select (any_of(names(tested)))

rbind ( aa, bb)

rlang::last_error()
