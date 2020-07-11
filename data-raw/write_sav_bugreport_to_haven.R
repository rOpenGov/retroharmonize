
### Read and change to missing labelled -----
iris_read <- haven::read_spss(
  system.file("examples", "iris.sav", package = "haven"))
iris$Species[1] <- 8; iris$Species[2] <- 9
na_values <- 8:9
names (na_values) <- c("unidentified", "missing")
labelled::na_values(iris$Species) <- na_values 
inherits(iris$Species, "haven_labelled_spss")
attr(iris$Species, "na_values")

### Save it ------------------------------------
temp_dir <- tempdir()
haven::write_sav(iris, 
                 path = file.path(temp_dir, "re-iris.sav")
)

### Re-read it ---------------------------------
re_iris <- haven::read_sav(
  file.path("temp_dir, "re-iris.sav"))

inherits(re_iris$Species, "haven_labelled_spss")
attr(re_iris$Species, "na_values")
  
  
  