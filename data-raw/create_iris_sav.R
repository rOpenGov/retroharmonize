library(labelled)
library(haven)

iris_file <- system.file("examples", "iris.sav", package = "haven")
iris <- haven::read_spss (iris_file )

random_rows <- round(runif (n = 10, min = 1, max = 150),0)
random_matrix <- matrix(
  c(c('117', '71', '19', '76', '55', '150', '121', '107', '27', '33'),
  c('16', '131', '122', '34', '105', '79', '67', '124', '120', '42'),
  c('6', '131', '66', '54', '78', '101', '109', '120', '11', '29')), 
  ncol = 10, 
  byrow = TRUE
)
i=1
for ( i in 1:3) {
  iris_copy <- iris
  random_rows <- as.integer(random_matrix[i,])
  missing_rows <- random_rows[random_rows %% 2== 0]
  unidentified_rows <- random_rows[random_rows %% 2 == 1]
  
  iris_copy$Sepal.Length[missing_rows] <- 990+i
  #iris_copy$Sepal.Width[missing_rows]  <- 990+1
  #iris_copy$Petal.Length[missing_rows] <- 990+1
  #iris_copy$Petal.Width[missing_rows]  <- 990+1
  iris_copy$Species[missing_rows] <- 8+i
  iris_copy$Species[unidentified_rows] <- 7+i
  
  na_values <- c(7:8+i)
  names (na_values) <- case_when (
    i == 1 ~ c("unidentified", "missing"), 
    i == 2 ~ c("UNKNOWN", "INAP"), 
    TRUE ~ c("unknown", "unobserved")
  ) 
  
  sepal_length_labels <- 990+i
  names(sepal_length_labels) <- case_when (
    i == 1 ~ "missing", 
    i == 2 ~ "missing", 
    TRUE ~ "unobserved"
  ) 
  
  iris_copy$Sepal.Length  <- haven::labelled_spss(
    x = unclass(iris_copy$Sepal.Length),
    labels =  sepal_length_labels,
    na_values = 990+i,
    label = "SEPAL LENGTH"
  )
  
  if ( i == 2 ) {
    iris_copy$Sepal.Width <- haven::labelled (
      x = unclass(iris_copy$Sepal.Width), 
      label = "SEPAL WIDTH"
    )
  }
  
  iris_copy$Species  <- haven::labelled_spss(
    x = c (unclass(iris_copy$Species)+i-1),
    labels =  c( labelled::val_labels(iris_copy$Species), na_values),
    label = paste0("IRIS SPECIES ", i)
  )
  
  labelled::na_values(iris_copy$Species) <- c(8+i,9+i)
  class(iris_copy$Species)
  
  haven::write_sav(iris_copy, 
                   path = file.path("inst", "examples", 
                                    paste0("iris",i,".sav"))
  )
  
}


re <- haven:::validate_sav (iris)

labelled::na_values ( iris_copy$Species)

re_iris <- haven::read_sav(
  file.path("inst", "examples", 
            paste0("iris",i,".sav")))

class(re_iris$Species)
