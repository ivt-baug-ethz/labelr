
<!-- README.md is generated from README.Rmd. Please edit that file -->
labelr
======

The goal of labelr is to make labelling data frames easier. Think of using different labels in `ggplot2` because of different languages or because of a different audience. Or just because you want to keep things more dynamic.

Installation
------------

You can install it like this (not on CRAN):

``` r
devtools::install_github("tinino/labelr")
```

How to use `labelr`
-------------------

Let's take `iris`. We have `Species` as factor.

``` r
str(iris)
#> 'data.frame':    150 obs. of  5 variables:
#>  $ Sepal.Length: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
#>  $ Sepal.Width : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
#>  $ Petal.Length: num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
#>  $ Petal.Width : num  0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
#>  $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
```

Now assume you want to change these values, i.e. give it a new label (maybe translate to different languages or whatever). Here, we want to change labels to titlecase. Define a data frame with a column named `from` and a column named `to`:

``` r
(species_from_to <- data.frame(
  from = levels(iris$Species), 
  to = stringr::str_to_title(levels(iris$Species))
))
#>         from         to
#> 1     setosa     Setosa
#> 2 versicolor Versicolor
#> 3  virginica  Virginica
```

Now set the labels. For convencience, make sure your entry is named like the variable (attention: `Species` is titlecase!):

``` r
labelr::labels$set(Species = species_from_to)
```

Now label `iris`:

``` r
labelr::label_df(iris)[c(1:3, 51:53, 101:103), ]
#>     Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
#> 1            5.1         3.5          1.4         0.2     Setosa
#> 2            4.9         3.0          1.4         0.2     Setosa
#> 3            4.7         3.2          1.3         0.2     Setosa
#> 51           7.0         3.2          4.7         1.4 Versicolor
#> 52           6.4         3.2          4.5         1.5 Versicolor
#> 53           6.9         3.1          4.9         1.5 Versicolor
#> 101          6.3         3.3          6.0         2.5  Virginica
#> 102          5.8         2.7          5.1         1.9  Virginica
#> 103          7.1         3.0          5.9         2.1  Virginica
```

Now immagine you want to be able to switch labels using other distinctive parameters, like language. Here, an example might be uppercase as a second representation. To achieve this, just add another column, for example `casetype`:

``` r
(species_from_to_titlecase <- data.frame(
  from = levels(iris$Species), 
  to = stringr::str_to_title(levels(iris$Species)),
  casetype = "titlecase"
))
#>         from         to  casetype
#> 1     setosa     Setosa titlecase
#> 2 versicolor Versicolor titlecase
#> 3  virginica  Virginica titlecase
# set that (hence replace previous):
labelr::labels$set(Species = species_from_to_titlecase)

# and add uppercase:
(species_from_to_uppercase <- data.frame(
  from = levels(iris$Species), 
  to = toupper(levels(iris$Species)),
  casetype = "uppercase"
))
#>         from         to  casetype
#> 1     setosa     SETOSA uppercase
#> 2 versicolor VERSICOLOR uppercase
#> 3  virginica  VIRGINICA uppercase

# append this (makes it also new default):
labelr::labels$append(Species = species_from_to_uppercase)
```

Now, default is `uppercase`:

``` r
labelr::label_df(iris)[c(1:3, 51:53, 101:103), ]
#>     Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
#> 1            5.1         3.5          1.4         0.2     SETOSA
#> 2            4.9         3.0          1.4         0.2     SETOSA
#> 3            4.7         3.2          1.3         0.2     SETOSA
#> 51           7.0         3.2          4.7         1.4 VERSICOLOR
#> 52           6.4         3.2          4.5         1.5 VERSICOLOR
#> 53           6.9         3.1          4.9         1.5 VERSICOLOR
#> 101          6.3         3.3          6.0         2.5  VIRGINICA
#> 102          5.8         2.7          5.1         1.9  VIRGINICA
#> 103          7.1         3.0          5.9         2.1  VIRGINICA
```

But we can also ask for titlecase:

``` r
labelr::label_df(iris, casetype = "titlecase")[c(1:3, 51:53, 101:103), ]
#>     Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
#> 1            5.1         3.5          1.4         0.2     Setosa
#> 2            4.9         3.0          1.4         0.2     Setosa
#> 3            4.7         3.2          1.3         0.2     Setosa
#> 51           7.0         3.2          4.7         1.4 Versicolor
#> 52           6.4         3.2          4.5         1.5 Versicolor
#> 53           6.9         3.1          4.9         1.5 Versicolor
#> 101          6.3         3.3          6.0         2.5  Virginica
#> 102          5.8         2.7          5.1         1.9  Virginica
#> 103          7.1         3.0          5.9         2.1  Virginica
```

Note, that the levels are still the same and will remain in the factor-order, if you specify it as a factor. If you however label a character value, it will return a factor according to the order you set in `labels$set()`. This is nice for use with, for example, `ggplot2`, where you might want to achieve a specific default order.
