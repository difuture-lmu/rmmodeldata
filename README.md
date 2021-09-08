
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rmmodeldata: Remove data from models

## Overview

Building models is an essential skill for scientists or practioneers
when working with data. In some research areas, such as it is for
medical data, contains sensitive information which should not get
shared. However, most `R` models, such as `lm`, contains the original
data used for training the model.

This package provides mechanisms to train algorithms without storing the
data or removing model data from a model. For simpler model objects
(e.g. `lm`) it is sufficient to remove parts from the object to ensure
privacy. More complex algorithms (e.g. `partykit::cforest`), on the
other side, requires adaptions of the fitting and prediction algorithm
to provide full privacy and functionality without storing data.

The functions you get by installing this package are: - A generic
`rmdata` method for simpler models to just remove data - Functions to
train and predict more complex models. Available algorithms: - `cforest`
- `traforest`

Using a more complex function overwrites the original one and returns
the same object with a suffix `.nodata`. For example, training a model
using `partykit::cforest` returns an object of class `cforest.nodata`.
For this class, special predict methods are implemented and used instead
of the original `partykit:::predict.cforest` method.

## Search for values (WIP)

In order to detect data in an `R` object, `rmmodeldata` provides a
function `searchObjectForValue` which searches through an object for a
specific value.

**Examples:**

``` r
vfound = searchObjectForValue(iris, iris[100, 2])
cat(vfound$found, sep = "\n")
#> iris[["Sepal.Width"]][[55]]
#> iris[["Sepal.Width"]][[56]]
#> iris[["Sepal.Width"]][[72]]
#> iris[["Sepal.Width"]][[74]]
#> iris[["Sepal.Width"]][[77]]
#> iris[["Sepal.Width"]][[100]]
#> iris[["Sepal.Width"]][[115]]
#> iris[["Sepal.Width"]][[122]]
#> iris[["Sepal.Width"]][[123]]
#> iris[["Sepal.Width"]][[127]]
#> iris[["Sepal.Width"]][[129]]
#> iris[["Sepal.Width"]][[131]]
#> iris[["Sepal.Width"]][[133]]
#> iris[["Sepal.Width"]][[134]]
```

``` r
mod = lm(Sepal.Length ~ ., data = iris)
vfound = searchObjectForValue(mod, iris[100, 2])
cat(vfound$found, sep = "\n")
#> mod[["model"]][["Sepal.Width"]][[55]]
#> mod[["model"]][["Sepal.Width"]][[56]]
#> mod[["model"]][["Sepal.Width"]][[72]]
#> mod[["model"]][["Sepal.Width"]][[74]]
#> mod[["model"]][["Sepal.Width"]][[77]]
#> mod[["model"]][["Sepal.Width"]][[100]]
#> mod[["model"]][["Sepal.Width"]][[115]]
#> mod[["model"]][["Sepal.Width"]][[122]]
#> mod[["model"]][["Sepal.Width"]][[123]]
#> mod[["model"]][["Sepal.Width"]][[127]]
#> mod[["model"]][["Sepal.Width"]][[129]]
#> mod[["model"]][["Sepal.Width"]][[131]]
#> mod[["model"]][["Sepal.Width"]][[133]]
#> mod[["model"]][["Sepal.Width"]][[134]]
```

## Disclaimer

  - It is suggested to use the namespace `rmmodeldata` for calling
    functions to ensure the usage of the correct method. A message is
    printed when a method of `rmmodeldata` is used. Otherwise, it cannot
    be distinguished between the original and the new method.

  - Searching through objects ignores functions and environments. Thus,
    if data are stored within an environment it is not detected.

## Installation

#### Developer version:

``` r
remotes::install_github("difuture-lmu/rmmodeldata")
```

## Example

``` r
set.seed(31415)
idx_train = sample(x = c(TRUE, FALSE), size = nrow(iris), replace = TRUE, prob = c(0.8, 0.2))

dat_train = iris[idx_train, ]
dat_test  = iris[! idx_train, ]

set.seed(31415)
mod_without_data = rmmodeldata::cforest(Petal.Length ~ ., data = dat_train)
#> [2021-09-08 14:13:30] Using `rmmodeldata::cforest``
pred_without_data = predict(mod_without_data, newdata = dat_test)
#> [2021-09-08 14:13:32] Using `rmmodeldata::predict.cforest.nodat`

str(pred_without_data)
#>  Named num [1:36] 1.49 1.46 1.42 1.96 1.52 ...
#>  - attr(*, "names")= chr [1:36] "1" "9" "10" "15" ...
```
