
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
the same object with a suffix `.nodat`. For example, training a model
using `partykit::cforest` returns an object of class `cforest.nodat`.
For this class, special predict methods are implemented and used instead
of the original `partykit:::predict.cforest` method.

## Search for values (WIP)

In order to detect data in an `R` object, `rmmodeldata` provides a
function `searchObjectForValue` which searches through an object for a
specific value.

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
