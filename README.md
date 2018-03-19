
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggneuro R package

[![Travis-CI Build
Status](https://travis-ci.org/muschellij2/ggneuro.svg?branch=master)](https://travis-ci.org/muschellij2/ggneuro)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/muschellij2/ggneuro?branch=master&svg=true)](https://ci.appveyor.com/project/muschellij2/ggneuro)

The goal of `ggneuro` is to allow users to create plotting functions for
nifti objects. The initial impetus was for plotting brain images

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("muschellij2/ggneuro")
```

## Example

This is a basic example which shows you how to plot a T1-weighted image

``` r
library(kirby21.t1)
library(ggneuro)
library(neurobase)
#> Loading required package: oro.nifti
#> oro.nifti 0.9.4
t1_fname = readnii(get_t1_filenames()[1])
g = ggortho(t1_fname)
class(g)
#> [1] "gg"     "ggplot"
print(g)
```

<img src="man/figures/README-example-1.png" width="100%" />

``` r
## basic example code
```

Not sure if `plotly` will work with this well:

``` r
library(plotly)
ggplotly(g)
```
