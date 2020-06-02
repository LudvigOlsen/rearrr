
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- # rearrr <a href='https://github.com/LudvigOlsen/rearrr'><img src='man/figures/rearrr_logo_242x280_250dpi.png' align="right" height="140" /></a> -->

# rearrr

**Rearrrange Data**  
**Authors:** [Ludvig R. Olsen](http://ludvigolsen.dk/) (
<r-pkgs@ludvigolsen.dk> ) <br/> **License:**
[MIT](https://opensource.org/licenses/MIT) <br/> **Started:** April
2020

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/rearrr)](https://cran.r-project.org/package=rearrr)
[![metacran
downloads](https://cranlogs.r-pkg.org/badges/rearrr)](https://cran.r-project.org/package=rearrr)
[![minimal R
version](https://img.shields.io/badge/R%3E%3D-3.2.0-6666ff.svg)](https://cran.r-project.org/)
[![Codecov test
coverage](https://codecov.io/gh/ludvigolsen/rearrr/branch/master/graph/badge.svg)](https://codecov.io/gh/ludvigolsen/rearrr?branch=master)
[![Travis build
status](https://travis-ci.org/LudvigOlsen/rearrr.svg?branch=master)](https://travis-ci.org/LudvigOlsen/rearrr)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/LudvigOlsen/rearrr?branch=master&svg=true)](https://ci.appveyor.com/project/LudvigOlsen/rearrr)
<!-- [![DOI](https://zenodo.org/badge/71063931.svg)](https://zenodo.org/badge/latestdoi/71063931) -->

## Overview

R package for rearranging data by a set of methods.

We distinguish between **rearrangers** and **mutators**, where the first
*reorders* the data points and the second *changes the values* of the
data
points.

### Main functions

| Function          | Description                                                            |
| :---------------- | :--------------------------------------------------------------------- |
| `center_max()`    | Center the highest value with values decreasing around it.             |
| `center_min()`    | Center the lowest value with values increasing around it.              |
| `position_max()`  | Position the highest value with values decreasing around it.           |
| `position_min()`  | Position the lowest value with values increasing around it.            |
| `pair_extremes()` | Arrange values as highest, lowest, second highest, second lowest, etc. |
| `closest_to()`    | Order values by shortest distance to a target.                         |
| `furthest_from()` | Order values by longest distance to a target.                          |
| `rev_windows()`   | Reverse order window-wise.                                             |
| `flip_values()`   | Flip the values around a center value.                                 |

## Table of Contents

  - [rearrr](#rearrr)
      - [Overview](#overview)
          - [Main functions](#main-functions)
      - [Installation](#installation)
      - [Attach packages](#attach-packages)
      - [Rearrangers](#rearrangers)
          - [Center min/max](#center-min/max)
          - [Position min/max](#position-min/max)
          - [Pair extremes](#pair-extremes)
          - [Closest to / furthest from](#closest-to-/-furthest-from)
          - [Reverse windows](#reverse-windows)
      - [Mutators](#mutators)
          - [Flip values](#flip-values)

## Installation

<!-- CRAN: -->

<!-- > `install.packages("rearrr")` -->

Development
version:

> `install.packages("devtools")`
> 
> `devtools::install_github("LudvigOlsen/rearrr")`

<!-- ## Vignettes   -->

<!-- `rearrr` contains a number of vignettes with relevant use cases and descriptions:   -->

<!-- > `vignette(package = "rearrr")` # for an overview    -->

## Attach packages

Let’s see some **examples**. We start by attaching the necessary
packages:

``` r
library(rearrr)
library(knitr)        # kable()
library(dplyr)        # %>% arrange()
library(tidyr)        # gather()
library(ggplot2)

xpectr::set_test_seed(1)
```

<!-- Note: The `kable()` function simply **formats** the output and is not required. -->

While we can use the functions with data frames, we showcase them with a
vector for simplicity. The functions work with grouped data frames and
in `magrittr` pipelines (`%>%`).

## Rearrangers

Rearrangers change the order of the data points.

### Center min/max

``` r
center_max(data = 1:10)
#>  [1]  1  3  5  7  9 10  8  6  4  2
```

``` r
center_min(data = 1:10)
#>  [1] 10  8  6  4  2  1  3  5  7  9
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="552" style="display: block; margin: auto;" />

### Position min/max

``` r
position_max(data = 1:10, position = 3)
#>  [1]  6  8 10  9  7  5  4  3  2  1
```

``` r
position_min(data = 1:10, position = 3)
#>  [1]  5  3  1  2  4  6  7  8  9 10
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="552" style="display: block; margin: auto;" />

### Pair extremes

``` r
pair_extremes(data = 1:10, keep_factor = TRUE)
#>    Value .pair
#> 1      1     1
#> 2     10     1
#> 3      2     2
#> 4      9     2
#> 5      3     3
#> 6      8     3
#> 7      4     4
#> 8      7     4
#> 9      5     5
#> 10     6     5
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="552" style="display: block; margin: auto;" />

### Closest to / furthest from

The target value/index can be passed as either a specific value or a
function.

``` r
closest_to(data = 1:10, target_fn = median)
#>  [1]  5  6  4  7  3  8  2  9  1 10
```

``` r
furthest_from(data = 1:10, target = 5)
#>  [1] 10  1  9  2  8  3  7  4  6  5
```

<img src="man/figures/README-unnamed-chunk-14-1.png" width="552" style="display: block; margin: auto;" />

### Reverse windows

``` r
rev_windows(data = 1:10, window_size = 3)
#>  [1]  3  2  1  6  5  4  9  8  7 10
```

<img src="man/figures/README-unnamed-chunk-16-1.png" width="552" style="display: block; margin: auto;" />

## Mutators

Mutators change the values of the data points.

### Flip values

``` r
# Set seed for reproducibility
xpectr::set_test_seed(1)

# Draw random numbers 
random_sample <- round(runif(10), digits=4)
random_sample
#>  [1] 0.2655 0.3721 0.5729 0.9082 0.2017 0.8984 0.9447 0.6608 0.6291 0.0618

# The median value to flip around
median(random_sample)
#> [1] 0.601

# Flip the random numbers around the median
flip_values(data = random_sample, center_fn = median)
#>  [1] 0.9365 0.8299 0.6291 0.2938 1.0003 0.3036 0.2573 0.5412 0.5729 1.1402
```

<img src="man/figures/README-unnamed-chunk-18-1.png" width="552" style="display: block; margin: auto;" />
