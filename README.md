
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- # rearrr <a href='https://github.com/LudvigOlsen/rearrr'><img src='man/figures/rearrr_logo_242x280_250dpi.png' align="right" height="140" /></a> -->

# rearrr

**Rearrrange Data**  
**Authors:** [Ludvig R. Olsen](http://ludvigolsen.dk/) (
<r-pkgs@ludvigolsen.dk> ), Hugh Benjamin Zachariae <br/> **License:**
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

  - **Position** the min/max value and have values increase/decrease
    around
it.

### Main functions

| Function          | Description                                                            |
| :---------------- | :--------------------------------------------------------------------- |
| `center_max()`    | Center the highest value with values decreasing around it.             |
| `center_min()`    | Center the lowest value with values increasing around it.              |
| `position_max()`  | Position the highest value with values decreasing around it.           |
| `position_min()`  | Position the lowest value with values increasing around it.            |
| `pair_extremes()` | Arrange values as highest, lowest, second highest, second lowest, etc. |

## Table of Contents

<!-- ```{r toc, echo=FALSE} -->

<!-- rearrr:::render_toc("README.Rmd") -->

<!-- ``` -->

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

# Examples

## Attach packages

``` r
library(rearrr)
library(knitr)        # kable()
library(dplyr)        # %>% arrange()
```
