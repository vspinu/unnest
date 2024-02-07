<!-- badges: start -->
[![R-CMD-check](https://github.com/vspinu/unnest/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/vspinu/unnest/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/unnest)](https://CRAN.R-project.org/package=unnest)
<!-- badges: end -->

## Description

[unnest][] is a zero-dependency R package for a very fast single-copy and single-pass
unnesting of hierarchical data structures.

`unnest(..)` function takes a nested list as an input (`JSON`, `XML` etc.) and
produces a `data.frame` according to an unnesting spec.

## Installation

```R
install.packages("unnest")
# or
devtools::install_github("vspinu/unnest")
```

[unnest]: https://vspinu.github.io/unnest/
