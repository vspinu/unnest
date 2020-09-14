<!-- badges: start -->
[![R build status](https://github.com/vspinu/unnest/workflows/R-CMD-check/badge.svg)](https://github.com/vspinu/unnest/actions)
<!-- badges: end -->

## Description

`unnest` is a 0-dependency R package for very fast, single-copy and single-pass,
unnesting of hierarchical data structures. `unnest(..)` function takes a
hierarchical list as an input (json, xml etc.) and produces a `data.frame`
according to an unnesting spec.

## Installation

```R
install.packages("unnest")
# or
devtools::install_github("vspinu/unnest")
```
