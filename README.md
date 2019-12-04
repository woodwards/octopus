# octopus

The aim of `library(octopus)` is to make a few of my commonly-used utility functions available to colleagues.

### Installation

```r
devtools::install_github("woodwards/octopus")
```

### Functions

* `%notin%` is a companion for `%in%`.

* `as_numeric()` is the same as `as.numeric` but suppressing the warnings.

* `is_loaded()` checks whether a package is loaded.

* `ensnakeify()` converts strings to snake_case. Symbols are converted to underscore, but you can optionally convert them to text, by supplying a named vector such as `c("%" = "percent", "/" = "per", "@" = "at", "#" = "hashtag")`.

* `autosnake()` converts dataframe names to snake_case using `ensnakeify()`.

* `excel_to_date()` converts Excel date numbers to Date.

* `dotty()` quickly visualises dataframe contents using ``ggplot()`. 
