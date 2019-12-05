# octopus

The aim of `library(octopus)` is to make a few of my commonly-used utility functions available to colleagues.

### Installation

```r
devtools::install_github("woodwards/octopus")
```

### Functions

* `%notin%` is a companion for `%in%`.

* `as_numeric()` is the same as `as.numeric` but suppressing the warnings.

* `autosnake(df)` converts dataframe names to snake_case using `ensnakeify()`.

* `despace()` removes factors, space and NA from a datafram prior to writing.

* `dotty(df)` quickly visualises dataframe contents using ``ggplot()`. 

* `ensnakeify()` converts strings to snake_case. Symbols are converted to underscore, but you can optionally convert them to text, by supplying a named vector such as `c("%" = "percent", "/" = "per", "@" = "at", "#" = "hashtag")`.

* `excel_to_date()` converts Excel date numbers to Date.

* `fillgaps()` fills gaps in a vector with the first non-NA value.

* `is_loaded()` checks whether a package is loaded.

* `not_all_na()` is a helper function to identify dataframe columns that contain values.

* `not_all_same()` is a helper function to identify dataframe columns that contain more than one value.

* `summaree(df)` is a modification of `summary()` that gives more information on string columns.

* `write_data(df)` writes a dataframe to a text file (but only if it has changed).


