# utility functions

#' Returns TRUE if x is not in y.
#'
#' @usage x \%notin\% y
#' @param x Anything.
#' @param y Anything.
#' @return Logical.
#' @examples
#' "a" %notin% c("b", "c")
#' @export
#' @rdname notin
"%notin%" <- function(x, y) !(x %in% y)

#' Convert to numeric while suppressing warnings.
#'
#' @param x A vector.
#' @param default A numeric scalar.
#' @return Numeric vector the same length as x.
#' @examples
#' as_numeric(c("1", "NA", NA, "b"))
#' @export
as_numeric <- function(x, default = NA_real_) {
  suppressWarnings(dplyr::if_else(is.na(as.numeric(x)), default, as.numeric(x)))
}

#' Check whether a package is loaded.
#'
#' @param package_name A string.
#' @return Logical.
#' @examples
#' is_loaded("dplyr")
#' @export
is_loaded <- function(package_name) {
  package_name %in% .packages()
}

#' Convert strings to snake_case.
#'
#' @param x A vector of strings.
#' @return replacements Named vector of replacements.
#' @examples
#' ensnakeify(c("Speed km/hr"), c("/" = "per"))
#' @export
ensnakeify <- function(x, replacements = c("%" = "pc")) {
  x %>%
    iconv(to = "ASCII//TRANSLIT") %>% # remove accents
    stringr::str_replace_na() %>% # convert NA to string
    stringr::str_to_lower() %>% # convert to lower case
    stringr::str_replace_all(setNames(stringr::str_c(" ", replacements, " "), names(replacements))) %>% # convert symbols
    stringr::str_replace_all(pattern = "[^[:alnum:]]", replacement = " ") %>% # convert remaining non-alphanumeric to space
    stringr::str_trim() %>% # trim leading and trailing spaces
    stringr::str_replace_all(pattern = "\\s+", replacement = "_") # convert remaining spaces to underscore
}

#' Convert dataframe names to snake_case.
#'
#' @param df A dataframe.
#' @return replacements Named vector of replacements.
#' @examples
#' library(magrittr)
#' data.frame("Speed km/hr" = 1:5, check.names = FALSE) %>% autosnake(c("/" = "per"))
#' @export
autosnake <- function(df, ...) { # to use in pipe
  names(df) <- ensnakeify(names(df), ...)
  df
}

#' Convert Excel date numbers to Date.
#'
#' @param excel A vector of excel date numbers.
#' @return A vector of dates.
#' @examples
#' excel_to_date(40000)
#' @export
excel_to_date <- function(excel) {
  as.Date(excel, origin = "1899-12-30")
}

#' Visualise dataframe contents using ggplot.
#'
#' @param df A dataframe.
#' @return A ggplot object.
#' @examples
#' library(magrittr)
#' data.frame(A = -10:10, B = NA, C = "C", stringsAsFactors = FALSE) %>% dotty() %>% print()
#' @export
dotty <- function(df) { # analyse a dataframe, return results as a dataframe
  dotty <- vector("list", ncol(df)) # loop through columns
  rows <- 1:nrow(df)
  for (i in 1:ncol(df)) {
    data <- df[[i]] # get a column
    if (typeof(data) == "logical") {
      type <- dplyr::case_when(is.na(data) ~ "NA", !data ~ "FALSE", TRUE ~ "TRUE")
    } else if (typeof(data) == "integer") {
      type <- dplyr::case_when(is.na(data) ~ "NA", data > 0 ~ "+int", data == 0 ~ "0int", TRUE ~ "-int")
    } else if (typeof(data) == "double") {
      type <- dplyr::case_when(is.na(data) ~ "NA", data > 0 ~ "+double", data == 0 ~ "0double", TRUE ~ "-double")
    } else if (typeof(data) == "character") {
      type <- dplyr::case_when(is.na(data) ~ "NA", TRUE ~ "string")
    } else {
      type <- dplyr::case_when(is.na(data) ~ "NA", TRUE ~ "unknown")
    }
    dotty[[i]] <- dplyr::data_frame(row = rows, column = i, type = type)
  }
  dplyr::bind_rows(dotty) %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(mapping = ggplot2::aes(x = column, y = row, colour = type)) +
    ggplot2::scale_y_reverse()
}
