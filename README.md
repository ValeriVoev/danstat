
<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/ValeriVoev/danstat.svg?branch=master)](https://travis-ci.org/ValeriVoev/danstat)
[![CRAN
status](https://www.r-pkg.org/badges/version/danstat)](https://CRAN.R-project.org/package=danstat)
![Since when](https://www.r-pkg.org/badges/ago/danstat)
<!-- ![Downloads](https://cranlogs.r-pkg.org/badges/danstat) -->
<!-- ![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/danstat) -->
<!-- badges: end -->

## Installation

From CRAN:

``` r
install.packages("danstat")
```

From Github:

``` r
# install.packages("devtools")
devtools::install_github("ValeriVoev/danstat")
```

## Overview

The `danstat` package provides an R interface to Danmarks Statistik
Statistikbank API to enable an easier access to the wealth of data in
the data bank for research and the general community. The documentation
of the API can be found here: [Statistikbank
API](https://www.dst.dk/en/Statistik/statistikbanken/api).

## Usage

The API has 4 endpoints which are mimicked by four main functions of the
package:

1.  `get_subjects()` (SUBJECTS endpoint) retrieves information about
    subjects around which the data tables in the data bank are
    organized. The subjects are arranged hierarchically highest level
    like “Population and elections”, “Living conditions”, etc.
    `get_subjects()` retrieves the highest level of the hierarchy. See
    the function documentation for more details.
2.  `get_tables()` (TABLES endpoint) retrieves a list of tables
    associated with a given subject code. For example
    `get_tables(subjects = "02")` retrieves all tables related to the
    subject “Population and elections” with table id, description,
    variables in the table, etc.
3.  `get_table_metadata()` (TABLEINFO endpoint) returns information
    about a particular table - description, time of last update, whether
    or not it is actively updated, and most importantly (for practical
    purposes) the variable names and id’s which are needed whenever you
    request actual data from the table. Set `variables_only = TRUE` if
    you only want to get information on the table variables.
4.  `get_data()` (DATA endpoint) - returns data from a selected table.
    It is required to include a `variables` argument as a list. Each
    element of the list should itself be a named list (with elements
    `code` and `values`) where `code` is the variable id for which data
    is requested, and `values` is a vector of values for this variable.
    If all values are requested, specify `values = NA`. For example:

<!-- end list -->

``` r
library(danstat)
user_input = list(list(code = "ieland", values = c(5100, 5128)),
                  list(code = "køn", values = c(1,2)),
                  list(code = "tid", values = NA))

get_data(table_id = "folk1c", variables = user_input)
#> # A tibble: 192 x 4
#>   IELAND  KØN   TID    INDHOLD
#>   <chr>   <chr> <chr>    <dbl>
#> 1 Denmark Men   2008Q1 2465810
#> 2 Denmark Men   2008Q2 2466036
#> 3 Denmark Men   2008Q3 2467712
#> 4 Denmark Men   2008Q4 2469977
#> 5 Denmark Men   2009Q1 2470457
#> # … with 187 more rows
```

Note that while default language is set to English and variable values
are indeed returned in english, e.g. “Men”, column names are returned in
Danish, e.g. “KØN”, “INDHOLD”, etc. Unfortunately, the API doesn’t
currently provide an option to return column names (variable names) in
English. However, you can get the English translation using
`get_table_metadata`. For example, for the above table

``` r
library(dplyr)
get_table_metadata(table_id = "folk1c", variables_only = TRUE) %>% 
    select(id, text)
#>         id              text
#> 1   OMRÅDE            region
#> 2      KØN               sex
#> 3    ALDER               age
#> 4 HERKOMST          ancestry
#> 5   IELAND country of origin
#> 6      Tid              time
```

we can see that “Område” translates to “region”, “Køn” to “sex”, “Alder”
to “age”, etc. “Indhold” is always the “value” column whenever data is
returned with the `get_data` function.

## See also

There are (as far as I know) two other packages with similar
functionality:

  - <https://github.com/rOpenGov/dkstat>
  - <https://github.com/mikkelkrogsholm/statsDK>

In the packages above, the API is called with a `GET` request, while
`POST` is the prefrerred option of the API developers and is also what
is used in this package. Also, I think that using `POST` requests makes
the package code more readable compared to the long url-encoded queries
needed for `GET` requests. Also, as of this moment, the rOpenGov package
seems to not have been maintained for the past 3 years. In any case,
users can consider the above 2 packages as alternatives to this one.
