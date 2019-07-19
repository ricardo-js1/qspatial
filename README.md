
qspatial
========

<!-- badges: start -->
<!-- badges: end -->
qspatial is a spatial statistics package for R which made with an user-friendly approach to allow users who are new to R or spatial statistics to visualize and analyze spatial data. The package utilizes spatstat and spdep functions to do the spatial analysis and ggplot2 to create all the resulting maps and plots.

The current version of the package has functions for point pattern and areal data. The package's functions uses what is considered a common methodology found in the literature, as an example, for areal data the functions automatically creates the neighborhood and the weight's matrix.

Installation
------------

You can install the released version of qspatial from github with:

``` r
# install.packages("devtools")
devtools::install_github("qspatialR/qspatial")
```

Example
-------
