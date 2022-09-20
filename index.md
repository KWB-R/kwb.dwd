[![R-CMD-check](https://github.com/KWB-R/kwb.dwd/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/kwb.dwd/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/kwb.dwd/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/kwb.dwd/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/kwb.dwd/branch/main/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.dwd)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.dwd)]()
[![R-Universe_Status_Badge](https://kwb-r.r-universe.dev/badges/kwb.dwd)](https://kwb-r.r-universe.dev/)

This package provides functions to simplify the access to the data 
provided online by the Deutscher Wetterdienst (DWD, https://www.dwd.de).
It currently contains a function \code{get_radolan_urls} that returns the
internet addresses (Unified Resource Locators, URLs) to the zip files 
containing rain data from the RADOLAN system in daily or hourly resolution
(see https://www.dwd.de/DE/leistungen/radolan/radolan.html). The function
\code{download_radolan} helps you to download these files. But take care,
you should rescrict the time period as each file contains the data for 
all of Germany.

## Installation

For installing the latest release of this R package run the following code below:

```r
# Enable repository from kwb-r
options(repos = c(
  kwbr = 'https://kwb-r.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))

# Download and install kwb.dwd in R
install.packages('kwb.dwd')

# Browse the kwb.dwd manual pages
help(package = 'kwb.dwd')
```
