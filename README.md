[![R-CMD-check](https://github.com/KWB-R/kwb.dwd/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/kwb.dwd/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/kwb.dwd/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/kwb.dwd/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/kwb.dwd/branch/main/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.dwd)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.dwd)]()
[![R-Universe_Status_Badge](https://kwb-r.r-universe.dev/badges/kwb.dwd)](https://kwb-r.r-universe.dev/)

# kwb.dwd

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

For details on how to install KWB-R packages checkout our [installation tutorial](https://kwb-r.github.io/kwb.pkgbuild/articles/install.html).

```r
### Optionally: specify GitHub Personal Access Token (GITHUB_PAT)
### See here why this might be important for you:
### https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat

# Sys.setenv(GITHUB_PAT = "mysecret_access_token")

# Install package "remotes" from CRAN
if (! require("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

# Install KWB package 'kwb.dwd' from GitHub
remotes::install_github("KWB-R/kwb.dwd")
```

## Documentation

Release: [https://kwb-r.github.io/kwb.dwd](https://kwb-r.github.io/kwb.dwd)

Development: [https://kwb-r.github.io/kwb.dwd/dev](https://kwb-r.github.io/kwb.dwd/dev)
