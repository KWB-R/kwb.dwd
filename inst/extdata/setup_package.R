# Set the name for your new package
package <- "kwb.dwd"

# Set the path to your new package
pkg_dir <- file.path("~/HAUKE/R", package)

# Create a default package structure
withr::with_dir(pkg_dir, {kwb.pkgbuild::use_pkg_skeleton(package)})

#kwb.orcid::get_kwb_orcids()

author <- list(
  name = "Hauke Sonnenberg",
  orcid = "0000-0001-9134-2871"
)

collapse = description <- list(
  name = package,
  title = "Access Information from Deutscher Wetterdienst (DWD)",
  desc  = paste(
    collapse = "\n",
    "This package provides functions to simplify the access to the data",
    "provided online by the Deutscher Wetterdienst (DWD)."
  )
)

