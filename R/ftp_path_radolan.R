# MAIN -------------------------------------------------------------------------
if (FALSE)
{
  #kwb.dwd:::ftp_path_grids_germany("daily")

  ftp_path_radolan_all()
  ftp_path_radolan_all(use_placeholder = TRUE)

  # Kann ich alle Radolan-Daten bearbeiten?
  get_radolan_metadata("grids_germany/5_minutes/radolan/reproc/<year>_002/asc/<year>")
  get_radolan_metadata(url = "grids_germany/5_minutes/radolan/reproc/1999_002/asc/1998")

  ftp_path_radolan()
  ftp_path_radolan(resolution = "")
  ftp_path_radolan(resolution = "hourly")
  ftp_path_radolan(resolution = "5_minutes")
  ftp_path_radolan(resolution = "5_minutes", type = "a")
  ftp_path_radolan(resolution = "5_minutes", type = "historical")
  ftp_path_radolan(type = "historical", resolution = "hourly")
  ftp_path_radolan(type = "historical", resolution = "daily")

  ftp_path_radolan(type = "reproc")
  ftp_path_radolan(format = "netCDF")
}

# ftp_path_radolan -------------------------------------------------------------
ftp_path_radolan <- function(
    resolution = NULL,
    type = NULL,
    format = NULL
)
{
  `%>%` <- magrittr::`%>%`

  ### Code to generate the following "templates <- c(...)" assignment
  # ftp_path_radolan_all(use_placeholder = TRUE) %>%
  #   dQuote('"') %>%
  #   paste(collapse = ",\n  ") %>%
  #   sprintf(fmt = "templates <- c(\n  %s\n)") %>%
  #   cat()

  paths <- ftp_path_radolan_all()

  paths <- c(
    "grids_germany/5_minutes/radolan/reproc/<year>_002/asc/<year>",
    "grids_germany/5_minutes/radolan/reproc/<year>_002/asc/supplement",
    "grids_germany/5_minutes/radolan/reproc/<year>_002/bin/<year>",
    "grids_germany/5_minutes/radolan/reproc/<year>_002/bin/supplement",
    "grids_germany/5_minutes/radolan/reproc/<year>_002/netCDF/<year>",
    "grids_germany/daily/radolan/historical/bin/<year>",
    "grids_germany/daily/radolan/recent/bin",
    "grids_germany/hourly/radolan/historical/asc/<year>",
    "grids_germany/hourly/radolan/historical/asc",
    "grids_germany/hourly/radolan/historical/bin/<year>",
    "grids_germany/hourly/radolan/historical/bin",
    "grids_germany/hourly/radolan/recent/asc",
    "grids_germany/hourly/radolan/recent/bin",
    "grids_germany/hourly/radolan/reproc/<year>_003/asc/<year>",
    "grids_germany/hourly/radolan/reproc/<year>_003/bin/<year>",
    "grids_germany/hourly/radolan/reproc/<year>_002/asc/<year>",
    "grids_germany/hourly/radolan/reproc/<year>_002/asc/supplement",
    "grids_germany/hourly/radolan/reproc/<year>_002/bin/<year>",
    "grids_germany/hourly/radolan/reproc/<year>_002/bin/supplement",
    "grids_germany/hourly/radolan/reproc/<year>_002/netCDF/<year>"
  )

  metadata <- get_radolan_metadata(paths)

  #View(metadata)

  filter_metadata <- function(metadata, property, value) {
    if (is.null(value)) {
      return(metadata)
    }
    values <- kwb.utils::selectColumns(metadata, property)
    options <- kwb.utils::toNamedList(unique(values))
    metadata[values == kwb.utils::selectElements(options, value), ]
  }

  metadata <- filter_metadata(metadata, "resolution", value = resolution)
  metadata <- filter_metadata(metadata, "type", type)
  metadata <- filter_metadata(metadata, "format", format)

  kwb.utils::selectColumns(metadata, "url")
}

# ftp_path_radolan_all ---------------------------------------------------------
ftp_path_radolan_all <- function(use_placeholder = FALSE)
{
  `%>%` <- magrittr::`%>%`

  paths <- kwb.dwd::dwd_files$file %>%
    grep(pattern = "/radolan/", value = TRUE) %>%
    dirname() %>%
    unique()

  if (use_placeholder) {
    paths <- paths %>%
      gsub(pattern = "/\\d{4}(_|$)", replacement = "/<year>\\1") %>%
      unique()
  }

  paths
}

# get_radolan_metadata ---------------------------------------------------------
get_radolan_metadata <- function(url)
{
  pattern <- paste0(
    "grids_germany",
    "/([^/]+)", # 1. pair of parentheses: resolution
    "/radolan",
    "/([^/]+)", # 2. pair of parentheses: type
    "(/(<year>_\\d{3}))?", # 4. pair of parentheses: version (if any)
    "/([^/]+)", # 5. pair of parentheses: format
    "(/.*)?"
  )

  year_placeholder <- "<year>"

  if (!any(grepl(year_placeholder, url))) {
    pattern <- gsub(year_placeholder, "\\\\d{4}", pattern)
  }

  metadata <- kwb.utils::extractSubstring(pattern, url, index = c(
    resolution = 1L,
    type = 2L,
    version = 4L,
    format = 5L
  ))

  cbind(metadata, url = url)
}
