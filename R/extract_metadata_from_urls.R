# extract_metadata_from_urls ---------------------------------------------------
#' @importFrom kwb.utils extractSubstring
extract_metadata_from_urls <- function(urls, columns = NULL)
{
  base_names <- basename(urls)

  result <- kwb.utils::extractSubstring(
    "(\\d{4})(\\d{2})", base_names, c(year = 1L, month = 2L)
  )

  result$year <- as.integer(result$year)
  result$month <- as.integer(result$month)
  result$file <- base_names
  result$origin <- dirname(urls)

  if (is.null(columns)) {
    return(result)
  }

  kwb.utils::selectColumns(result, columns)
}
