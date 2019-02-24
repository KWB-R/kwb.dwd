# clean_stop -------------------------------------------------------------------
clean_stop <- function(...)
{
  stop(..., call. = FALSE)
}

# get_element_or_stop ----------------------------------------------------------
get_element_or_stop <- function(x, element, name = deparse(substitute(element)))
{
  x[safe_element(element, names(x), name)]
}

# safe_element -----------------------------------------------------------------
safe_element <- function(element, elements, name = deparse(substitute(element)))
{
  if (! element %in% elements) clean_stop(sprintf(
    "%s ('%s') must be one of %s",
    name, element, kwb.utils::stringList(elements)
  ))

  element
}
