# select_shapes ----------------------------------------------------------------
select_shapes <- function(shapes)
{
  index <- select_shape(stats::setNames(shapes, basename(names(shapes))))

  shape <- shapes[[index]]

  variable <- select_variable(shape)
  shape_variable <- shape[, variable]

  pattern <- readline("pattern: ")

  result <- list(
    index = index,
    variable = variable,
    pattern = pattern
  )

  cat("Code to create this configuration:\n")
  cat("config <-", kwb.utils::objectToText(result))

  result
}

# select_shape -----------------------------------------------------------------
select_shape <- function(shapes)
{
  choices <- names(shapes)

  selection <- utils::select.list(choices, title = "Select layer")

  which(selection == choices)
}

# select_variable --------------------------------------------------------------
select_variable <- function(shape)
{
  first_values <- kwb.utils::excludeNULL(
    lapply(stats::setNames(nm = names(shape)), function(v) {
      values <- unique(shape[[v]])
      values <- values[! is.na(values)]
      if (length(values)) {
        head(values, 3L)
      }
    })
  )

  choices <- sapply(names(first_values), function(name) {
    x <- first_values[[name]]
    sprintf("%s (%s)", name, paste(x, collapse = ", "))
  })

  selection <- utils::select.list(choices, title = "Select variable")

  names(selection)
}
