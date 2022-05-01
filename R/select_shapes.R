# select_shapes ----------------------------------------------------------------
select_shapes <- function(shapes, files)
{
  index <- select_shape(stats::setNames(shapes, basename(files)))

  shape <- shapes[[index]]

  variable <- select_variable(shape)
  shape_variable <- shape[, variable]

  pattern <- readline("pattern: ")

  list(
    index = index,
    variable = variable,
    pattern = pattern
  )
}

# select_shape -----------------------------------------------------------------
select_shape <- function(shapes)
{
  choices <- names(shapes)

  selection <- utils::select.list(choices, title = "Select shape file")

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

  selection <- utils::select.list(choices, title = "Select Variable")

  names(selection)
}
