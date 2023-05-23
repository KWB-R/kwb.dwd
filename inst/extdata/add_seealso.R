###############################################################################+
#
# Add @seealso directives to exported functions that link to related functions
# A function is assumed to be related if they have one or more parts of their
# name (as separated by underscore) in common.
#
# - Source the whole script first to load the functions defined below.
# - Then, manually go through the MAIN section
#
###############################################################################+

# MAIN -------------------------------------------------------------------------
if (FALSE)
{
  relations <- get_related_function_names("kwb.dwd")

  files <- dir("R", full.names = TRUE)

  for (file in files) {

    lines <- readLines(file)

    (def_indices <- grep("^\\S+ <- function", lines))

    for (i in rev(def_indices)) {
      #i <- rev(def_indices)[1L]
      name <- kwb.utils::extractSubstring("^(\\S+)", lines[i], 1L)

      related_names <- relations[[name]]
      n_related <- length(related_names)

      if (n_related > 0L) {
        kwb.utils::catAndRun(
          sprintf(
            "Adding links to %d related functions for %s() in %s",
            n_related, name, file
          ),
          expr = {
            see_also <- sprintf(
              "#' @seealso\n%s.",
              paste0("#'  * [", related_names, "]", collapse = ",\n")
            )
            lines <- insert_before(lines, see_also, i)
          }
        )
      }

    }

    writeLines(lines, file)
  }
}

# insert_before ----------------------------------------------------------------
insert_before <- function(x, y, i = 1L)
{
  parts <- split(x, ifelse(seq_along(x) < i, "before", "after"))

  c(parts$before, y, parts$after)
}

# get_related_function_names ---------------------------------------------------
get_related_function_names <- function(package, max_relatives = 4L)
{
  #package <- "kwb.dwd"
  `%>%` <- magrittr::`%>%`

  ls(getNamespace(package), all.names = FALSE)

  function_names <- getNamespaceExports(package)
  names(function_names) <- function_names

  parts <- strsplit(function_names, "_")

  all_keywords <- parts %>%
    unlist() %>%
    unique() %>%
    sort()

  clean_keywords <- function(keywords) {
    keywords %>%
      gsub(pattern = "[0-9]", replacement = "") %>%
      unique() %>%
      kwb.utils::removeEmpty() %>%
      setdiff(c("and", "by", "or", "to", "with"))
  }

  clean_parts <- lapply(parts, clean_keywords)

  clean_parts

  lapply(function_names, function(name_1) {
    #name_1 <- function_names[[7L]]

    matches <- sapply(setdiff(function_names, name_1), function(name_2) {
      intersect(clean_parts[[name_1]], clean_parts[[name_2]])
    })

    matches <- matches[lengths(matches) > 0L]

    threshold <- 1L

    while (length(matches) > max_relatives) {

      new_matches <- matches[lengths(matches) > threshold]

      matches <- if (length(new_matches) > 0L) {
        threshold <- threshold + 1L
        new_matches
      } else {
        head(matches, max_relatives)
      }
    }

    names(matches)
  })
}
