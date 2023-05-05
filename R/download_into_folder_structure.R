# download_into_folder_structure -----------------------------------------------
download_into_folder_structure <- function(
    urls, target_dir, skip_url_segments, ...
)
{
  relative_paths <- url_to_relative_path(urls, skip_url_segments)

  # Paths to target files
  target_files <- file.path(target_dir, relative_paths)

  # Paths of directories to be created
  directory_paths <- unique(dirname(target_files))

  # Create required directory structure
  kwb.utils::createDirectories(directory_paths, dbg = FALSE)

  # Download the files that are not yet in the target directory structure
  mapply(
    FUN = download_if_not_there,
    urls,
    target_files,
    MoreArgs = list(...)
  )

  # Return the target paths and the target directory in attribute "base_dir"
  structure(target_files, base_dir = target_dir)
}

# url_to_relative_path ---------------------------------------------------------
url_to_relative_path <- function(url, skip_url_segments = 1L)
{
  stopifnot(skip_url_segments > 0L)

  segments <- strsplit(remove_protocol(url), "/")

  stopifnot(all(lengths(segments) > skip_url_segments))

  sapply(segments, function(x) {
    paste(x[-seq_len(skip_url_segments)], collapse = "/")
  })
}
