# read_binary_radolan_file_raw -------------------------------------------------
read_binary_radolan_file_raw <- function(file, version = 3)
{
  stopifnot(version %in% 1:3)

  if (version == 1) {
    return(read_binary_radolan_file_raw_v1(file))
  }

  if (version == 2) {
    return(read_binary_radolan_file_raw_v2(file))
  }

  if (version == 3) {
    return(read_binary_radolan_file_raw_v3(file))
  }
}

# read_binary_radolan_file_raw_v1 ----------------------------------------------
read_binary_radolan_file_raw_v1 <- function(path)
{
  header_end <- regexpr("\003", readLines(path, 1, warn = FALSE))

  rb_stream <- file(path, "rb")
  on.exit(close(rb_stream))

  skip_temp <- readBin(rb_stream, "raw", n = header_end, endian = "little")
  rb_data <- readBin(rb_stream, "raw", size = 1, n = 900*900*2, endian = "big")
  rb_data <- rawToBits(rb_data)

  rbi <- lapply(seq(0, 900*900-1, 1), function(x){
    sum(as.integer(rb_data[(x*16+1):(x*16+12)]) * 2**seq(0, 11, 1))
  })

  unlist(rbi)
}

# read_binary_radolan_file_raw_v2 ----------------------------------------------
#' Read bindry radoline file raw v2
#'
#' @param path path
#'
#' @return ???
#' @keywords internal
#' @noMd
#' @noRd
#' @importFrom bitops bitAnd
read_binary_radolan_file_raw_v2 <- function(path)
{
  # Read the header from the file
  header <- read_binary_radolan_header(path)

  # Open file for binary reading
  stream <- file(path, "rb")

  # Close file on exit
  on.exit(close(stream))

  # Skip the header
  readBin(stream, "raw", nchar(header) + 1)

  # We expect to find 900 x 900 in the header
  stopifnot(grepl(" 900x 900", header))

  # Number of integers to read
  n_integers <- 900 * 900

  # Read a stream of bytes (two bytes per integer value)
  bytes <- readBin(stream, "raw", n = n_integers * 2)

  # Determine the even indices in the vector of bytes
  even_indices <- 2 * seq_len(n_integers)

  # Set the high bits of the even bytes to zero
  bytes[even_indices] <- as.raw(bitops::bitAnd(bytes[even_indices], 0x0f))

  # Re-interpret the byte stream as stream of integers
  readBin(bytes, "integer", n_integers, size = 2, endian = "little")
}

# read_binary_radolan_file_raw_v3 ----------------------------------------------
read_binary_radolan_file_raw_v3 <- function(path)
{
  # Read the header from the file
  header <- read_binary_radolan_header(path)

  # Open file for binary reading
  stream <- file(path, "rb")

  # Close file on exit
  on.exit(close(stream))

  # Skip the header
  readBin(stream, "raw", nchar(header) + 1)

  # We expect to find 900 x 900 in the header
  stopifnot(grepl(" 900x 900", header))

  # Number of integers to read
  n_integers <- 900 * 900

  # Read integer values of two bytes each
  integers <- readBin(
    stream, "integer", n = n_integers, size = 2, signed = FALSE,
    endian = "little"
  )

  # Return the values and the flags in attribute "flags"
  structure(bitwAnd(integers, 0x0fff), flags = bitwAnd(integers, 0xf000))
}

# read_binary_radolan_header ---------------------------------------------------
read_binary_radolan_header <- function(path, buffer_size = 1024L)
{
  # Open file for binary reading
  stream <- file(path, "rb")

  # Close file on exit
  on.exit(close(stream))

  # Read the first bytes into a buffer
  buffer <- readBin(stream, "raw", buffer_size)

  # Which byte indicates the end of the header?
  is_end_of_header <- buffer == 0x03

  if (! any(is_end_of_header)) {

    stop(
      "Could not find a byte '0x03' indicating the end of the header\n",
      "within the first ", buffer_size, " bytes of the file\n", path, ".",
      call. = FALSE
    )
  }

  # Get the header information as character string
  rawToChar(buffer[seq_len(which(is_end_of_header)[1] - 1)])
}
