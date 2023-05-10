# check_binary_radolan_files ---------------------------------------------------
check_binary_radolan_files <- function(bin_files)
{
  # Read information on binary files from their headers
  info <- get_radolan_metadata_from_header(bin_files)

  # Check that information that were read are consistent with file properties
  stopifnot(identical(file.size(bin_files), as.numeric(info$bytes)))
}

# get_radolan_format_description -----------------------------------------------
get_radolan_format_description <- function()
{
  path <- "extdata/radolan_format-description.txt"

  file <- system.file(path, package = "kwb.dwd")

  #file <- "./inst/extdata/radolan_format-description.txt"

  read.table(file, sep = "\t", header = TRUE)
}

# get_radolan_header_patterns --------------------------------------------------
get_radolan_header_patterns <- function()
{
  product_keys <- kwb.utils::selectColumns(
    get_radolan_format_description(),
    "Produktkennung"
  )

  # Different example headers
  # "RW010050100000116BY1620142VS 3SW   2.13.1PR E-01INT  60GP 900x 900MS 70<boo,ros,emd,hnr,umd,pro,ess,fld,drs,neu,nhb,oft,eis,tur,isn,fbg,mem> "
  # "RW310950100000816BY1620153VS 3SW   2.16.0PR E-01INT  60GP 900x 900MF 00000001MS 70<boo,ros,emd,hnr,umd,pro,ess,fld,drs,neu,nhb,oft,eis,tur,isn,fbg,mem> "
  # "RW010050100000116BY1620142VS 3SW   2.13.1PR E-01INT  60GP 900x 900           MS 70<boo,ros,emd,hnr,umd,pro,ess,fld,drs,neu,nhb,oft,eis,tur,isn,fbg,mem> "

  # https://www.dwd.de/DE/leistungen/radolan/radolan_info/radolan_radvor_op_komposit_format_pdf
  patterns <- list(

    # One of the product keys described in get_radolan_format_description()
    product = paste(product_keys, collapse = "|"),

    # 3I2 Zeitpunkt der Messung: Tag, Stunde und Minute (ddhhmm) in UTC
    ddHHMM = "[0-9]{6}",

    # I5 Radarstandort; für Komposit wird immer 10000 verwendet
    site = "[0-9]{5}",

    # 2I2 Zeitpunkt der Messung: Monat und Jahr (MMYY)
    mmyy = "[0-9]{4}",

    # A2 Kennung "BY“
    # I7 Produktlänge (in Byte);
    # bei Version 4: I10;
    # bei Version 5: I7 oder I10 (s. Kennung VS)
    bytes = "BY([0-9]{7})",

    # A2 Kennung "VS" (falls nicht vorhanden, dann wurden als
    # Grundlage zur Generierung des Komposits standortbezogene
    # Radardaten mit 100km Radius verwendet)
    # I2 Format-Version:
    # 0: sog. „Mischversion“ mit 100 km und 128 km Radius, bedingt
    #    durch die Erweiterung des quantitativen Messbereiches im
    #    Frühjahr 2000;
    # 1: 100 km Radius;
    # 2: 128 km Radius;
    # 3: 150 km Radius (ab 30.06.2010 gesetzt; ab 17.03.2010
    #    fälschlicherweise noch als 2 dokumentiert);
    # 4: räumliche Auflösung in 250m und Produktlänge (in Byte) bei
    # der Kennung BY mit 10 Stellen (I10);
    # 5: WGS84-Georeferenzierung und Produktlänge (in Byte) bei der
    #    Kennung BY mit 10 Stellen (I10) für die Produkte RV,
    #    RS und RE (beim RQ, FS und FQ bleibt es bei 7 Stellen (I7)
    #    bei der Kennung BY)
    format_version = "V?S?([0-9 ]{2})",

    # A2 Kennung "SW"
    # 1X,A8 Software-Version von RADOLAN - beginnend mit „00.01.00“ für die erste
    # Testverion von RADOLAN - bzw. von den RADVOR-/POLARA-Versionen
    software_version = "SW.(.{8})",

    # A2 Kennung "PR"
    # 1X,A4 Genauigkeit der Daten: „E-00“ für ganze Zahlen, „E-01“ für 1/10;
    # „E-02“ für 1/100
    precision = "PR.(.{4})",

    # A3 Kennung "INT"
    # I4 Intervalldauer in Minuten
    duration = "INT([0-9 ]{4})",

    # A1 Kennung "U"; nur bei den Produkten S2, S3, D2, D3, W1, W2, W3, W4, SM,
    # SZ, SJ, SY, AM, %M, AZ, AJ, %J und %Y
    # I1 Maßeinheit von "INT": 0 = Minuten, 1 = Tage
    unit = "(U[01])?",

    # A2 Kennung "GP"
    # A9 Anzahl der Pixel im Ausschnittsgebiet: " 900x 900" für nationale
    # Komposits, „1100x 900“ für erweiterte nationale Komposits, "1500x1400" für
    # mitteleuropäische Komposits (Bedeutung: 1500 Zeilen und 1400 Spalten (s.a.
    # Kap. 3.2))
    n_pixel = "GP([0-9 x-]{9})",

    # A2 Kennung “VV“ (Vorhersagezeitpunkt)
    # 1X,I3 Vorhersagezeitpunkt in Minuten nach der Messung
    prediction = "(VV.[0-9 ]{3})?",

    # A2 Kennung “MF“ (Modul Flags)
    # 1X,I8 Dezimalwert der entsprechenden Binärdarstellung (Erklärung s. beim Produkt RV)
    flags = "(MF.[0-9]{8})?",

    # A2 Kennung "QN" (Quantifizierungsart)
    # 1X,I3 definierte Quantifizierungen (s. nachfolgende Tabelle)
    quantification = "(QN.[0-9 ]{3})?",

    # A2 Kennung "MS"
    # I3 Textlänge m (max. 999)
    text_length = "MS([0-9 ]{3})",

    # all the rest, not further split
    rest = ".*"
  )

  # Enclose elements that do not yet contain parentheses in parentheses
  pp <- unlist(patterns)
  has_no_parentheses <- !grepl("\\(", pp)
  pp[has_no_parentheses] <- paste0("(", pp[has_no_parentheses], ")")

  pp
}

# get_radolan_metadata_from_header ---------------------------------------------
get_radolan_metadata_from_header <- function(bin_files, dbg = FALSE)
{
  bin_files %>%
    sapply(read_binary_radolan_header, dbg = dbg) %>%
    split_radolan_header() %>%
    kwb.utils::resetRowNames()
}

# get_radolan_timestamps -------------------------------------------------------
get_radolan_timestamps <- function(bin_files, from = "headers")
{
  if (from == "headers") {
    return(get_radolan_timestamps_from_headers(bin_files))
  }

  if (from == "filenames") {
    return(get_radolan_timestamps_from_filenames(bin_files))
  }

  kwb.utils::stopFormatted(
    "from must be one of 'headers', 'filenames' but was: '%s'",
    from
  )
}

# get_radolan_timestamps_from_filenames ----------------------------------------
get_radolan_timestamps_from_filenames <- function(files)
{
  filenames <- basename(files)

  extensions <- kwb.utils::fileExtension(filenames)

  stopifnot(kwb.utils::allAreEqual(extensions))

  extension <- extensions[1L]

  if (extension == "") {

    pattern <- "-([0-9]{10})-dwd---bin"
    format <- "%y%m%d%H%M"

  } else if (extension == "asc") {

    pattern <- "_([0-9]{8}-[0-9]{4})\\.asc"
    format <- "RW_%Y%m%d-%H%M.asc"

  } else {

    kwb.utils::stopFormatted("File extension '%s' not supported.", extension)
  }

  stopifnot(all(grepl(pattern, filenames)))

  timestrings <- kwb.utils::extractSubstring(pattern, filenames, index = 1L)

  as.POSIXct(timestrings, format = format, tz = "UTC")
}

# get_radolan_timestamps_from_headers ------------------------------------------
#' @importFrom kwb.utils pasteColumns resetRowNames
get_radolan_timestamps_from_headers <- function(bin_files, dbg = FALSE)
{
  # Read information on binary files from their headers
  info <- get_radolan_metadata_from_header(bin_files, dbg = dbg)

  # Create the timestamps that the grids relate to
  as.POSIXct(
    kwb.utils::pasteColumns(info, c("mmyy", "ddHHMM")),
    format = "%m%y %d%H%M",
    tz = "UTC"
  )
}

# get_regional_stats_from_binary_radolan_files ---------------------------------
get_regional_stats_from_binary_radolan_files <- function(
    bin_files, shape, blocksize = 24L, dbg = TRUE
)
{
  shape <- transform_coords(shape, get_radolan_projection_string())

  # Create timestamps from file names
  bin_times <- get_radolan_timestamps(bin_files, from = "filenames")

  block_indices <- data.frame(i = seq_along(bin_files)) %>%
    kwb.utils::splitIntoFixSizedBlocks(blocksize = blocksize) %>%
    lapply(kwb.utils::selectColumns, "i")

  result_list <- lapply(seq_along(block_indices), function(block_no) {

    #block_no <- 1L
    kwb.utils::catAndRun(
      sprintf("Reading block %d of %d", block_no, length(block_indices)),
      dbg = dbg,
      expr = {

        indices <- block_indices[[block_no]]

        # Read binary files into raster objects
        grids_bin <- lapply(
          bin_files[indices],
          kwb.dwd:::read_binary_radolan_file,
          consider_flags = TRUE
        )

        data <- as.data.frame(do.call(rbind, lapply(grids_bin, function(grid) {
          grid %>%
            raster::mask(shape) %>%
            raster::crop(shape) %>%
            kwb.dwd:::raster_stats()
        })))

      }
    )

  })

  do.call(rbind, result_list) %>%
    kwb.utils::setColumns(datetime_utc = bin_times) %>%
    kwb.utils::moveColumnsToFront("datetime_utc")
}

# split_radolan_header ---------------------------------------------------------
split_radolan_header <- function(x)
{
  pattern_parts <- get_radolan_header_patterns()

  pattern <- paste0(pattern_parts, collapse = "")

  index <- stats::setNames(seq_along(pattern_parts), names(pattern_parts))

  kwb.utils::extractSubstring(pattern, x, index)
}

