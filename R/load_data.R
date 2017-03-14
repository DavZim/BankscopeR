#' Reads saved Banscope data
#'
#'
#' @param file a .zip, .RDS, .csv-file as outputted by \code{\link{save_data}}
#' @param verbose show current status of operations, defaults to false
#'
#' @return a data.table in the same format as \code{\link{save_folder}}
#' @export
#'
#' @seealso \code{\link{save_folder}}
#' @examples
#' dt <- load_folder("my_raw_data")
#' save_data(dt, "my_data.zip")
#' dt2 <- load_data("my_data.zip")
#'
load_data <- function(file, verbose = F) {
  if (!substr(file, nchar(file) - 3, nchar(file)) %in% c(".zip", ".RDS", ".csv"))
    stop("file has to be a file with the extension .zip, .RDS, or .csv")

  t0 <- Sys.time()
  if (grepl("\\.zip$", file, perl = T)) {
    owd <- getwd()
    tmp <- tempdir()
    tmp_dir <- paste(tmp, "data", sep = "/")

    if (dir.exists(tmp_dir)) dir.create(tmp_dir)
    unzip(file, exdir = tmp_dir, overwrite = T)

    setwd(tmp_dir)
    dt_data <- data.table::fread("data.csv")
    dt_vars <- data.table::fread("vars.csv")
    dt_comps <- data.table::fread("companies.csv")
    dt_extra <- data.table::fread("extra.csv")

    setwd(owd)
    unlink(tmp_dir)

    # merge the data to the original format again

    # add the information from
    res <- merge(dt_vars, dt_data, by = "var_id")
    res[, var_id := NULL]
    res <- merge(dt_comps, res, by = "comp_id")
    res[, comp_id := NULL]
    res <- merge(dt_extra, res, by = "bvd_id")

  } else if (grepl("\\.RDS$", file, perl = T)) {
    res <- readRDS(file)
  } else if (grepl("\\.csv$", file, perl = T)) {
    res <- data.table::fread(file)
  }

  res <- unique(res)
  if (verbose) cat(sprintf("## Data successfully loaded from '%s' in %.2f seconds ##\n",
                           file, difftime(Sys.time(), t0, units = "sec")))
  return(res)
}
