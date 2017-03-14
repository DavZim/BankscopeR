#' Saves the (raw)-data from Bankscope into a csv or zip-file
#'
#' @param dt a data.table as outputted from read_folder or read_bankscope
#' @param file the name to a file (with file-extension (.zip, .RDS, .csv))
#' @param folder the path to the folder, defaults to current working directory
#' @param verbose show current status of operations, defaults to false
#'
#' @return nothing
#' @export
#'
#' @seealso all file-formats can be read again with \code{\link{load_data}}
#'
#' @examples
#' dt <- read_folder("folder_to_my_exports")
#' save_data(dt, file = "my_data.zip")
#' save_data(dt, file = "my_data.RDS")
#' save_data(dt, file = "my_data.csv")
save_data <- function(dt, file, folder = NA, verbose = F) {
  if (!substr(file, nchar(file) - 3, nchar(file)) %in% c(".csv", ".zip", ".RDS"))
    stop("You have to specify either .zip, .RDS, or .csv for 'file'")

  t0 <- Sys.time()
  if (verbose) cat(sprintf("## Writing to '%s' ##\n", file))

  if (grepl("\\.zip$", file, perl = T)) {
    vars <- names(dt)
    if (!all(c("bvd_id", "variable", "year", "cur", "units", "value") %in% vars))
      stop("dt has to have at least the following variables: bvd_id, variable, year, cur, units, and value")

    # split the data up
    dt_data <- dt[, .(bvd_id, variable, year, cur, units, value)]
    # split the variable names up (leave var_id)
    dt_vars <- dt[, .(variable = sort(unique(variable)))][, var_id := 1:.N]
    dt_data <- merge(dt_data, dt_vars, all.x = T, by = "variable")[, variable := NULL][]

    # split the company ids up (leave comp_id) (shorter)
    dt_comps <- dt[, .(bvd_id = sort(unique(bvd_id)))][, comp_id := 1:.N]
    dt_data <- merge(dt_data, dt_comps, all.x = T, by = "bvd_id")[, bvd_id := NULL][]
    setcolorder(dt_data, c("comp_id", "var_id", "year", "cur", "units", "value"))

    dt_extra <- dt[, which(!(names(dt) %in% c("variable", "year", "cur", "units", "value"))), with = F]
    dt_extra <- unique(dt_extra)

    zip_files(zip_name = file, dt_data, dt_vars, dt_comps, dt_extra,
              file_names = c("data.csv", "vars.csv", "companies.csv", "extra.csv"),
              folder = NA, verbose = verbose)

  } else if (grepl("\\.RDS$", file, perl = T)) {
    if (!is.na(folder)) {
      saveRDS(dt, paste(folder, file, sep = "/"), compress = "gzip")
    } else {
      saveRDS(dt, file, compress = "gzip")
    }
  } else if (grepl("\\.csv$", file, perl = T)) {
    if (exists("fwrite", where = "package:data.table", mode = "function")) {
      if (!is.na(folder)) {
        data.table::fwrite(dt, paste(folder, file, sep = "/"))
      } else {
        data.table::fwrite(dt, file)
      }
    } else {
      if (!is.na(folder)) {
        write.csv(dt, paste(folder, file, sep = "/"), row.names = F)
      } else {
        write.csv(dt, file, row.names = F)
      }
    }

  }
  if (verbose) cat(sprintf("## Data successfully written to '%s' in %.2f seconds ##\n",
                           file, difftime(Sys.time(), t0, units = "sec")))
}
