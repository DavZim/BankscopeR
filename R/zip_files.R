#' Compresses files to a .zip-files in the current folder
#'
#' @param zip_name the output name for the .zip-file
#' @param ... data.frames that can be written to csv using fwrite
#' @param file_names a vector of names for the .csv-files
#' (if left empty, the files will be named file1.csv, file2.csv etc.)
#' @param folder the folder, where the file will be saved
#' @param verbose show current status of operations, defaults to false
#'
#' @return nothing
#' @export
#'
#' @examples
#' x <- data.frame(x = 1:10)
#' y <- data.frame(y = 1:10)
#'
#' zip_files(zip_name = "my_zip", x, y)
zip_files <- function(zip_name, ..., file_names = NA, folder = NA, verbose = F) {
  # check input
  if (grepl("\\.zip$", zip_name, perl = T))
    zip_name <- gsub("\\.zip$", "", zip_name, perl = T)
  files <- list(...)
  if (!all(sapply(files, is.data.frame)))
    stop("all objects have to be a data.frame/data.table")
  if (length(file_names) == 1 &&is.na(file_names))
    file_names <- paste0("file", 1:length(files), ".csv")

  # save the data to a tmp-file
  tmp <- tempdir()
  tmp_dir <- paste(tmp, zip_name, sep = "/")

  if (!dir.exists(tmp_dir)) dir.create(tmp_dir)

  if (exists("fwrite", where = "package:data.table", mode = "function")) {
    a <- lapply(1:length(files), function(i) {
      data.table::fwrite(files[[i]], paste(tmp_dir, file_names[[i]], sep = "/"))
    })
  } else {
    a <- lapply(1:length(files), function(i) {
      write.csv(files[[i]], paste(tmp_dir, file_names[[i]], sep = "/"), row.names = F)
    })
  }

  # zip the data
  owd <- getwd()
  setwd(tmp_dir)
  zip_file_name <- paste0(zip_name, ".zip")
  if (verbose) {
    a <- capture.output(zip(zip_file_name, "."))
  } else {
    zip(zip_file_name, ".")
  }

  setwd(owd)
  if (is.na(folder)) {
    file.copy(paste(tmp, zip_name, zip_file_name, sep = "/"),
              zip_file_name, overwrite = T)
  } else {
    file.copy(paste(tmp, zip_name, zip_file_name, sep = "/"),
              paste(folder, zip_file_name, sep = "/"), overwrite = T)
  }

  # delete the tmp-files
  unlink(tmp)
  # cat("Files successfully zipped to file", paste0("'", zip_name, ".zip'"), "\n")
}
