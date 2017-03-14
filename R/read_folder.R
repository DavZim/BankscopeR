
#' Read all files in a folder
#'
#' Be aware that all files have to be of type .xlsx and have to be manually opened and saved first (dunno why)
#'
#' @param path_folder the path to a folder
#' @param ... additional parameters that are passed to \code{\link{read_bankscope}}
#' @param verbose show current status of operations, defaults to false
#'
#' @return a data.table in long-format
#' @export
#'
#' @examples
#'
#' read_folder("data")
#'
#' read_folder("data/")
#'
#' read_folder("data", ncores = 2)
#'
#' read_folder("data", escape_vars = c("Shareholder", "Number of Branches"))
read_folder <- function(path_folder, ..., verbose = F) {
  parallel_wrapper <- function(file, ...) {
    library(data.table)
    library(readxl)
    library(stringr)

    res <- BankscopeR::read_bankscope(file, ...)

    return(res)
  }
  ncores = 1
  t0 <- Sys.time()
  if (verbose) cat(sprintf("## Reading folder %s ##\n", path_folder))
  # remove trailing / (doesnt really matter, but prints nicer.)
  if (substr(path_folder, nchar(path_folder), nchar(path_folder)) == "/")
    path_folder <- substr(path_folder, 1, nchar(path_folder) - 1)

  files <- list.files(path_folder, full.names = T)
  files <- str_replace(files, "\\~\\$", "")

  if (!is.na(ncores) && ncores == 1) {
    res <- lapply(files, function(file) {
      if (verbose) cat("# Reading in:", file, "#\n")
      read_bankscope(file, ...)
    })
  } else {
    # currently unfunctional...
    if (is.na(ncores)) ncores <- min(parallel::detectCores(logical = T), 128)
    if (verbose) cat(sprintf("# Loading the files on %d cores #\n", ncores))

    cl <- parallel::makeCluster(ncores)
    parallel::clusterExport(cl, "read_bankscope")
    res <- parallel::clusterApplyLB(cl, files, parallel_wrapper, ...)
    parallel::stopCluster(cl)
  }

  res <- data.table::rbindlist(res)
  res <- unique(res)

  if (verbose)
    cat(sprintf("## Read %d files in %.3f seconds ##\n",
                length(files), difftime(Sys.time(), t0, units = "sec")))
  return(res)
}
