#' Reads an Excel-export from Bankscope
#'
#' Be aware that the file has to be manually opened and saved first (Dunno why...)
#' @param path the path to a file
#' @param escape_vars a vector of column (variable) names that have a \% or a currency key but that should not be treated as a variable, defaults to "Shareholder"
#'
#' @return a data.table in long-format
#' @export
#'
#' @seealso to read all files within a folder, use \code{\link{read_folder}}
#'
#' @examples
#' read_bankscope("Bankscope_Export_1.xslx")
#' read_bankscope("Bankscope_Export_1.xslx", escape_vars = c("Shareholders", "Number of Branches"))
#'
read_bankscope <- function(path, escape_vars = "Shareholder") {
  dt <- suppressWarnings(readxl::read_excel(path, sheet = 2, na = "n.a.|n.s."))

  names_ <- names(dt)
  names_[1] <- "id"

  names_ <- gsub("\r\n", " ", names_)

  variable_ <- grepl("EUR|USD|AUS|%|Number of", names_) &
    !(grepl(paste(escape_vars, collapse = "|"), names_))
  names(dt) <- names_

  data.table::setDT(dt)
  # takes out the row-numbers...
  dt <- dt[, -1]
  names_ <- names_[-1]
  # check for double entry names
  if (length(names_) != length(unique(names_))) {
    names_ <- unique(names_)
    dt <- dt[, names_, with = F]
    variable_ <- grepl("EUR|USD|AUS|%|Number of", names_) &
      !(grepl(paste(escape_vars, collapse = "|"), names_))
  }

  # clean-data
  data.table::setnames(dt, "BvD ID Number", "bvd_id")
  names_[names_ == "BvD ID Number"] <- "bvd_id"

  dt_extra <- dt[, names_[!variable_], with = F]
  dt <- dt[, c("bvd_id", names_[variable_]), with = F]

  dt_long <- suppressWarnings(data.table::melt(dt, id.vars = "bvd_id",
                                   measure.vars = names_[variable_],
                                   variable.factor = F))

  # otherwise we get an error in parallel-mode
  if (!data.table::is.data.table(dt_long)) data.table::setDT(dt_long)

  dt_long[, variable := as.character(variable)]
  suppressWarnings(dt_long[, value := as.numeric(value)])
  dt_long <- dt_long[!is.na(value)]

  # take out "last avail. yr"
  dt_long <- dt_long[!grepl("Last avail\\. yr", variable)]

  # take out years
  dt_long[, year := as.numeric(stringr::str_extract(variable, "[0-9]{4}"))]
  dt_long[, variable := str_replace(variable, "[0-9]{4}", "")]

  # Currencies
  # USD
  # EUR
  # GBP
  # JPY
  # CHF
  currencies <- c("USD", "EUR", "GBP", "JPY", "CHF")
  clp_cur <- paste(currencies, collapse = "|")

  dt_long[, ':='(cur = str_extract(variable, clp_cur),
                 variable = str_replace(variable, clp_cur, ""))]

  # units:
  #  -> units
  # th -> thousands
  # mil -> million
  # bil -> billion
  units_ <- c("th", "mil", "bil", "%")
  clp_uni <- paste(units_, collapse = "|")

  dt_long[, ':='(units = str_extract(variable, clp_uni),
                 variable = str_replace(variable, clp_uni, ""))]

  dt_long[, ':=' (cur = ifelse(is.na(cur), "", cur),
                  units = ifelse(is.na(units), "", units),
                  variable = trimws(variable))]

  setcolorder(dt_long, c("bvd_id", "variable", "year", "cur", "units", "value"))
  setorder(dt_long, bvd_id, variable, year)


  dt_all <- merge(dt_extra, dt_long, by = "bvd_id", all.y = T)

  return(dt_all)
}
