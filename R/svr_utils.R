#' connect_to_database
#'
#' @return a database connection
#' @noRd
#'
connect_to_database <- function() {
  con <- try(connect_to_mysql(), silent = TRUE)
  if(inherits(con, "try-error")) {
    con <- try(connect_to_sqlite(), silent = TRUE)
  }
  if(inherits(con, "try-error")) {
    stop("Error connecting to data")
  }
  return(con)
}

#' connect_to_mysql
#'
#' @importFrom odbc odbc dbConnect
#' @return a mySQL database connection
#' @noRd
#'
connect_to_mysql <- function() {
  dbConnect(odbc(), dsn = "bad database name")
  #dbConnect(odbc(), dsn = "cardiology_db")
}

#' connect_to_sqlite
#'
#' @importFrom RSQLite SQLite
#' @return an SQLite database connection
#' @noRd
#'
connect_to_sqlite <- function() {
  dbConnect(SQLite(), "inst/app/www/test.db")
}
