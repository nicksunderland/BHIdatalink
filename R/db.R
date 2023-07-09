. = NULL

#' connect_to_database
#' @import dbplyr
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

#' @title get_data
#' @description
#' A function that connects to the database and extracts a specific table, columns, and rows.
#' @param table string, name of the table in the database
#' @param columns string or vector of strings, column names e.g. "id". If a named character vector
#'   is given then the columns are selected on the old_name but renamed such that c("new_name" =
#'   "old_name") - see *NOTE* below.
#' @param conditions string or vector of strings, filter expressions e.g. "id==001", or "sex=male".
#'   *NOTE* filtering occurs after the columns are renamed and so much be constructed with the new
#'   column names.
#' @return a data.frame object
#' @importFrom dplyr select filter all_of everything collect tbl
#' @importFrom magrittr %>%
#' @importFrom DBI dbDisconnect
#' @importFrom rlang parse_expr
#' @noRd
#'
get_data <- function(table, columns=NA_character_, conditions=NA_character_) {
  # argument validation
  stopifnot("`table` must be a string" = is.character(table))
  stopifnot("`columns` must be a string or string vector" = is.character(columns))
  stopifnot("`conditions` must be a string or string vector" = is.character(conditions))

  # connect to the database
  con <- connect_to_database()

  # Get the data as a data.frame, select specific columns if provided
  data <- tbl(con, table) %>%
    {if(!all(is.na(columns))) select(., all_of( columns )) else select(., everything())} %>%
    {if(!all(is.na(conditions))) filter(., !!parse_expr(conditions)) else . } |>
    collect() |>
    as.data.frame()

  # disconnect from the database
  dbDisconnect(con)

  # return the data
  return(data)
}

#' @title admissions_data
#' @description
#' A function returning a clean admissions dataframe with the column names and types:
#'    `nhs_number` numeric
#'    `admission_datetime` POSIXct
#'    `discharge_datetime` POSIXct
#'    `consultant` string
#'    `ward` string
#' @param admitted logical, whether to include currently admitted patients
#' @param discharged logical, whether to include previously admitted patients, i.e. discharged.
#' @return a dataframe with clean standardised column names and correct column types
#' @importFrom dplyr case_when
#' @importFrom purrr map2_df
#' @noRd
#'
admissions_data <- function(admitted=TRUE, discharged=TRUE) {
  # argument validation
  stopifnot("`admitted` and/or `discharged` must be TRUE" = any(c(admitted, discharged)))

  # specify the database -> desired column mappings
  col_map <- c("nhs_number" = "nhs_number",
               "admission_datetime" = "datetime_start",
               "discharge_datetime" = "datetime_end",
               "consultant" = "consultant",
               "ward" = "ward")

  # specify the column data types
  type_map <- c("nhs_number" = as.numeric,
                "admission_datetime" = as.POSIXct,
                "discharge_datetime" = as.POSIXct,
                "consultant" = as.character,
                "ward" = as.character)

  # create the filters if needed
  filters <- case_when( admitted &  discharged ~ NA_character_,
                       !admitted &  discharged ~ "!is.na(discharge_datetime)",
                        admitted & !discharged ~ "is.na(discharge_datetime)")

  # get the data from the admissions table and apply type conversion
  data <- get_data("admissions", col_map, filters) |>
    map2_df(type_map, ~ .y(.x))

  # return the data
  return(data)
}

#' @title orders_data
#' @description
#' A function returning a clean orders dataframe with the column names and types:
#'    `nhs_number` numeric
#'    `order_datetime` POSIXct
#'    `order_type` string
#' @return a dataframe with clean standardised column names and correct column types
#' @importFrom dplyr case_when
#' @importFrom purrr map2_df
#' @noRd
#'
orders_data <- function() {

  # specify the database -> desired column mappings
  col_map <- c("nhs_number" = "nhs_number",
               "order_datetime" = "datetime",
               "order_type" = "type")

  # specify the column data types
  type_map <- c("nhs_number" = as.numeric,
                "order_datetime" = as.POSIXct,
                "order_type" = as.character)

  # get the data from the admissions table and apply type conversion
  data <- get_data("orders", col_map) |>
    map2_df(type_map, ~ .y(.x))

  # return the data
  return(data)
}

#' @title orders_data
#' @description
#' A function returning a clean orders dataframe with the column names and types:
#'    `nhs_number` numeric
#'    `order_datetime` POSIXct
#'    `order_type` string
#' @return a dataframe with clean standardised column names and correct column types
#' @importFrom dplyr case_when
#' @importFrom purrr map2_df
#' @noRd
#'
procedures_data <- function() {

  # specify the database -> desired column mappings
  col_map <- c("nhs_number" = "nhs_number",
               "procedure_datetime" = "datetime",
               "procedure_type" = "type")

  # specify the column data types
  type_map <- c("nhs_number" = as.numeric,
                "procedure_datetime" = as.POSIXct,
                "procedure_type" = as.character)

  # get the data from the admissions table and apply type conversion
  data <- get_data("procedures", col_map) |>
    map2_df(type_map, ~ .y(.x))

  # return the data
  return(data)
}
