# library(dplyr)
# library(lubridate)
# library(odbc)
# library(RSQLite)
#
# root_fp <- system.file("scripts", package = "BHIdatalink")
#
# set.seed(1)
#
# n <- 200
# consultant_names <- c("Barman", "Nisbet", "Duncan", "Thomas", "Diab")
# procedure_types <- c("angiogram", "pacemaker", "ablation", "crt")
# wards <- c("c705", "c805", "c708", "ccu", "cicu")
#
#
# start_date <- as.POSIXct("2023-07-01 12:00")
# end_date <- as.POSIXct("2023-07-30 12:00")
# mean_date <- as.numeric(start_date + (end_date - start_date) / 2)  # Mean date
# sd_date <- as.numeric((end_date - start_date) / 6)  # Standard deviation of dates
# random_numbers <- rnorm(n, mean = mean_date, sd = sd_date)
#
# admissions <- data.frame(
#   "nhs_number" = sample(seq(9000000000, 9999999999), n, replace=FALSE),
#   "datetime_start" = sample(seq(as.POSIXct("2023-07-01 12:00"), as.POSIXct("2023-07-30 12:00"), by="hour"), n, replace=FALSE),
#   "datetime_end" = as.POSIXct(NA),
#   "consultant" = sample(consultant_names, n, replace=TRUE),
#   "ward" = sample(wards, n, replace=TRUE, prob=c(0.4,0.4,0.1,0.05, 0.05))
# )
# admissions_hist <- do.call("rbind", replicate(4, admissions, simplify = FALSE)) |>
#   mutate(nhs_number = sample(seq(9000000000, 9999999999), 4*nrow(admissions), replace=FALSE),
#          datetime_end = datetime_start + duration(sample(seq(0,30,length.out=1000), 4*nrow(admissions), replace=TRUE, prob=dnorm(1:1000, mean(1:1000), sd(1:250))), units="days")) |>
#   distinct(nhs_number, .keep_all = TRUE)
# admissions <- rbind(admissions, admissions_hist)
#
#
# n_orders <- round(n*0.35)
# orders <- data.frame(
#   "nhs_number" = sample(admissions$nhs_number, n_orders, replace=FALSE),
#   "type" = sample(procedure_types, n_orders, replace=TRUE, prob=seq(0.8, 0.2, length.out=length(procedure_types)))
# )
# orders$datetime = left_join(orders, admissions, by="nhs_number") |>
#   select(datetime_start) |>
#   pull() + duration(sample(2:(24*3), nrow(orders), replace=TRUE), units="hours")
#
# procedures <- data.frame(
#   "nhs_number" = sample(orders$nhs_number, round(n_orders*0.5), replace=FALSE)
# )
# procedures = left_join(procedures, orders, by="nhs_number") |>
#   mutate(datetime = datetime + duration(sample(6:(24*7), nrow(procedures), replace=TRUE), units="hours"))
# # 2000 more procedures
# n = 2000
# procedures = rbind(
#   procedures,
#   data.frame(
#     "nhs_number" = sample(seq(9000000000, 9999999999), n, replace=FALSE),
#     "datetime" = sample(seq(as.POSIXct("2022-07-30 12:00"), as.POSIXct("2023-07-01 12:00"), by="hour"), n, replace=FALSE),
#     "type" = sample(procedure_types, n, replace=TRUE, prob=seq(0.8, 0.2, length.out=length(procedure_types)))
#   )
# )
#
# # MySQL database locally
# con <- dbConnect(odbc::odbc(), dsn="cardiology_db")
# dbWriteTable(con, "admissions", admissions, overwrite=TRUE)
# dbWriteTable(con, "orders", orders, overwrite=TRUE)
# dbWriteTable(con, "procedures", procedures, overwrite=TRUE)
# dbDisconnect(con)
#
# # SQLite database
# db_fp <- "/Users/nicholassunderland/git/BHIdatalink/inst/app/www/test.db"
# con <- dbConnect(RSQLite::SQLite(), db_fp)
# dbWriteTable(con, "admissions", admissions, overwrite=TRUE)
# dbWriteTable(con, "orders", orders, overwrite=TRUE)
# dbWriteTable(con, "procedures", procedures, overwrite=TRUE)
# dbDisconnect(con)
#





