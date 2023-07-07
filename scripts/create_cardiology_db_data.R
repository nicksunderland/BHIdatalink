library(dplyr)
library(lubridate)
library(odbc)

root_fp <- system.file("scripts", package = "BHIdatalink")

set.seed(1)

n <- 200
consultant_names <- c("Barman", "Nisbet", "Duncan", "Thomas", "Diab")
procedure_types <- c("angiogram", "pacemaker", "ablation", "crt")

admissions <- data.frame(
  "nhs_number" = sample(seq(9000000000, 9999999999), n, replace=FALSE),
  "datetime_start" = sample(seq(as.POSIXct("2023-07-01 12:00"), as.POSIXct("2023-07-30 12:00"), by="day"), n, replace=TRUE),
  "datetime_end" = "NULL",
  "consultant" = sample(consultant_names, n, replace=TRUE)
)

n_orders <- round(n*0.35)
orders <- data.frame(
  "nhs_number" = sample(admissions$nhs_number, n_orders, replace=FALSE),
  "type" = sample(procedure_types, n_orders, replace=TRUE, prob=seq(0.8, 0.2, length.out=length(procedure_types)))
)
orders$datetime = left_join(orders, admissions, by="nhs_number") |>
  select(datetime_start) |>
  pull() + duration(sample(2:(24*3), nrow(orders), replace=TRUE), units="hours")

procedures <- data.frame(
  "nhs_number" = sample(orders$nhs_number, round(n_orders*0.5), replace=FALSE)
)
procedures = left_join(procedures, orders, by="nhs_number") |>
  mutate(datetime = datetime + duration(sample(6:(24*7), nrow(procedures), replace=TRUE), units="hours"))


con <- dbConnect(odbc::odbc(), dsn="cardiology_db")
dbWriteTable(con, "admissions", admissions, overwrite=TRUE)
dbWriteTable(con, "orders", orders, overwrite=TRUE)
dbWriteTable(con, "procedures", procedures, overwrite=TRUE)


