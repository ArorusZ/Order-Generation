library(RPostgres)
library(DBI)

# Connect to database
get_con <- function() {
  dbConnect(
    RPostgres::Postgres(),
    dbname   = Sys.getenv("PGDATABASE"),
    host     = Sys.getenv("PGHOST"),
    port     = Sys.getenv("PGPORT"),
    user     = Sys.getenv("PGUSER"),
    password = Sys.getenv("PGPASSWORD")
  )
}

# Save scrip master data
save_scrip <- function(df) {
  con <- get_con()
  dbWriteTable(con, "scrip_master", df, overwrite = TRUE)
  dbDisconnect(con)
}

# Load scrip master data
load_scrip <- function() {
  con <- get_con()
  if (dbExistsTable(con, "scrip_master")) {
    df <- dbReadTable(con, "scrip_master")
    dbDisconnect(con)
    return(df)
  }
  dbDisconnect(con)
  return(NULL)
}

# Save model portfolio data
save_portfolio <- function(df) {
  con <- get_con()
  dbWriteTable(con, "portfolio", df, overwrite = TRUE)
  dbDisconnect(con)
}

# Load model portfolio data
load_portfolio <- function() {
  con <- get_con()
  if (dbExistsTable(con, "portfolio")) {
    df <- dbReadTable(con, "portfolio")
    dbDisconnect(con)
    return(df)
  }
  dbDisconnect(con)
  return(NULL)
}

# Save order generation data
save_ogw <- function(df) {
  con <- get_con()
  dbWriteTable(con, "ogw", df, overwrite = TRUE)
  dbDisconnect(con)
}

# Load order generation data
load_ogw <- function() {
  con <- get_con()
  if (dbExistsTable(con, "ogw")) {
    df <- dbReadTable(con, "ogw")
    dbDisconnect(con)
    return(df)
  }
  dbDisconnect(con)
  return(NULL)
}