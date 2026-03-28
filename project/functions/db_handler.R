library(RPostgres)
library(DBI)

get_con <- function() {
  url <- Sys.getenv("DATABASE_URL")
  url <- gsub("^postgres(ql)?://", "", url)
  
  user_pass <- sub("@.*", "", url)
  host_rest <- sub(".*@", "", url)
  
  user     <- sub(":.*", "", user_pass)
  password <- sub(".*:", "", user_pass)
  host     <- sub("[:/].*", "", host_rest)
  dbname   <- sub(".*/", "", host_rest)
  port     <- if (grepl(":", host_rest)) as.integer(sub(".*:(\\d+)/.*", "\\1", host_rest)) else 5432L
  
  dbConnect(
    RPostgres::Postgres(),
    dbname   = dbname,
    host     = host,
    port     = port,
    user     = user,
    password = password,
    sslmode  = "require"
  )
}

# Add to db_handler.R
reset_tables <- function() {
  tryCatch({
    con <- get_con()
    dbExecute(con, "DROP TABLE IF EXISTS portfolio")
    dbExecute(con, "DROP TABLE IF EXISTS ogw")
    dbExecute(con, "DROP TABLE IF EXISTS scrip_master")
    dbDisconnect(con)
    message("Tables dropped successfully")
  }, error = function(e) message("Reset error: ", e$message))
}
           
# SCRIP MASTER
save_scrip <- function(df, device_id) {
  tryCatch({
    con <- get_con()
    df$device_id <- device_id
    dbWriteTable(con, "scrip_master", df[df$device_id == device_id, ],
                 overwrite = FALSE, append = TRUE)
    # Delete old rows for this device first
    dbExecute(con, "DELETE FROM scrip_master WHERE device_id = $1", list(device_id))
    dbWriteTable(con, "scrip_master", df, overwrite = FALSE, append = TRUE)
    dbDisconnect(con)
  }, error = function(e) message("Save error: ", e$message))
}

load_scrip <- function(device_id) {
  tryCatch({
    con <- get_con()
    if (dbExistsTable(con, "scrip_master")) {
      df <- dbGetQuery(con, "SELECT * FROM scrip_master WHERE device_id = $1", list(device_id))
      dbDisconnect(con)
      df$device_id <- NULL
      return(if (nrow(df) == 0) NULL else df)
    }
    dbDisconnect(con)
    return(NULL)
  }, error = function(e) { message("Load error: ", e$message); NULL })
}

# ORDER GENERATION
save_ogw <- function(df, device_id) {
  tryCatch({
    con <- get_con()
    df$device_id <- device_id
    dbExecute(con, "DELETE FROM ogw WHERE device_id = $1", list(device_id))
    dbWriteTable(con, "ogw", df, overwrite = FALSE, append = TRUE)
    dbDisconnect(con)
  }, error = function(e) message("Save error: ", e$message))
}

load_ogw <- function(device_id) {
  tryCatch({
    con <- get_con()
    if (dbExistsTable(con, "ogw")) {
      df <- dbGetQuery(con, "SELECT * FROM ogw WHERE device_id = $1", list(device_id))
      dbDisconnect(con)
      df$device_id <- NULL
      return(if (nrow(df) == 0) NULL else df)
    }
    dbDisconnect(con)
    return(NULL)
  }, error = function(e) { message("Load error: ", e$message); NULL })
}

# MODEL PORTFOLIO
save_portfolio <- function(df, device_id) {
  tryCatch({
    con <- get_con()
    df$device_id <- device_id
    dbExecute(con, "DELETE FROM portfolio WHERE device_id = $1", list(device_id))
    dbWriteTable(con, "portfolio", df, overwrite = FALSE, append = TRUE)
    dbDisconnect(con)
  }, error = function(e) message("Save error: ", e$message))
}

load_portfolio <- function(device_id) {
  tryCatch({
    con <- get_con()
    if (dbExistsTable(con, "portfolio")) {
      df <- dbGetQuery(con, "SELECT * FROM portfolio WHERE device_id = $1", list(device_id))
      dbDisconnect(con)
      df$device_id <- NULL
      return(if (nrow(df) == 0) NULL else df)
    }
    dbDisconnect(con)
    return(NULL)
  }, error = function(e) { message("Load error: ", e$message); NULL })
}
