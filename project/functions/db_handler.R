library(RPostgres)
library(DBI)

# Connect to database
get_con <- function() {
  url <- Sys.getenv("DATABASE_URL")
  url <- gsub("^postgres(ql)?://", "", url)
  
  # Split user:password from the rest
  user_pass <- sub("@.*", "", url)
  host_rest <- sub(".*@", "", url)
  
  user     <- sub(":.*", "", user_pass)
  password <- sub(".*:", "", user_pass)
  
  # Host may or may not have a port
  host   <- sub("[:/].*", "", host_rest)
  dbname <- sub(".*/", "", host_rest)
  port   <- if (grepl(":", host_rest)) as.integer(sub(".*:(\\d+)/.*", "\\1", host_rest)) else 5432L
  
  message("Parsed - user: ", user, " host: ", host, " port: ", port, " db: ", dbname)
  
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

# Save scrip master data
save_scrip <- function(df) {
  con <- get_con()
  dbWriteTable(con, "scrip_master", df, overwrite = TRUE)
  dbDisconnect(con)
}

load_scrip <- function() {
  tryCatch({
    con <- get_con()
    if (dbExistsTable(con, "scrip_master")) {
      df <- dbReadTable(con, "scrip_master")
      dbDisconnect(con)
      return(df)
    }
    dbDisconnect(con)
    return(NULL)
  }, error = function(e) {
    message("DB load error: ", e$message)
    return(NULL)
  })
}

# Save order generation data
save_ogw <- function(df) {
  con <- get_con()
  dbWriteTable(con, "ogw", df, overwrite = TRUE)
  dbDisconnect(con)
}

load_ogw <- function() {
  tryCatch({
    con <- get_con()
    if (dbExistsTable(con, "ogw")) {
      df <- dbReadTable(con, "ogw")
      dbDisconnect(con)
      return(df)
    }
    dbDisconnect(con)
    return(NULL)
  }, error = function(e) {
    message("DB load error: ", e$message)
    return(NULL)
  })
}

# Save model portfolio data
save_portfolio <- function(df) {
  con <- get_con()
  dbWriteTable(con, "portfolio", df, overwrite = TRUE)
  dbDisconnect(con)
}

load_portfolio <- function() {
  tryCatch({
    con <- get_con()
    if (dbExistsTable(con, "portfolio")) {
      df <- dbReadTable(con, "portfolio")
      dbDisconnect(con)
      return(df)
    }
    dbDisconnect(con)
    return(NULL)
  }, error = function(e) {
    message("DB load error: ", e$message)
    return(NULL)
  })
}
