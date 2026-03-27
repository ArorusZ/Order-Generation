library(RPostgres)
library(DBI)

# Connect to database
library(RPostgres)
library(DBI)

get_con <- function() {
  url <- Sys.getenv("DATABASE_URL")
  
  # Parse the URL manually
  # Format: postgresql://user:password@host:port/dbname
  url <- sub("postgresql://", "", url)
  user     <- sub(":.*", "", url)
  url      <- sub(".*:", "", url)
  password <- sub("@.*", "", url)
  url      <- sub(".*@", "", url)
  host     <- sub(":.*", "", url)
  url      <- sub(".*:", "", url)
  port     <- as.integer(sub("/.*", "", url))
  dbname   <- sub(".*/", "", url)
  
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
