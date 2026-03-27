library(RPostgres)
library(DBI)

# Connect to database
get_con <- function() {
  url <- Sys.getenv("DATABASE_URL")
  
  # Remove postgresql:// or postgres:// prefix
  url <- gsub("^postgres(ql)?://", "", url)

  message("DATABASE_URL: ", url)
  message("Parsed - host: ", host, " port: ", port, " db: ", dbname)
  
  # Extract using regex
  # Format: user:password@host:port/dbname
  matches <- regmatches(url, regexec("^([^:]+):([^@]+)@([^:]+):([0-9]+)/(.+)$", url))[[1]]
  
  user     <- matches[2]
  password <- matches[3]
  host     <- matches[4]
  port     <- as.integer(matches[5])
  dbname   <- matches[6]
  
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
