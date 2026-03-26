get_data <- function(input, rows) {
  
  if (length(rows) == 0) return(NULL)
  
  data <- lapply(rows, function(i) {
    
    isin <- input[[paste0("isin", i)]]
    security <- input[[paste0("security", i)]]
    short <- input[[paste0("short", i)]]
    sector <- input[[paste0("sector", i)]]
    mcap <- input[[paste0("mcap", i)]]
    
    if (is.null(isin) || isin == "") return(NULL)
    
    data.frame(
      ISIN = isin,
      Security = security,
      Shortname = short,
      Sector = sector,
      Mcap = mcap,
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, data)
}