library(openxlsx)

load_sheet <- function(filepath, sheet_name, 
                       col_map,
                       validate_against = NULL,
                       on_valid_row,
                       on_clear = NULL,
                       on_complete = NULL    # ← add this
                       ) {
  tryCatch({
    xl <- openxlsx::read.xlsx(filepath, sheet = sheet_name)
    
    names(xl) <- tolower(trimws(gsub("\\s+", " ", names(xl))))
    
    sec_col <- names(xl)[grepl(col_map[["sec"]], names(xl))][1]
    
    detected <- lapply(col_map, function(pattern) {
      names(xl)[grepl(pattern, names(xl))][1]
    })
    
    missing_cols <- names(detected)[sapply(detected, is.na)]
    if (length(missing_cols) > 0) {
      showNotification(
        paste("Could not find columns:", paste(missing_cols, collapse = ", "),
              "| Found:", paste(names(xl), collapse = ", ")),
        type = "error"
      )
      return()
    }
    
    xl$normalized_sec <- tolower(trimws(gsub("\\s+", " ", xl[[detected[["sec"]]]])))
    
    if (!is.null(validate_against)) {
      valid      <- tolower(trimws(gsub("\\s+", " ", validate_against)))
      xl_valid   <- xl[xl$normalized_sec %in% valid, ]
      xl_skipped <- xl[!xl$normalized_sec %in% valid, ]
      
      if (nrow(xl_skipped) > 0) {
        showNotification(
          paste("Skipped", nrow(xl_skipped), "not in Scrip Master:",
                paste(xl_skipped[[detected[["sec"]]]], collapse = ", ")),
          type = "warning", duration = 8
        )
      }
      if (nrow(xl_valid) == 0) {
        showNotification("No valid securities found!", type = "error")
        return()
      }
    } else {
      xl_valid <- xl
    }
    
    if (!is.null(on_clear)) on_clear()
    
    # Store all rows for on_complete
    all_rows <- list()
    
    for (i in seq_len(nrow(xl_valid))) {
      on_valid_row(i, xl_valid[i, ], detected)
      all_rows[[i]] <- xl_valid[i, ]    # ← collect rows
    }
    
    # Fire once after all rows processed
    if (!is.null(on_complete)) on_complete(all_rows, detected)    # ← add this
    
    showNotification(
      paste(nrow(xl_valid), "rows loaded successfully!"),
      type = "message"
    )
    
  }, error = function(e) {
    showNotification(paste("Load error:", e$message), type = "error")
  })
}
