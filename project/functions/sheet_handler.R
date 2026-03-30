library(openxlsx)

load_sheet <- function(filepath, sheet_name, 
                       col_map,        # named list e.g. list(sec = "security", alloc = "allocation")
                       validate_against = NULL,  # vector of valid names to check against (optional)
                       on_valid_row,    # function(i, row) — what to do with each valid row
                       on_clear = NULL  # function() — what to do before loading (optional)
) {
  tryCatch({
    xl <- openxlsx::read.xlsx(filepath, sheet = sheet_name)
    
    names(xl) <- tolower(trimws(gsub("\\s+", " ", names(xl))))
    
    message("Excel columns found: ", paste(names(xl), collapse = ", "))
    
    # Detect columns flexibly
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
    
    sec_col <- detected[["sec"]]
    xl$normalized_sec <- tolower(trimws(gsub("\\s+", " ", xl[[sec_col]])))
    
    # Validate against scrip master if provided
    if (!is.null(validate_against)) {
      valid <- tolower(trimws(gsub("\\s+", " ", validate_against)))
      xl_valid   <- xl[xl$normalized_sec %in% valid, ]
      xl_skipped <- xl[!xl$normalized_sec %in% valid, ]
      
      if (nrow(xl_skipped) > 0) {
        showNotification(
          paste("Skipped", nrow(xl_skipped), "not in Scrip Master:",
                paste(xl_skipped[[sec_col]], collapse = ", ")),
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
    
    # Clear existing rows if needed
    if (!is.null(on_clear)) on_clear()
    
    # Process each valid row
    for (i in seq_len(nrow(xl_valid))) {
      on_valid_row(i, xl_valid[i, ], detected)
    }
    
    showNotification(
      paste(nrow(xl_valid), "rows loaded successfully!"),
      type = "message"
    )
    
  }, error = function(e) {
    showNotification(paste("Load error:", e$message), type = "error")
  })
}