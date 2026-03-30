library(shiny)
library(later)
library(openxlsx)

scripMasterUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Scrip Master"),
    
    div(style = "max-width: 1600px; margin: auto;",
        fluidRow(
          column(4, br(), actionButton(ns("add"), "Add Company")),
          column(4, br(), actionButton(ns("save"), "Save Data")),
          column(4, br(), fileInput(ns("upload_excel"), "Upload from Excel", accept = ".xlsx"))
        ),
        
        br(),
        
        div(class = "table-wrapper",
            div(
              class = "flex-row header-row",
              div("Sr.No",         class = "col-sm"),
              div("ISIN",          class = "col-sm"),
              div("Security Name", class = "col-lg"),
              div("Shortname",     class = "col-sm"),
              div("Sector",        class = "col-sm"),
              div("Mcap",          class = "col-sm"),
              div("",              class = "col-sm")
            ),
            div(id = ns("company_rows"))
        )
    )
  )
}

scripMasterServer <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    
    source("functions/data_handler.R")
    
    ns <- session$ns
    
    saved_data <- NULL
    
    count <- reactiveVal(0)
    active_rows <- reactiveVal(integer(0))
    
    # 🔥 ADD ROW FUNCTION
    add_row <- function(i) {
      
      insertUI(
        selector = paste0("#", ns("company_rows")),
        where = "beforeEnd",
        ui = div(
          id = ns(paste0("row", i)),
          class = "flex-row data-row",
          div(textOutput(ns(paste0("num", i))),          class = "col-sm"),
          div(textInput(ns(paste0("isin", i)),     NULL), class = "col-sm"),
          div(textInput(ns(paste0("security", i)), NULL), class = "col-lg"),
          div(textInput(ns(paste0("short", i)),    NULL), class = "col-sm"),
          div(textInput(ns(paste0("sector", i)),   NULL), class = "col-sm"),
          div(selectInput(ns(paste0("mcap", i)),   NULL,  # ← changed
                      choices  = c("", "Large", "Medium", "Small"),
                      selected = ""),                  class = "col-sm"),
          div(actionButton(ns(paste0("remove", i)), "X"), class = "col-sm")
        )
      )
      
      observeEvent(input[[paste0("remove", i)]], {
        removeUI(selector = paste0("#", ns(paste0("row", i))))
        active_rows(setdiff(active_rows(), i))
      }, ignoreInit = TRUE)
    }
    
    loaded <- reactiveVal(FALSE)
    
    loaded <- reactiveVal(FALSE)
    
    observeEvent(shared$device_id, {
  dev_id <- shared$device_id
  
  session$onFlushed(function() {
    saved_data <- load_scrip(dev_id)
    if (is.null(saved_data)) return()
    
    n <- nrow(saved_data)
    isolate(count(n))
    isolate(active_rows(seq_len(n)))
    
    for (i in seq_len(n)) {
      add_row(i)
    }
    
    session$onFlushed(function() {
      for (i in seq_len(n)) {
        updateTextInput(session, paste0("isin", i),     value = saved_data$ISIN[i])
        updateTextInput(session, paste0("security", i), value = saved_data$Security[i])
        updateTextInput(session, paste0("short", i),    value = saved_data$Shortname[i])
        updateTextInput(session, paste0("sector", i),   value = saved_data$Sector[i])
        updateTextInput(session, paste0("mcap", i),     value = saved_data$Mcap[i])
      }
      isolate(loaded(TRUE))
    }, once = TRUE)
    
  }, once = TRUE)
}, once = TRUE)
    
    # ADD BUTTON
    observeEvent(input$add, {
      new_id <- count() + 1
      count(new_id)
      active_rows(c(active_rows(), new_id))
      add_row(new_id)
    })

# EXCEL UPLOAD
observeEvent(input$upload_excel, {
  req(input$upload_excel)
  
  tryCatch({
    xl <- openxlsx::read.xlsx(input$upload_excel$datapath)
    
    # Normalize column names
    names(xl) <- tolower(trimws(gsub("\\s+", " ", names(xl))))
    
    message("Excel columns found: ", paste(names(xl), collapse = ", "))
    
    # Flexible column detection
    isin_col     <- names(xl)[grepl("isin",     names(xl))][1]
    sec_col      <- names(xl)[grepl("security", names(xl))][1]
    short_col    <- names(xl)[grepl("short",    names(xl))][1]
    sector_col   <- names(xl)[grepl("sector",   names(xl))][1]
    mcap_col     <- names(xl)[grepl("mcap",     names(xl))][1]
    
    if (any(is.na(c(isin_col, sec_col, short_col, sector_col, mcap_col)))) {
      showNotification(
        paste("Could not find all columns. Found:", paste(names(xl), collapse = ", ")),
        type = "error"
      )
      return()
    }
    
    n <- nrow(xl)
    
    # Clear existing rows first
    current_rows <- active_rows()
    for (i in current_rows) {
      removeUI(selector = paste0("#", ns(paste0("row", i))))
    }
    active_rows(integer(0))
    count(0)
    
    # Add new rows from excel
    for (i in seq_len(n)) {
      new_id <- count() + 1
      count(new_id)
      active_rows(c(active_rows(), new_id))
      add_row(new_id)
    }
    
    # Fill values after rows are created
    session$onFlushed(function() {
      for (i in seq_len(n)) {
        updateTextInput(session, paste0("isin", i),     
                        value = trimws(xl[[isin_col]][i]))
        updateTextInput(session, paste0("security", i), 
                        value = trimws(xl[[sec_col]][i]))
        updateTextInput(session, paste0("short", i),    
                        value = trimws(xl[[short_col]][i]))
        updateTextInput(session, paste0("sector", i),   
                        value = trimws(xl[[sector_col]][i]))
        updateTextInput(session, paste0("mcap", i),     
                        value = trimws(xl[[mcap_col]][i]))
      }
    }, once = TRUE)
    
    showNotification(paste(n, "companies uploaded successfully!"), type = "message")
    
  }, error = function(e) {
    showNotification(paste("Upload error:", e$message), type = "error")
  })
})
    
    # ROW NUMBERS
    observe({
      rows <- active_rows()
      
      for (j in seq_along(rows)) {
        local({
          idx <- rows[j]
          num <- j
          
          output[[paste0("num", idx)]] <- renderText(num)
        })
      }
    })
    
    # 🔥 REAL-TIME DATA (IMPORTANT)
    live_data <- reactive({
      rows <- active_rows()
      get_data(input, rows)
    })
    
    # SAVE BUTTON (optional persistence)
    observeEvent(input$save, {
      message("Save clicked")
      message("device_id: ", shared$device_id)
  
      if (is.null(shared$device_id)) {
        showNotification("Device ID not ready, try again!", type = "error")
        return()
      }
      
      df <- live_data()
      message("df rows: ", nrow(df))
      
      if (!is.null(df)) {
        save_scrip(live_data(), shared$device_id)
        showNotification("Data saved!", type = "message")
      } else {
        showNotification("No data!", type = "error")
      }
    })
    
    observe({
      shared$scrip <- live_data()
    })
  })
}
