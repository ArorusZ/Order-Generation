library(shiny)
library(openxlsx)

modelPortfolioUI <- function(id) { 
  ns <- NS(id) 
  
  tagList( 
    h3("Model Portfolio"), 
    
    div(style = "max-width: 1600px; margin: auto;", 
        
        fluidRow( 
          column(4, br(), actionButton(ns("add"), "Add Company")), 
          column(4, br(), actionButton(ns("save"), "Save Portfolio")),
          column(4, br(), fileInput(ns("upload_excel"), "Upload from Excel", accept = ".xlsx"),uiOutput(ns("sheet_selector"))
        ), 
        
        br(), 
        
        div(class = "table-wrapper",
            div(
              class = "flex-row header-row",
              div("Sr.No",         class = "col-sm"),
              div("Security Name", class = "col-lg"),
              div("Allocation(%)", class = "col-sm"),
              div("",              class = "col-sm")
            ),
            div(id = ns("company_rows"))
        ),
        
        div(
          class = "flex-row data-row",
          div("",                              class = "col-sm"),   # blank — lines up under Sr.No
          div(strong("Cash"),                  class = "col-lg"),   # lines up under Security Name
          div(textOutput(ns("cash_value")),    class = "col-sm"),   # lines up under Allocation(%)
          div("",                              class = "col-sm")    # blank — lines up under Remove
        )
    )
  ) 
}


modelPortfolioServer <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    data <- reactive({
      req(shared$scrip)
      shared$scrip
    })
    
    # 🔥 SAFE SECURITY LIST
    securities <- reactive({
      req(data())
      
      if (!"Security" %in% colnames(data())) return(character(0))
      
      unique(data()$Security)
    })
    
    count <- reactiveVal(0)
    active_rows <- reactiveVal(integer(0))
    
    # 🔥 SAFE NUMERIC HELPER
    safe_numeric <- function(x) {
      if (is.null(x) || x == "") return(0)
      val <- as.numeric(x)
      if (is.na(val)) 0 else val
    }
    
    add_row <- function(i, sec_val = NULL, alloc_val = NULL) {
      
      insertUI(
        selector = paste0("#", ns("company_rows")),
        where = "beforeEnd",
        
        ui = div(
          id = ns(paste0("row", i)),
          class = "flex-row data-row",
          div(textOutput(ns(paste0("num", i))),                                                           class = "col-sm"),
          div(uiOutput(ns(paste0("security_ui", i))),                                                     class = "col-lg"),
          div(textInput(ns(paste0("Allocation", i)), NULL, value = alloc_val, placeholder = "Allocation"), class = "col-sm"),
          div(actionButton(ns(paste0("remove", i)), "Remove"),                                             class = "col-sm")
        )
      )
      if (!is.null(sec_val)) {
        later::later(function() {
          updateSelectInput(
            session,
            paste0("Security", i),
            selected = sec_val
          )
        }, delay = 0.05)
      }
      
      # 🔥 DROPDOWN WITH PREFILL
      output[[paste0("security_ui", i)]] <- renderUI({
        req(securities())
  
        choices <- securities()
  
        current <- input[[paste0("Security", i)]]
        selected_val <- if (!is.null(current) && current != "") current else sec_val
  
        # Get all selected values from OTHER rows
        rows <- active_rows()
        other_selected <- sapply(rows[rows != i], function(j) {
          input[[paste0("Security", j)]]
        })
        other_selected <- other_selected[!is.na(other_selected) & other_selected != ""]
  
        # Remove other rows' selections from this dropdown
        available_choices <- choices[!choices %in% other_selected]
  
        # Always keep current row's own selection in the list
        if (!is.null(selected_val) && selected_val != "" && 
            !(selected_val %in% available_choices)) {
          available_choices <- c(available_choices, selected_val)
        }
  
        selectInput(
          ns(paste0("Security", i)),
          NULL,
          choices  = available_choices,
          selected = selected_val,
          selectize = FALSE
        )
      })
      
      observeEvent(input[[paste0("remove", i)]], {
        removeUI(selector = paste0("#", ns(paste0("row", i))))
        active_rows(setdiff(active_rows(), i))
      }, ignoreInit = TRUE)
    }

    # SHOW SHEET SELECTOR AFTER FILE UPLOAD
    output$sheet_selector <- renderUI({
      req(input$upload_excel)
  
      tryCatch({
        sheets <- openxlsx::getSheetNames(input$upload_excel$datapath)
    
        if (length(sheets) == 1) return(NULL)  # only one sheet — no need to show
    
        tagList(
          selectInput(ns("sheet_choice"), "Select Sheet", choices = sheets),
          actionButton(ns("confirm_sheet"), "Load Sheet", 
                       style = "margin-top: 5px;")
        )
      }, error = function(e) NULL)
    })
    
    # EXCEL UPLOAD
    observeEvent(input$upload_excel, {
      req(input$upload_excel, securities())
  
      tryCatch({
        xl <- openxlsx::read.xlsx(input$upload_excel$datapath)
        
        # Normalize column names
        names(xl) <- tolower(trimws(gsub("\\s+", " ", names(xl))))
    
        message("Excel columns found: ", paste(names(xl), collapse = ", "))
    
        # Flexible column detection
        sec_col   <- names(xl)[grepl("security",   names(xl))][1]
        alloc_col <- names(xl)[grepl("allocation", names(xl))][1]
    
        if (is.na(sec_col) || is.na(alloc_col)) {
          showNotification(
            paste("Could not find columns. Found:", paste(names(xl), collapse = ", ")),
            type = "error"
          )
          return()
        }
    
        # Normalize securities from scrip master
        valid_securities <- tolower(trimws(gsub("\\s+", " ", securities())))
    
        # Filter excel rows — only keep securities that exist in scrip master
        xl$normalized_sec <- tolower(trimws(gsub("\\s+", " ", xl[[sec_col]])))
        xl_valid <- xl[xl$normalized_sec %in% valid_securities, ]
        xl_skipped <- xl[!xl$normalized_sec %in% valid_securities, ]
    
        if (nrow(xl_skipped) > 0) {
          message("Skipped securities not in scrip master: ", 
                  paste(xl_skipped[[sec_col]], collapse = ", "))
          showNotification(
            paste("Skipped", nrow(xl_skipped), "securities not in Scrip Master:",
                  paste(xl_skipped[[sec_col]], collapse = ", ")),
            type = "warning",
            duration = 8
          )
        }
    
        if (nrow(xl_valid) == 0) {
          showNotification("No valid securities found in Scrip Master!", type = "error")
          return()
        }
    
        # Clear existing rows
        current_rows <- active_rows()
        for (i in current_rows) {
          removeUI(selector = paste0("#", ns(paste0("row", i))))
        }
        active_rows(integer(0))
        count(0)
    
        # Get original security names from scrip master for display
        scrip_secs <- securities()
    
        n <- nrow(xl_valid)
    
        for (i in seq_len(n)) {
          # Match back to original casing from scrip master
          original_sec <- scrip_secs[tolower(trimws(gsub("\\s+", " ", scrip_secs))) == 
                                       xl_valid$normalized_sec[i]][1]
          alloc_val <- round(as.numeric(xl_valid[[alloc_col]][i]), 2)
      
          new_id <- count() + 1
          count(new_id)
          active_rows(c(active_rows(), new_id))
          add_row(new_id, sec_val = original_sec, alloc_val = alloc_val)
        }
    
        showNotification(
          paste(n, "companies uploaded successfully!"),
          type = "message"
        )
    
      }, error = function(e) {
        showNotification(paste("Upload error:", e$message), type = "error")
      })
    })
    
    loaded <- reactiveVal(FALSE)
    
    observeEvent(shared$device_id, {
  dev_id <- shared$device_id
  
  session$onFlushed(function() {
    saved_portfolio <- load_portfolio(dev_id)
    if (is.null(saved_portfolio)) return()
    if (isolate(loaded())) return()
    
    n <- nrow(saved_portfolio)
    isolate(count(n))
    isolate(active_rows(seq_len(n)))
    
    for (i in seq_len(n)) {
      sec   <- saved_portfolio$Security[i]
      alloc <- round(saved_portfolio$allocation[i] * 100, 2)
      add_row(i, sec_val = sec, alloc_val = alloc)
    }
    
    isolate(loaded(TRUE))
  }, once = TRUE)
}, once = TRUE)
    
    # 🔥 ADD BUTTON
    observeEvent(input$add, {
      new_id <- count() + 1
      count(new_id)
      
      active_rows(c(active_rows(), new_id))
      add_row(new_id)
    })
    
    # 🔥 AUTO NUMBERING
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
    
    # 🔥 LIVE PORTFOLIO
    live_portfolio <- reactive({
      
      rows <- active_rows()
      if (length(rows) == 0) return(NULL)
      
      data_list <- lapply(rows, function(i) {
        
        alloc <- safe_numeric(input[[paste0("Allocation", i)]])
        sec <- input[[paste0("Security", i)]]
        
        if (is.null(sec) || sec == "") return(NULL)
        
        data.frame(
          Security = sec,
          allocation = alloc / 100
        )
      })
      
      data_list <- Filter(Negate(is.null), data_list)
      if (length(data_list) == 0) return(NULL)
      
      df <- do.call(rbind, data_list)
      
      aggregate(allocation ~ Security, data = df, sum)
    })
    
    # 🔥 CASH CALCULATION (FIXED)
    output$cash_value <- renderText({
      
      rows <- active_rows()
      if (length(rows) == 0) return("100%")
      
      total <- sum(sapply(rows, function(i) {
        safe_numeric(input[[paste0("Allocation", i)]])
      }))
      
      cash <- max(0, 100 - total)
      
      if (total > 100) {
        paste0("⚠ ", round(100 - total, 2), "%")
      } else {
        paste0(round(cash, 2), "%")
      }
    })
    
    # 🔥 SAVE
    observeEvent(input$save, {
      req(shared$device_id)
      df_final <- live_portfolio()
      
      if (!is.null(df_final)) {
        save_portfolio(df_final, shared$device_id)
        showNotification("Portfolio saved!", type = "message")
      } else {
        showNotification("Nothing to save!", type = "warning")
      }
    })
    
    observe({
      shared$portfolio <- live_portfolio()
    })
  })
}
