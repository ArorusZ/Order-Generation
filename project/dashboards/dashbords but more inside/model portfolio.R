library(shiny)

modelPortfolioUI <- function(id) { 
  ns <- NS(id) 
  
  tagList( 
    h3("Model Portfolio"), 
    
    div(style = "max-width: 1600px; margin: auto;", 
        
        fluidRow( 
          column(4, br(), actionButton(ns("add"), "Add Company")), 
          column(4, br(), actionButton(ns("save"), "Save Portfolio")) 
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
    
    saved_portfolio <- NULL

    # REPLACE LOAD WITH
    source("functions/db_handler.R")
    saved_data <- load_portfolio()
    
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
        
        # 🔥 CURRENT VALUE (most important)
        current <- input[[paste0("Security", i)]]
        
        # fallback to saved value
        selected_val <- if (!is.null(current)) current else sec_val
        
        # ensure selected value exists in choices
        if (!is.null(selected_val) && !(selected_val %in% choices)) {
          choices <- c(choices, selected_val)
        }
        
        selectInput(
          ns(paste0("Security", i)),
          NULL,
          choices = choices,
          selected = selected_val,   # ✅ FIXED
          selectize = FALSE
        )
      })
      
      observeEvent(input[[paste0("remove", i)]], {
        removeUI(selector = paste0("#", ns(paste0("row", i))))
        active_rows(setdiff(active_rows(), i))
      }, ignoreInit = TRUE)
    }
    
    loaded <- reactiveVal(FALSE)
    
    observeEvent(shared$device_id, {
      saved_portfolio <- load_portfolio(shared$device_id)
      
      if (is.null(saved_portfolio)) return()
      req(securities())
      if (loaded()) return()
      
      n <- nrow(saved_portfolio)
      
      count(n)
      active_rows(seq_len(n))
      
      for (i in seq_len(n)) {
        
        sec <- saved_portfolio$Security[i]
        alloc <- round(saved_portfolio$allocation[i] * 100, 2)
        
        # ✅ PASS VALUES DIRECTLY
        add_row(i, sec_val = sec, alloc_val = alloc)
      }
      
      loaded(TRUE)
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
      df_final <- live_portfolio()
      
      if (!is.null(df_final)) {
        save_portfolio(df_final)
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
