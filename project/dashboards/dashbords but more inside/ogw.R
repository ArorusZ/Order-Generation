library(shiny)

portfolioUI <- function(id) {
  ns <- NS(id)
  
  tagList(
      
    h3("Share Allocation Calculator"),
    
    # Replace both tags$script blocks with this single one
    tags$script(HTML(sprintf("
      document.addEventListener('keyup', function(e) {
        if (e.key === 'Enter') {
          let el = document.activeElement;
          if (el && el.id) {
            Shiny.setInputValue('%s', el.id, {priority: 'event'});
          }
        }
      });
    ", ns("enter_pressed")))),
    
    div(style = "max-width: 1600px; margin: auto;",
        
        tags$style(HTML("
          .tight-row .col-sm-1, .tight-row .col-sm-2, .tight-row .col-sm-3 {
            padding-left: 2px !important;
            padding-right: 2px !important;
          }
          .tight-row .form-group { margin-bottom: 5px; }
          .shares-output { padding-top: 7px; font-weight: bold; }
        ")),
        
        fluidRow(
          column(4, numericInput(ns("amt"), "Total AUM", 100)),
          column(4, h4("Cash Value"), textOutput(ns("value"))),
          column(4, h4("Cash%"), textOutput(ns("Cash_per")))
        ),
        
        br(),
        
        actionButton(ns("save"), "Save Data"),
        
        br(), br(),
        
        div(class = "table-wrapper",
            fluidRow(
              div(
                class = "flex-row header-row",
                div("Sr.No", class = "col-sm"),
                div("Security", class = "col-lg"),
                div("CMP", class = "col-sm"),
                div("Target%", class = "col-sm"),
                div("Target Quantity", class = "col-sm"),
                div("Current Holding", class = "col-sm"),
                div("Current Holding%", class = "col-sm"),
                div("Deviation Qty", class = "col-sm"),
                div("Deviation Value", class = "col-sm"),
                div("Approved Qty", class = "col-sm"),
                div("Approved%", class = "col-sm"),
                div("Approved Value", class = "col-approved-val")
              )
            ),
            uiOutput(ns("company_rows"))
        )
    )
  )
}

portfolioServer <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    df <- reactive({
      req(shared$portfolio)
      shared$portfolio
    })
    
    # LOAD
    saved_calc <- NULL
    if (file.exists("data/ogw.rds")) {
      saved_calc <- readRDS("data/ogw.rds")
    }
    
    saved_lookup <- function(sec) {
      if (is.null(saved_calc)) return(NULL)
      row <- saved_calc[saved_calc$Security == sec, ]
      if (nrow(row) == 0) return(NULL)
      row
    }
    
    # RESTORE AUM
    observe({
      if (!is.null(saved_calc) && "amt" %in% colnames(saved_calc)) {
        updateNumericInput(session, "amt", value = saved_calc$amt[1])
      }
    })
    
    # UI ROWS
    output$company_rows <- renderUI({
      req(df())
      n <- nrow(df())
      
      tagList(
        lapply(1:n, function(i) {
          
          sec <- df()$Security[i]
          row_match <- saved_lookup(sec)
          
          cmp_val <- if (!is.null(row_match)) row_match$cmp else 0
          holding_val <- if (!is.null(row_match)) row_match$holding else 0
          qty_val <- if (!is.null(row_match)) row_match$qty else 0
          perc_val <- if (!is.null(row_match) && "approved_perc" %in% colnames(row_match)) row_match$approved_perc else 0
          
          div(
            class = "flex-row data-row",
            
            div(i, class = "col-sm"),
            div(sec, class = "col-lg"),
            
            div(numericInput(ns(paste0("cmp", i)), NULL, cmp_val), class = "col-sm"),
            div(round(df()$allocation[i] * 100, 2), class = "col-sm"),
            
            div(textOutput(ns(paste0("target_qty", i))), class = "col-sm"),
            
            div(numericInput(ns(paste0("holding", i)), NULL, holding_val), class = "col-sm"),
            
            div(textOutput(ns(paste0("holding_perc", i))), class = "col-sm"),
            
            div(textOutput(ns(paste0("dev_qty", i))), class = "col-sm"),
            div(textOutput(ns(paste0("dev_val", i))), class = "col-sm"),
            
            div(numericInput(ns(paste0("approved_qty", i)), NULL, qty_val), class = "col-sm"),
            div(numericInput(ns(paste0("approved_perc", i)), NULL, perc_val), class = "col-sm"),
            
            div(textOutput(ns(paste0("approved_val", i))), class = "col-approved-val")
          )
        })
      )
    })
    
    # CALCULATIONS (ONLY ONCE)
    observe({
      req(df(), input$amt)
      n <- nrow(df())
      
      for (i in 1:n) {
        local({
          idx <- i
          
          output[[paste0("target_qty", idx)]] <- renderText({
            req(input[[paste0("cmp", idx)]], input$amt)
            cmp <- input[[paste0("cmp", idx)]]
            alloc <- df()$allocation[idx]
            if (cmp > 0) round((input$amt * alloc) / cmp, 2) else 0
          })
          
          output[[paste0("holding_perc", idx)]] <- renderText({
            req(input[[paste0("holding", idx)]],
                input[[paste0("cmp", idx)]],
                input$amt)
            
            val <- input[[paste0("holding", idx)]] * input[[paste0("cmp", idx)]]
            round((val / input$amt) * 100, 2)
          })
          
          output[[paste0("dev_qty", idx)]] <- renderText({
            req(input[[paste0("cmp", idx)]],
                input[[paste0("holding", idx)]],
                input$amt)
            
            cmp <- input[[paste0("cmp", idx)]]
            alloc <- df()$allocation[idx]
            holding <- input[[paste0("holding", idx)]]
            
            if (cmp > 0) {
              target_qty <- (input$amt * alloc) / cmp
              round(abs(target_qty - holding), 2)
            } else 0
          })
          
          output[[paste0("dev_val", idx)]] <- renderText({
            req(input[[paste0("cmp", idx)]],
                input[[paste0("holding", idx)]],
                input$amt)
            
            cmp <- input[[paste0("cmp", idx)]]
            alloc <- df()$allocation[idx]
            holding <- input[[paste0("holding", idx)]]
            
            if (cmp > 0) {
              target_qty <- (input$amt * alloc) / cmp
              dev_qty <- target_qty - holding
              round(abs(dev_qty * cmp), 2)
            } else 0
          })
          
          output[[paste0("approved_val", idx)]] <- renderText({
            req(input[[paste0("approved_qty", idx)]],
                input[[paste0("cmp", idx)]])
            
            format(
              round(input[[paste0("approved_qty", idx)]] *
                      input[[paste0("cmp", idx)]], 2),
              big.mark = ",",
              scientific = FALSE
            )
          })
          
        })
      }
    })
    
    # 🔥 ENTER KEY LOGIC (ONLY ONE OBSERVER)
    observeEvent(input$enter_pressed, {
      
      req(input$enter_pressed)
      
      input_id <- input$enter_pressed
      
      idx <- as.numeric(gsub(".*(\\d+)$", "\\1", input_id))
      if (is.na(idx)) return()
      
      is_qty  <- grepl("approved_qty", input_id)
      is_perc <- grepl("approved_perc", input_id)
      
      cmp <- input[[paste0("cmp", idx)]]
      holding <- input[[paste0("holding", idx)]]
      alloc <- df()$allocation[idx]
      
      req(cmp, holding, input$amt)
      if (cmp <= 0) return()
      
      target_qty <- (input$amt * alloc) / cmp
      dev_qty <- target_qty - holding
      
      if (is_qty) {
        qty <- input[[paste0("approved_qty", idx)]]
        perc <- if (dev_qty != 0) (qty / abs(dev_qty)) * 100 else 0
        
        updateNumericInput(session,
                           paste0("approved_perc", idx),
                           value = round(perc, 2))
      }
      
      if (is_perc) {
        perc <- input[[paste0("approved_perc", idx)]]
        qty <- (perc / 100) * abs(dev_qty)
        
        updateNumericInput(session,
                           paste0("approved_qty", idx),
                           value = round(qty, 2))
      }
      
    })
    
    # CASH
    output$value <- renderText({
      req(df())
      total_used <- sum(df()$allocation, na.rm = TRUE)
      remaining_perc <- 1 - total_used
      
      format(round(input$amt * remaining_perc, 2),
             big.mark = ",", scientific = FALSE)
    })
    
    output$Cash_per <- renderText({
      req(df())
      total_used <- sum(df()$allocation) * 100
      round(100 - total_used, 2)
    })
    
    # SAVE DATA
    updated_df <- reactive({
      req(df(), input$amt)
      n <- nrow(df())
      
      df_new <- df()
      df_new$cmp <- sapply(1:n, function(i) input[[paste0("cmp", i)]] %||% 0)
      df_new$holding <- sapply(1:n, function(i) input[[paste0("holding", i)]] %||% 0)
      df_new$qty <- sapply(1:n, function(i) input[[paste0("approved_qty", i)]] %||% 0)
      df_new$approved_perc <- sapply(1:n, function(i) input[[paste0("approved_perc", i)]] %||% 0)
      df_new$amt <- input$amt
      
      df_new
    })
    
    observeEvent(input$save, {
      saveRDS(updated_df(), "data/ogw.rds")
      showNotification("Portfolio saved!", type = "message")
    })
    
    observe({
      shared$final <- updated_df()
    })
    
  })
}