library(shiny)
library(later)

scripMasterUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Scrip Master"),
    
    div(style = "max-width: 1600px; margin: auto;",
        fluidRow(
          column(4, br(), actionButton(ns("add"), "Add Company")),
          column(4, br(), actionButton(ns("save"), "Save Data"))
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
    
    if (file.exists("data/data.rds")) {
      saved_data <- readRDS("data/data.rds")
    }
    
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
          div(textInput(ns(paste0("mcap", i)),     NULL), class = "col-sm"),
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
    
    observe({
      if (is.null(saved_data)) return()
      if (loaded()) return()
      
      n <- nrow(saved_data)
      
      count(n)
      active_rows(seq_len(n))
      
      # 🔹 Step 1: create rows
      for (i in seq_len(n)) {
        add_row(i)
      }
      
      # 🔹 Step 2: delay updates (CRITICAL FIX)
      later::later(function() {
        for (i in seq_len(n)) {
          updateTextInput(session, paste0("isin", i), value = saved_data$ISIN[i])
          updateTextInput(session, paste0("security", i), value = saved_data$Security[i])
          updateTextInput(session, paste0("short", i), value = saved_data$Shortname[i])
          updateTextInput(session, paste0("sector", i), value = saved_data$Sector[i])
          updateTextInput(session, paste0("mcap", i), value = saved_data$Mcap[i])
        }
      }, delay = 0.1)
      
      loaded(TRUE)
    })
    
    # ADD BUTTON
    observeEvent(input$add, {
      new_id <- count() + 1
      count(new_id)
      active_rows(c(active_rows(), new_id))
      add_row(new_id)
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
      
      df <- live_data()
      
      if (!is.null(df)) {
        saveRDS(df, "data/data.rds")
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