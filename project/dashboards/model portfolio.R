library(shiny)

ui<- fluidPage(
  titlePanel("Model Portfolio"),
  
  div(style = "max-width: 800px; margin: auto;",
      tags$style(HTML("
      .tight-row .row {
        display: flex;
        align-items: center;  
      }

      .tight-row .form-group {
        margin-bottom: 0;
      }

      .tight-row .btn {
        height: 38px;
      }

      .tight-row .form-control {
        height: 38px;
      }

      .tight-row .selectize-input {
        height: 38px !important;
        padding: 6px 12px;
      }
    ")),
      fluidRow(
        column(4, br(), actionButton("add", "Add Company")),
        column(4, br(), actionButton("save", "Save Portfolio"))
      ),
      br(),
      
      fluidRow(
        class = "tight-row",
        column(1, strong("Sr.No")),
        column(3, strong("Security name")),
        column(2, strong("Allocation(%)")),
        column(1, "")
      ),
      div(id = "company_rows"),
      fluidRow(
        class = "tight-row",
        
        column(1, ""),  # empty Sr.No
        
        column(3, strong("Cash")),
        
        column(2, textOutput("cash_value")),
        
        column(1, "")
      )
  )
)

server <- function(input, output, session){
  
  data <- readRDS("C:/Users/mahik/OneDrive/ドキュメント/Internship the second coming/project/data/data.rds")
  securities <- unique(data$Security)
  
  count <- reactiveVal(0)
  active_rows <- reactiveVal(integer(0)) # Track which rows currently exist
  
  add_row <- function(i) {
    insertUI(
      selector = "#company_rows",
      where = "beforeEnd",
      ui = div(
        id = paste0("row", i),   
        
        fluidRow(
          class = "tight-row",
          
          column(1, textOutput(paste0("num", i))),
          
          column(3, selectInput(   
            paste0("Security", i),
            NULL,
            choices = securities,
            selectize = FALSE
          )),
          
          column(2, textInput(
            paste0("Allocation", i),
            NULL,
            placeholder = "Allocation"
          )),
          
          column(1, actionButton(paste0("remove", i), "Remove"))
        )
      )
    )
    
    observe({
      lapply(active_rows(), function(i) {
        input[[paste0("Allocation", i)]]
      })
    })
    
    observeEvent(input[[paste0("remove", i)]], {
      removeUI(selector = paste0("#row", i))
      active_rows(setdiff(active_rows(), i)) 
    })
  }
  
  observeEvent(input$add, {
    new_id <- count() + 1
    count(new_id)
    active_rows(c(active_rows(), new_id))
    add_row(new_id)
  })
  
  observe({
    rows <- active_rows()
    
    for (j in seq_along(rows)) {
      local({
        idx <- rows[j]
        num <- j
        
        output[[paste0("num", idx)]] <- renderText({
          num
        })
      })
    }
  })
  
  output$cash_value <- renderText({
    
    rows <- active_rows()
    
    if (length(rows) == 0) return("100%")
    
    total <- sum(sapply(rows, function(i) {
      val <- as.numeric(input[[paste0("Allocation", i)]])
      if (is.na(val)) 0 else val
    }))
    
    cash <- max(0, 100 - total)
    
    if (total > 100) {
      paste0("⚠ ", round(100 - total, 2), "%")
    } else {
      paste0(round(cash, 2), "%")
    }
  })
  
  observeEvent(input$save, {
    
    req(length(active_rows()) > 0)  # ensure rows exist
    
    data_list <- lapply(active_rows(), function(i) {
      
      alloc <- as.numeric(input[[paste0("Allocation", i)]])
      sec <- input[[paste0("Security", i)]]
      
      # validation
      if (is.na(alloc) || sec == "") return(NULL)
      
      data.frame(
        Security = sec,
        allocation = alloc / 100
      )
      
    })
    
    data_list <- Filter(Negate(is.null), data_list)
    
    req(length(data_list) > 0)
    
    df <- do.call(rbind, data_list)
    
    df_final <- aggregate(allocation ~ Security, data = df, sum)
    
    saveRDS(
      df_final,
      "C:/Users/mahik/OneDrive/ドキュメント/Internship the second coming/project/data/portfolio.rds"
    )
    
    showNotification("Portfolio saved!", type = "message")  
    
  })
}

shinyApp(ui, server)