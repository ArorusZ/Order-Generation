library(shiny)
df <- readRDS("C:/Users/mahik/OneDrive/ドキュメント/Internship the second coming/project/data/portfolio.rds")

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
    
    .flex-row {
      display: flex;
      gap: 10px;
      align-items: center;
      margin-bottom: 5px;
    }
    
    .col-sm {
      flex: 1;
      min-width: 80px;
    }
    
    .col-lg {
      flex: 2;
      min-width: 150px;
    }
    
    .data-row input {
      width: 100%;
    }
    
  "))
  ),
  
  titlePanel("Share Allocation Calculator"),
  
  div(style = "max-width: 800px; margin: auto;",
      tags$style(HTML("
        .tight-row .col-sm-1, .tight-row .col-sm-2, .tight-row .col-sm-3 {
          padding-left: 2px !important;
          padding-right: 2px !important;
        }
        .tight-row .form-group { margin-bottom: 5px; }
        .shares-output { padding-top: 7px; font-weight: bold; }
      ")),
      
      fluidRow(
        column(4, numericInput("amt", "Total AUM", 100)),
        column(4, h4("Cash Value"), textOutput("value")),
        column(4, h4("Cash%"), textOutput("Cash_per") )
      ),
      
      br(),
      
      actionButton("save", "Save Data"),
      
      br(), br(),
      
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
          div("Approved Value", class = "col-sm"),
        )
      ),
      uiOutput("company_rows")
  )
)

server <- function(input, output, session) {
  
  # Render all rows at once
  output$company_rows <- renderUI({
    
    n <- nrow(df)
    
    tagList(
      lapply(1:n, function(i) {
        
        div(
          class = "flex-row data-row",
          
          div(i, class = "col-sm"),
          
          div(df$Security[i], class = "col-lg"),
          
          div(numericInput(paste0("cmp", i), NULL, 0), class = "col-sm"),
          
          div(df$allocation[i], class = "col-sm"),
          
          div(textOutput(paste0("target_qty", i)), class = "col-sm"),
          
          div(numericInput(paste0("holding", i), NULL, 0), class = "col-sm"),
          
          div(textOutput(paste0("holding_perc", i)), class = "col-sm"),
          
          div(textOutput(paste0("dev_qty", i)), class = "col-sm"),
          
          div(textOutput(paste0("dev_val", i)), class = "col-sm"),
          
          div(numericInput(paste0("approved_qty", i), NULL, 0), class = "col-sm"),
          
          div(textOutput(paste0("approved_perc", i)), class = "col-sm"),
          
          div(textOutput(paste0("approved_val", i)), class = "col-sm")
        )
      })
    )
  })
    
  observe({
    
    n <- nrow(df)
    
    for (i in 1:n) {
      local({
        idx <- i
        
        # Target Quantity
        output[[paste0("target_qty", idx)]] <- renderText({
          req(input[[paste0("cmp", idx)]])
          
          cmp <- input[[paste0("cmp", idx)]]
          alloc <- df$allocation[idx]
          total <- 
            
          if (cmp > 0) {
            round((input$amt * alloc) / cmp, 2)
          } else {
            0
          }
        })
        
        
        # Holding %
        output[[paste0("holding_perc", idx)]] <- renderText({
          req(input[[paste0("holding", idx)]], input[[paste0("cmp", idx)]])
          
          val <- input[[paste0("holding", idx)]] * input[[paste0("cmp", idx)]]
          round((val / input$amt) * 100, 2)
        })
        
        
        # Deviation Qty
        output[[paste0("dev_qty", idx)]] <- renderText({
          req(input[[paste0("cmp", idx)]], input[[paste0("holding", idx)]])
          
          cmp <- input[[paste0("cmp", idx)]]
          alloc <- df$allocation[idx]
          holding <- input[[paste0("holding", idx)]]
          
          if (cmp > 0) {
            target_qty <- (input$amt * alloc / 100) / cmp
            round(abs(target_qty - holding), 2)
          } else {
            0
          }
        })
        
        
        # Deviation Value
        output[[paste0("dev_val", idx)]] <- renderText({
          req(input[[paste0("cmp", idx)]], input[[paste0("holding", idx)]])
          
          cmp <- input[[paste0("cmp", idx)]]
          alloc <- df$allocation[idx]
          holding <- input[[paste0("holding", idx)]]
          
          if (cmp > 0) {
            target_qty <- (input$amt * alloc / 100) / cmp
            dev_qty <- target_qty - holding
            round(abs(dev_qty * cmp), 2)
          } else {
            0
          }
        })
        
        
        # Approved %
        output[[paste0("approved_perc", idx)]] <- renderText({
          req(input[[paste0("approved_qty", idx)]],
              input[[paste0("cmp", idx)]],
              input[[paste0("holding", idx)]])
          
          cmp <- input[[paste0("cmp", idx)]]
          holding <- input[[paste0("holding", idx)]]
          approved_qty <- input[[paste0("approved_qty", idx)]]
          alloc <- df$allocation[idx]
          
          if (cmp > 0) {
            
            # Calculate deviation qty
            target_qty <- (input$amt * alloc / 100) / cmp
            dev_qty <- target_qty - holding
            
            if (dev_qty == 0) {
              return("0%")
            }
            
            perc <- (approved_qty / dev_qty) * 100
            
            paste0(abs(round(perc, 2)), "%")
            
          } else {
            "0%"
          }
        })
        
        
        # Approved Value
        output[[paste0("approved_val", idx)]] <- renderText({
          req(input[[paste0("approved_qty", idx)]], input[[paste0("cmp", idx)]])
          
          format(round(input[[paste0("approved_qty", idx)]] * input[[paste0("cmp", idx)]], 2),big.mark = ",",scientific = FALSE)
        })
      })
    }
  })
  
  output$value <- renderText({
    total_used <- sum(df$allocation, na.rm = TRUE)
    remaining_perc <- 100 - total_used*100
    
    format(round(input$amt * (remaining_perc / 100), 2),big.mark = ",",scientific = FALSE)
  })
  
  output$Cash_per <- renderText({
    total_used <- sum(df$allocation, na.rm = TRUE)*100
    round(100 - total_used, 2)
  })
  
  updated_df <- reactive({
    
    n <- nrow(df)
    
    qty <- sapply(1:n, function(i) {
      val <- input[[paste0("approved_qty", i)]]
      if (is.null(val)) 0 else val
    })
    
    df_new <- df
    df_new$qty <- qty
    
    df_new
  })
  
  observeEvent(input$save, {
    
    saveRDS(updated_df(), "C:/Users/mahik/OneDrive/ドキュメント/Internship the second coming/project/data/ogw.rds")
    
  })
  
}

shinyApp(ui, server)