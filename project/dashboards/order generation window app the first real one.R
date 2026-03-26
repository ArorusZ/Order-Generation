library(shiny)

ui <- fluidPage(
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
        column(4, numericInput("amt", "Total Amount", 100)),
        column(4, h4("Total Amount Remaining"), textOutput("remaining")),
        column(4, br(), actionButton("add", "Add Company"))
      ),
      br(),
      fluidRow(
        class = "tight-row",
        column(3, strong("Company")),
        column(2, strong("Share Price")),
        column(2, strong("Percentage")),
        column(2, strong("Shares")),
        column(2, "")
      ),
      div(id = "company_rows")
  )
)

server <- function(input, output, session) {
  
  count <- reactiveVal(0)
  active_rows <- reactiveVal(integer(0)) # Track which rows currently exist
  
  add_row <- function(i) {
    insertUI(
      selector = "#company_rows",
      where = "beforeEnd",
      ui = fluidRow(
        id = paste0("row", i),
        class = "tight-row",
        column(3, textInput(paste0("name", i), NULL, placeholder = "Name")),
        column(2, numericInput(paste0("price", i), NULL, 0)),
        column(2, numericInput(paste0("percent", i), NULL, 0)),
        column(2, div(class = "shares-output", textOutput(paste0("shares", i)))),
        column(2, actionButton(paste0("remove", i), "Remove"))
      )
    )
    
    # INDIVIDUAL CALCULATION FOR THIS ROW
    output[[paste0("shares", i)]] <- renderText({
      req(input[[paste0("price", i)]], input[[paste0("percent", i)]])
      price <- input[[paste0("price", i)]]
      percent <- input[[paste0("percent", i)]]
      
      if (price > 0) {
        # Calculate: (Total * Percent / 100) / Price
        val <- (input$amt * (percent / 100)) / price
        round(val, 2)
      } else {
        0
      }
    })
    
    observeEvent(input[[paste0("remove", i)]], {
      removeUI(selector = paste0("#row", i))
      active_rows(setdiff(active_rows(), i)) # Remove from active tracking
    })
  }
  
  observeEvent(input$add, {
    new_id <- count() + 1
    count(new_id)
    active_rows(c(active_rows(), new_id))
    add_row(new_id)
  })
  
  # REMAINING BALANCE CALCULATION
  output$remaining <- renderText({
    # If no rows are active, the used amount is 0
    if (length(active_rows()) == 0) {
      return(round(input$amt, 2))
    }
    
    total_used <- sum(sapply(active_rows(), function(i) {
      percent <- input[[paste0("percent", i)]]
      # Return 0 if the input hasn't initialized yet to avoid errors
      if (is.null(percent) || is.na(percent)) 0 else (input$amt * (percent / 100))
    }))
    
    round(input$amt - total_used, 2)
  })
}

shinyApp(ui, server)