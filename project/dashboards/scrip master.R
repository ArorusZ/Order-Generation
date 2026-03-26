library(shiny)
source("C:/Users/mahik/OneDrive/ドキュメント/Internship the second coming/project/functions/data_handler.R")


ui <- fluidPage(
  titlePanel("scrip master"),
  
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
        column(4, br(), actionButton("add", "Add Company"))
      ),
      br(),

      fluidRow(
        class = "tight-row",
        column(1, strong("Sr.No")),
        column(2, strong("ISIN")),
        column(3, strong("Security name")),
        column(2, strong("Shortname")),
        column(2, strong("Sector")),
        column(1, strong("Mcap")),
        column(1, "")
      ),
      div(id = "company_rows")
  )
)

server <- function(input, output, session){
  count <- reactiveVal(0)
  active_rows <- reactiveVal(integer(0)) # Track which rows currently exist
  
  add_row <- function(i) {
    insertUI(
      selector = "#company_rows",
      where = "beforeEnd",
      ui = fluidRow(
        id = paste0("row", i),
        class = "tight-row",
        
        column(1, textOutput(paste0("num", i))),
        column(2, textInput(paste0("isin", i), NULL, placeholder = "ISIN")),
        column(3, textInput(paste0("security", i), NULL, placeholder = "Security")),
        column(2, textInput(paste0("short", i), NULL, placeholder = "Shortname")),
        column(2, textInput(paste0("sector", i), NULL, placeholder = "Sector")),
        column(1, textInput(paste0("mcap", i), NULL, placeholder = "mcap")),
        column(1, actionButton(paste0("remove", i), "Remove"))
      )
    )
    
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
  
  observe({
    rows <- active_rows()
    df <- get_data(input, rows)
    
    if (!is.null(df)) {
      saveRDS(df, "C:/Users/mahik/OneDrive/ドキュメント/Internship the second coming/project/data/data.rds")
    }
  })

}


shinyApp(ui, server)