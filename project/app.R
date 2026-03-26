library(shiny)

source("dashboards/dashbords but more inside/scrip master.R")
source("dashboards/dashbords but more inside/model portfolio.R")
source("dashboards/dashbords but more inside/ogw.R")
source("dashboards/dashbords but more inside/order_file.R")

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "Style.css")
  ),
  tabsetPanel(
    tabPanel("Scrip Master", scripMasterUI("sm1")),
    tabPanel("Model Portfolio", modelPortfolioUI("mp1")),
    tabPanel("Order Generation", portfolioUI("p1")),
    tabPanel("Order File", o_fUI("s1"))
  )
)

server <- function(input, output, session) {
  
  shared <- reactiveValues(
    scrip = NULL,
    portfolio = NULL,
    final = NULL
  )
  
  scripMasterServer("sm1", shared)
  modelPortfolioServer("mp1", shared)
  portfolioServer("p1", shared)
  o_fServer("s1", shared)
  
}
shinyApp(ui, server)

