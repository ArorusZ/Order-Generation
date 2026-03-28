library(shiny)

source("dashboards/dashbords but more inside/scrip master.R")
source("dashboards/dashbords but more inside/model portfolio.R")
source("dashboards/dashbords but more inside/ogw.R")
source("dashboards/dashbords but more inside/order_file.R")
source("functions/db_handler.R")
reset_tables() 

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "Style.css"),
    tags$script(HTML("
  function generateUUID() {
    return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
      var r = Math.random() * 16 | 0;
      var v = c === 'x' ? r : (r & 0x3 | 0x8);
      return v.toString(16);
    });
  }

  function getOrCreateDeviceID() {
    var name = 'device_id=';
    var cookies = document.cookie.split(';');
    for (var i = 0; i < cookies.length; i++) {
      var c = cookies[i].trim();
      if (c.indexOf(name) === 0) {
        return c.substring(name.length, c.length);
      }
    }
    var newID = generateUUID();
    var expires = new Date();
    expires.setFullYear(expires.getFullYear() + 10);
    document.cookie = name + newID + '; expires=' + expires.toUTCString() + '; path=/';
    return newID;
  }

  // Try multiple events to make sure it fires
  function sendDeviceID() {
    var deviceID = getOrCreateDeviceID();
    console.log('Device ID:', deviceID);
    if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
      Shiny.setInputValue('device_id', deviceID, {priority: 'event'});
    }
  }

  document.addEventListener('DOMContentLoaded', sendDeviceID);
  $(document).on('shiny:connected', sendDeviceID);
  setTimeout(sendDeviceID, 2000);
"))
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

  # Capture device ID as soon as it arrives from browser
  observeEvent(input$device_id, {
    shared$device_id <- input$device_id
    message("Device ID: ", input$device_id)
  })
  
  scripMasterServer("sm1", shared)
  modelPortfolioServer("mp1", shared)
  portfolioServer("p1", shared)
  o_fServer("s1", shared)
  
}
shinyApp(ui, server)

