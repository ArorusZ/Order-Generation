library(shiny)
library(dplyr)
library(DT)

ui <- fluidPage(
  titlePanel("order file"),
  DTOutput("final_table")
)

server <- function(input, output, session) {
  
  merged_data <- reactive({
    
    file1 <- readRDS("C:/Users/mahik/OneDrive/ドキュメント/Internship the second coming/project/data/data.rds")
    file2 <- readRDS("C:/Users/mahik/OneDrive/ドキュメント/Internship the second coming/project/data/ogw.rds")
    
    # Standardize column names (recommended)
    names(file1) <- tolower(names(file1))
    names(file2) <- tolower(names(file2))
    
    # Now use lowercase versions
    df1 <- file1 %>% select(security, isin)
    df2 <- file2 %>% select(security, qty)
    
    merged <- left_join(df1, df2, by = "security")
    
    merged %>%
      mutate(qty = ifelse(is.na(qty), 0, qty)) %>%
      distinct() %>%
      arrange(security)
  })
  
  output$final_table <- renderDT({
    datatable(merged_data(), options = list(pageLength = 10))
  })
}

shinyApp(ui, server)