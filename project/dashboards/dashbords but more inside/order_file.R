library(shiny)
library(dplyr)
library(DT)
library(openxlsx)

o_fUI <- function(id){
  ns <- NS(id)
  
  fluidPage(
  titlePanel("Order File"),
  downloadButton(ns("download"), "Download Order File"),
  DTOutput(ns("final_table"))
  )
}

o_fServer <- function(id, shared) {
  moduleServer(id, function(input, output, session){
    
    merged_data <- reactive({
      
      req(shared$scrip, shared$final)
      
      file1 <- shared$scrip
      file2 <- shared$final
      
      # Standardize column names
      names(file1) <- tolower(names(file1))
      names(file2) <- tolower(names(file2))
      
      df1 <- file1 %>% select(security, isin)
      df2 <- file2 %>% select(security, qty)
      
      merged <- left_join(df1, df2, by = "security")
      
      merged %>%
        mutate(qty = ifelse(is.na(qty), 0, qty)) %>%
        distinct() %>%
        arrange(security)
    })
    
    output$final_table <- renderDT({
      req(merged_data())
      datatable(merged_data(), options = list(pageLength = 10))
    })
    
    # ✅ STORE instead of return
    observe({
      shared$order_file <- merged_data()
    })

    output$download <- downloadHandler(
      filename = function() {
        paste0("order_file_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        req(merged_data())
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Order File")
        openxlsx::writeData(wb, "Order File", merged_data())
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
    
  })
}
