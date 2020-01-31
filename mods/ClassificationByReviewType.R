
ClassificationByReviewTypeUI <- function(id) {
  ns <- NS(id)
  
  tagList(fluidRow(
    column(2, verticalLayout(
      h3(em(strong("Filtering ")))
    ))
    , column(10, tabsetPanel(
      type = "tabs"
      , tabPanel("Table", shinycssloaders::withSpinner(DT::DTOutput(ns("table"))))
      , tabPanel("Pivot Table", shinycssloaders::withSpinner(DT::DTOutput(ns("ptable"))))
      , tabPanel("Latex", uiOutput(ns("fields")), verbatimTextOutput(ns("latex")))
      ))
  ))

    #sidebarLayout(
    #sidebarPanel(width=3
    #             , , br()
    #             , uiOutput("filterTextDim")
    #             , uiOutput("filterSelectType"), uiOutput("filterSelectType2")
    # br()
    #             , verticalLayout(
    #             ), br()
    #             , uiOutput("filterSliderPY"), uiOutput("filterSliderTC")
    #)
}

ClassificationByReviewType <- function(input, output, session, df) {
  ns <- session$ns
  
  output$table <- DT::renderDataTable({
    df
  }, options = list(
    pageLength = -1, dom = 'Bfrtip'
    , buttons = c('pageLength','copy', 'csv', 'excel', 'pdf', 'print')
    , lengthMenu = list(c(25,50,100,-1), c('25 rows', '50 rows', '100 rows','Show all'))
    , columnDefs = list(list(className = 'dt-center'))
  ))
  
  output$ptable <- DT::renderDataTable({
    df
  }, options = list(
    pageLength = -1, dom = 'Bfrtip'
    , buttons = c('pageLength','copy', 'csv', 'excel', 'pdf', 'print')
    , lengthMenu = list(c(25,50,100,-1), c('25 rows', '50 rows', '100 rows','Show all'))
    , columnDefs = list(list(className = 'dt-center'))
  ))
  
  output$latex <- renderText({
    filename <- paste0(getwd(), '/tmp/', digest::digest(df), '.tex')
    Hmisc::latex(df, longtable = T, ctable=F, landscape = F, rowlabel = "", where='!htbp', file = filename, rowname = NULL, append = F)
    paste(readLines(filename), collapse = '\n')
  })
  
}
