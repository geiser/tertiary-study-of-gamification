
source(paste0(getwd(),'/lib/df2DT.R'))

rawDataUI <- function(id, links = list()) {
  ns <- NS(id)
  tabPanel(
    "Raw Data"
    , h3('Raw Data')
    , tabsetPanel(
      type = "tabs"
      , tabPanel("Zotero Items", verticalLayout(
        selectInput(ns("itemColnames"), "Columns"
                    , choices = c(
                      'itemID','key','citekey','citationKey','DOI','title','authors','date'
                      ,'itemType','publicationTitle','journalAbbreviation','conferenceName'
                      ,'place','series','publisher','language'
                      ,'ISSN','ISBN','volume','issue','pages'
                      ,'archive','uri','url','abstractNote')
                    , selected = c('key','title','authors','date','itemType')
                    , multiple =TRUE)
        , DT::DTOutput(ns("itemsDT"))))
      , tabPanel("PDF-annotations", verticalLayout(
        span("key and itemID are keys defined in the zotero items")
        , span("(see \"Zotero Items Tab\" to known where these annotations come from)")
        , DT::DTOutput(ns("annotationsDT"))))
      
      , tabPanel("Classification by Contexts"
                 , verticalLayout(a(href=links[['context']], paste0("Click here to acess: ", links[['context']]))
                                  , DT::DTOutput(ns("classificationByContextDT"))))
      , tabPanel("Classification by Review Types"
                 , verticalLayout(a(href=links[['type']], paste0("Click here to acess: ", links[['type']]))
                                  , DT::DTOutput(ns("classificationByTypeDT"))))
      , tabPanel("Classification by Review Objectives"
                 , verticalLayout(a(href=links[['objective']], paste0("Click here to acess: ", links[['objective']]))
                                  , DT::DTOutput(ns("classificationByRODT"))))
      , tabPanel("Classification by Review Questions"
                 , verticalLayout(a(href=links[['question']], paste0("Click here to acess: ", links[['question']]))
                                  , DT::DTOutput(ns("classificationByRQDT"))))
      )
  )
}

rawDataMD <- function(input, output, session, data) {
  
  output$itemsDT <- DT::renderDataTable({
    df2DT(data$items[,input$itemColnames])
  })
  
  output$annotationsDT <- DT::renderDataTable({
    df2DT(data$annotations[,c('ID','field','value','key','itemID')], pageLength = 50)
  })
  
  output$classificationByContextDT <- DT::renderDataTable({
    df2DT(data$classification$reviewsByContext$df)
  })
  
  output$classificationByTypeDT <- DT::renderDataTable({
    df2DT(data$classification$reviewsByType$df)
  })
  
  output$classificationByRODT <- DT::renderDataTable({
    df2DT(data$classification$reviewsByObjective$df)
  })
  
  output$classificationByRQDT <- DT::renderDataTable({
    df2DT(data$classification$reviewsByQuestion$df)
  })
  
}
