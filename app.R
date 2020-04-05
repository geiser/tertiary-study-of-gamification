
library(shiny)
library(plotly)
library(shinyTree)
library(rpivotTable)

options(stringsAsFactors=F)

source('lib/df2DT.R')
source('lib/initValue.R')
source('modules/rawData.R')
source('modules/mappingData.R')


source('lib/addAnnotations.R')
source('lib/addClassification.R')
source('lib/changeItem2keyDF.R')

if (!dir.exists(paste0(getwd(), '/tmp'))) {
  dir.create(paste0(getwd(), '/tmp'))
}

### UI ###

ui <- navbarPage(
  "Tertiary Study: Gamification"
  , navbarMenu(title = "Source", rawDataUI('rawData', links = list(
    context = 'https://bit.ly/2wPdeu6', type = 'https://bit.ly/2wWoXa9'
    , objective = 'https://bit.ly/343i2rX', question = 'https://bit.ly/39FElVA'))
    , tabPanel("Processed data", DT::DTOutput("dataDT")))
  , navbarMenu(
    title = "Mapping ..."
    , " ", " ", "Basic mapping"
    , mappingDataUI('mapByPubVenue', 'publication venues', 'Mapping by publication venues')
    , mappingDataUI('mapByContext', 'contexts', 'Mapping by contexts')
    , mappingDataUI('mapByReviewType', 'review types', 'Mapping by review types')
    , mappingDataUI('mapByReviewObjective', 'review objectives', 'Mapping by review objectives')
    , mappingDataUI('mapByReviewQuestion', 'review questions', 'Mapping by review questions')
    , mappingDataUI('mapByTypeSelectedStudy', 'type of selected studies', 'Mapping by type of selected studies')
    , mappingDataUI('mapByItemType', 'item types (Zotero)', 'Mapping by item types (based on Zotero)')
    #, mappingDataUI('mapByPublicationTitle', 'publication title', 'Mapping by publication titles (conf/journal where was published)')
    
    , " ", " ", "Mapping number of selected studies"
    , mappingDataUI('mapNumSelectedStudiesByContext', 'number of selected studies by contexts'
                    , 'Mapping number of selected studies by contexts')
    , mappingDataUI('mapNumSelectedStudiesByReviewType', 'number of selected studies by review types'
                    , 'Mapping number of selected studies by review types')
    , mappingDataUI('mapNumSelectedStudiesByReviewObjective', 'number of selected studies by review objectives'
                    , 'Mapping number of selected studies by review objectives')
    , mappingDataUI('mapNumSelectedStudiesByReviewQuestion', 'number of selected studies by review questions'
                    , 'Mapping number of selected studies by review questions')
    , mappingDataUI('mapNumSelectedStudiesByTypeSelectedStudy', 'number of selected studies by type of selected studies'
                    , 'Mapping number of selected studies by type of selected studies')
    , mappingDataUI('mapNumSelectedStudiesByItemType', 'number of selected studies by item types (Zotero)'
                    , 'Mapping number of selected studies by item types (based on Zotero)')
    #, mappingDataUI('mapNumSelectedStudiesByPublicationTitle', 'number of selected studies by item types'
    #                , 'Mapping number of selected studies by publication titles (conf/journal where was published)')
    
    , " ", " ", tabPanel('explore by yourself', h3('Explore mapping by yourself'),  uiOutput('pivotPanel'))
  )
)

###

rawData <- list(
  items = initValue('items', 'src-data/zotero.json')
  , annotations = initValue('annotations', 'src-data/zotero.json', T, 'src-data/annotations.rds')
  , classification = list(
    reviewsByContext = initValue('reviewsByContext', 'src-data/mom/context.xml')
    , reviewsByType = initValue('reviewsByType', 'src-data/mom/type.xml')
    , reviewsByObjective = initValue('reviewsByReviewObjective', 'src-data/mom/review-objective.xml')
    , reviewsByQuestion = initValue('reviewsByReviewQuestion', 'src-data/mom/review-question.xml')
  )
)

### SERVER LOGIC ###

server <- function(input, output, session) {
  
  df2list <- function(df, parent = NA) {
    df <- df[!df$isLeaf,]
    toReturn <- list()
    
    children <- df$item[df$parent == parent]
    children <- children[!is.na(children)]
    if (is.na(parent)) {
      children <- df$item[is.na(df$parent)]
    }
    for (child in children) {
      toReturn[[child]] <- df2list(df, parent = child)
      if (is.null(toReturn[[child]])) {
        toReturn[[child]] <- child
      }
    }
    
    return(toReturn)
  }
  
  callModule(rawDataMD, "rawData", rawData)
  
  data <- reactive({
    md5file <- paste0('tmp/data_', as.character(digest::digest(rawData)), '.rds')
    if (file.exists(md5file)) {
      df <- SemNetCleaner::read.data(file = md5file)
    } else {
      df <- rawData$items[,c('key','citekey','citationKey','itemType', 'title', 'authors'
                             ,'publicationTitle', 'journalAbbreviation', 'conferenceName'
                             , 'date', 'archive', 'libraryCatalog')]
      
      notes <- rawData$annotations
      df <- addAnnotations(df, notes, 'Cited by')
      df <- addAnnotations(df, notes, 'Paper type')
      df <- addAnnotations(df, notes, 'Number of selected studies')
      df <- addAnnotations(df, notes, 'Type of selected studies', T)
      
      df <- addClassification(df, rawData$classification$reviewsByContext, 'Context', T)
      df <- addClassification(df, rawData$classification$reviewsByType, 'Review type', F)
      
      rawData$classification$reviewsByObjective$df <- changeItem2keyDF(
        rawData$classification$reviewsByObjective$df, rawData$items, notes=notes, field = 'RO')
      classification <- rawData$classification$reviewsByObjective
      df <- addClassification(df, classification, 'Review objective', F)
      
      rawData$classification$reviewsByQuestion$df <- changeItem2keyDF(
        rawData$classification$reviewsByQuestion$df, rawData$items, notes=notes, field = 'RQ')
      classification <- rawData$classification$reviewsByQuestion
      df <- addClassification(df, classification, 'Review question', F)
      
      if (!file.exists(md5file)) saveRDS(df, file = md5file)
    }
    df
  })
  
  output$dataDT <- DT::renderDataTable({ df2DT(data()) })
  
  trees <- list(
    `Review type` = df2list(rawData$classification$reviewsByType$df)
    , `Review objective` = df2list(rawData$classification$reviewsByObjective$df)
    , `Review question` = df2list(rawData$classification$reviewsByQuestion$df)
    , `Context` = df2list(rawData$classification$reviewsByContext$df))
  
  pctExpression <- paste0("(100*values$val)/", length(unique(rawData$items$key)))
  
  ##
  
  callModule(mappingDataMD, "mapByPubVenue", data(), 'Paper type', trees
             , list(filterValues = c('Journal article'
                                     , 'Conference (full-paper)', 'Conference (book chapter)'
                                     , 'Workshop', 'Workshop (book chapter)', 'Book chapter'))
             , pctExpression=pctExpression)
  
  callModule(mappingDataMD, "mapByContext", data(), 'Context', trees, list(
    filterValues = c('Education', 'Business, Marketing, Enterprise and Services'
                     , 'Software Engineering', 'Information Systems', 'Crowsourcing'
                     , 'Health', 'Education + Business, Marketing, Enterprise and Services'
                     , 'Education + Software Engineering', 'Education + Information Systems'
                     , 'Education + Health', 'Environment and Society'
                     , 'Education + Environment and Society'
                     , 'Urban, Social and Political'
                     , 'Without context')
    , removeDuplicateSort = c('Education + Environment and Society', 'Education + Health'
                              , 'Education + Software Engineering', 'Education + Information Systems'
                              , 'Education + Business, Marketing, Enterprise and Services'
                              , 'Software Engineering', 'Information Systems', 'Crowsourcing'
                              , 'Business, Marketing, Enterprise and Services'
                              , 'Education', 'Health', 'Environment and Society'
                              , 'Urban, Social and Political'
                              , 'Without context'))
    , pctExpression=pctExpression)
  
  callModule(mappingDataMD, "mapByReviewType", data(), 'Review type', trees, list(
    filterValues = c('Narrative Review', 'Mapping Review', 'Meta-analysis', 'Systematic Review'
                     , 'Scoping Review', 'Critical Review')
    , removeDuplicateSort = c('Systematic Review', 'Meta-analysis', 'Aggregative Review'
                              , 'Critical Review', 'Mapping Review', 'Narrative Review', 'Scoping Review'))
    , pctExpression=pctExpression)
  
  callModule(mappingDataMD, "mapByReviewObjective", data(), 'Review objective', trees, list(
    filterValues = c('ROs about gamification', 'ROs about the game-related approaches'
                     , 'ROs about models/frameworks related to gamification'
                     , 'ROs about the gamification analytics')
    , removeDuplicateSort = c())
    , pctExpression=pctExpression)
  
  callModule(mappingDataMD, "mapByReviewQuestion", data(), 'Review question', trees, list(
    filterValues = c('RQs about the gamification','RQs about the game-related approaches'
                     , 'RQs about the gamification models/frameworks'
                     , 'RQs about the gamification analytics')
    , removeDuplicateSort = c())
    , pctExpression=pctExpression)
  
  callModule(mappingDataMD, "mapByTypeSelectedStudy", data(), 'Type of selected studies', trees, list(
    filterValues = c('empirical', 'non-empirical', 'empirical + non-empirical')
    , removeDuplicateSort = c('empirical + non-empirical', 'empirical', 'non-empirical'))
    , pctExpression=pctExpression)
  
  callModule(mappingDataMD, "mapByItemType", data(), 'itemType', trees, pctExpression=pctExpression)
  
  ##
  
  callModule(mappingDataMD, "mapNumSelectedStudiesByPubVenue", data(), 'Paper type', trees
             , list(filterValues = c('Journal article'
                                     , 'Conference (full-paper)', 'Conference (book chapter)'
                                     , 'Workshop', 'Workshop (book chapter)', 'Book chapter'))
             , pctExpression=pctExpression, numericField = 'Number of selected studies')
  
  callModule(mappingDataMD, "mapNumSelectedStudiesByContext", data(), 'Context', trees, list(
    filterValues = c('Education', 'Business, Marketing, Enterprise and Services'
                     , 'Software Engineering', 'Information Systems', 'Crowsourcing'
                     , 'Health', 'Education + Business, Marketing, Enterprise and Services'
                     , 'Education + Software Engineering', 'Education + Information Systems'
                     , 'Education + Health', 'Environment and Society'
                     , 'Education + Environment and Society'
                     , 'Urban, Social and Political'
                     , 'Without context')
    , removeDuplicateSort = c('Education + Environment and Society', 'Education + Health'
                              , 'Education + Software Engineering', 'Education + Information Systems'
                              , 'Education + Business, Marketing, Enterprise and Services'
                              , 'Software Engineering', 'Information Systems', 'Crowsourcing'
                              , 'Business, Marketing, Enterprise and Services'
                              , 'Education', 'Health', 'Environment and Society'
                              , 'Urban, Social and Political'
                              , 'Without context'))
    , pctExpression=pctExpression, numericField = 'Number of selected studies')
  
  callModule(mappingDataMD, "mapNumSelectedStudiesByReviewType", data(), 'Review type', trees, list(
    filterValues = c('Mapping Review', 'Aggregative Review', 'Systematic Review', 'Meta-analysis'
                     , 'Narrative Review', 'Scoping Review', 'Critical Review')
    , removeDuplicateSort = c('Systematic Review', 'Meta-analysis', 'Aggregative Review'
                              , 'Critical Review', 'Mapping Review', 'Narrative Review', 'Scoping Review'))
    , pctExpression=pctExpression, numericField = 'Number of selected studies')
  
  callModule(mappingDataMD, "mapNumSelectedStudiesByReviewObjective", data(), 'Review objective', trees, list(
    filterValues = c('ROs about gamification', 'ROs about the game-related approaches'
                     , 'ROs about models/frameworks related to gamification'
                     , 'ROs about the gamification analytics')
    , removeDuplicateSort = c())
    , pctExpression=pctExpression, numericField = 'Number of selected studies')
  
  callModule(mappingDataMD, "mapNumSelectedStudiesByReviewQuestion", data(), 'Review question', trees, list(
    filterValues = c('RQs about the gamification','RQs about the game-related approaches'
                     , 'RQs about the gamification models/frameworks'
                     , 'RQs about the gamification analytics')
    , removeDuplicateSort = c())
    , pctExpression=pctExpression, numericField = 'Number of selected studies')
  
  callModule(mappingDataMD, "mapNumSelectedStudiesByTypeSelectedStudy", data(), 'Type of selected studies', trees, list(
    filterValues = c('empirical', 'non-empirical', 'empirical + non-empirical')
    , removeDuplicateSort = c('empirical + non-empirical', 'empirical', 'non-empirical'))
    , pctExpression=pctExpression, numericField = 'Number of selected studies')
  
  callModule(mappingDataMD, "mapNumSelectedStudiesByItemType", data(), 'itemType', trees
             , pctExpression=pctExpression, numericField = 'Number of selected studies')
  
  ##
  
  output$pivotPanel <- renderUI({
    withProgress(message = 'Loading pivotal table', detail = 'This may take a while ...', {
      verticalLayout(
        fluidRow(
          span("- To obtain more help about how to use pivottable in R, Click here: ")
          , a(href='https://pivottable.js.org/', 'https://pivottable.js.org/'))
        , rpivotTable(data=data(), rendererName="Table", width="100%")
      )
    })
  })
  
}

### RUN APPLICATIONS ###
shinyApp(ui = ui, server = server)
