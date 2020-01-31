if (!(require(bibliometrix))){install.packages("bibliometrix"); require(bibliometrix, quietly=TRUE)}
if (!(require(shiny))){install.packages("shiny"); require(shiny, quietly=TRUE)} 
if (!(require(rio))){install.packages("rio")} 
if (!(require(DT))){install.packages("DT")} 
if (!(require(ggplot2))){install.packages("ggplot2"); require(ggplot2, quietly=TRUE)} 
if (!(require(shinycssloaders))){install.packages("shinycssloaders")} 
if (!(require(shinythemes))){install.packages("shinythemes")} 
if (!(require(wordcloud2))){install.packages("wordcloud2")} 
if (!require(colourpicker)){install.packages("colourpicker")}
if (!require(treemap)){install.packages("treemap")}
if (!require(ggmap)){install.packages("ggmap"); require(ggmap, quietly=TRUE)}
if (!require(maps)){install.packages("maps"); require(maps, quietly=TRUE)}
if (!require(visNetwork)){install.packages("visNetwork"); require(visNetwork, quietly=TRUE)}
if (!require(plotly)){install.packages("plotly"); require(plotly, quietly=TRUE)}
#
require(Matrix, quietly = TRUE)
library(shiny)
library(shinyTree)
options(stringsAsFactors=F)

source('lib/zconvert2df.R')
source('lib/md2list.R')
source('lib/getSelection.R')
source('lib/tree2latex_df.R')
source('lib/tree2List.R')
source('lib/dfAnnual2plotly.R')

source('mods/ClassificationByContext.R')
source('mods/ClassificationByReviewType.R')

initValues <- function(values, filepath) {
  values$results = list("NA")
  values$log = "working..."
  values$load = "FALSE"
  values$field = "NA"
  values$citField = values$colField = values$citSep="NA"
  values$NetWords = values$NetRefs = values$ColNetRefs=matrix(NA,1,1)
  values$Title = "Network"
  values$Histfield = "NA"
  values$histlog = "working..."
  values$kk = 0
  
  values$Morig <- zconvert2df(filepath)
  values$M <- values$Morig
  
  values$histsearch = "NA"
  values$citShortlabel = "NA"
  values$S = list("NA")
  values$GR = "NA"
  return(values)
}

# initial values
items <- jsonlite::fromJSON('src-data/zotero.json')$items
types <- md2list("src-data/mom/type.xml", rnode = 'All types of reviews')
contexts <- md2list("src-data/mom/context.xml", rnode = 'From all contexts')

### UI ###
ui <- navbarPage(
  "A tertiary study of gamification"
  , theme=shinythemes::shinytheme("flatly")
  
  ## LOAD PAGE ##
  , tabPanel(
    "Load"
    , mainPanel(
      tabsetPanel(
        type = 'tabs'
        , tabPanel(
          "Table"
          , tags$head(tags$style(HTML("table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover { background-color: #9c4242 !important; }"))) 
          , tags$style(HTML(".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,.dataTables_wrapper .dataTables_paginate .paginate_button, .dataTables_wrapper .dataTables_paginate .paginate_button.disabled { color: #000000 !important; }"))
          , shinycssloaders::withSpinner(DT::DTOutput("load_dt")))
        , tabPanel("Latex", verbatimTextOutput("load_tex"))
      )
    )
  )
  
  ## FILTER PAGE ##
  , tabPanel(
    "Filter"
    , sidebarLayout(
      sidebarPanel(
        width=3
        , h3(em(strong("Filtering "))), br()
        , uiOutput("filterTextDim")
        , uiOutput("filterSelectType"), uiOutput("filterSelectType2")
        , verticalLayout(
          strong("Selection by context")
          , shinyTree("filterTreeReviewContext", checkbox = TRUE)
        ), br()
        , verticalLayout(
          strong("Selection by review type")
          , shinyTree("filterTreeReviewType", checkbox = TRUE)
        ), br()
        , uiOutput("filterSliderPY"), uiOutput("filterSliderTC")
      )
      , mainPanel(DT::DTOutput("dataFiltered"))
    )
  )
  
  ## CLASSIFICATION PAGE ##
  , navbarMenu(
    "Classification"
    , "  ", "  ", "Related to the review by itself"
    , tabPanel(
      "Classification by context/domain"
      , ClassificationByContextUI("class-context")
    )
    , tabPanel(
      "Classification by types"
      , mainPanel(
        tabsetPanel(
          type = "tabs"
          , tabPanel("Table", shinycssloaders::withSpinner(DT::DTOutput("classification_by_type_table")))
          , tabPanel("Latex", uiOutput("classification_by_type_tex_fields"), verbatimTextOutput("classification_by_type_tex"))
        )
      )
    )
    , tabPanel(
      "Classification by scientific forums"
      , mainPanel(
        tabsetPanel(
          type = "tabs"
          , tabPanel("Table", shinycssloaders::withSpinner(DT::DTOutput("classification_by_sciforum_table")))
          , tabPanel("Latex", uiOutput("classification_by_sciforum_tex_fields"), verbatimTextOutput("classification_by_sciforum_tex"))
        )
      )
    )
    , tabPanel(
      "Classification by types x scientific forums"
      , mainPanel(
        tabsetPanel(
          type = "tabs"
          , tabPanel("Table", shinycssloaders::withSpinner(DT::DTOutput("classification_by_type_sciforum_table")))
          , tabPanel("Latex", uiOutput("classification_by_type_sciforum_tex_fields"), verbatimTextOutput("classification_by_type_sciforum_tex"))
        )
      )
    )
    , tabPanel(
      "Annual evolution by types and scientific forums"
      , sidebarLayout(
        sidebarPanel(
          width=3
          , numericInput("e2TypeForumPlotWidth", "Plot width:", 640, min = 20)
          , numericInput("e2TypeForumPlotHeight", "Plot height:", 320, min = 20)
          , numericInput("e2TypeForumPlotYMin", "Plot min(y):", 0, min = 0)
          , numericInput("e2TypeForumPlotYMax", "Plot max(y):", 100)
          , numericInput("e2TypeForumPlotFontSize", "Plot font size:", 12, min = 4)
          , numericInput("e2TypeForumPlotInnerFontSize", "Plot inner font size:", 8, min = 4)
        )
        , mainPanel(
          tabsetPanel(
            type = "tabs"
            , tabPanel("Table", shinycssloaders::withSpinner(DT::DTOutput("evolution_by_type_sciforum_table")))
            , tabPanel("Bar Charts", uiOutput("e2TypeForumBarCharts"))
          )
        )
      )
    )
  )
)

### SERVER LOGIC ###
server <- function(input, output, session) {
  
  values <- reactiveValues()
  isolate({ values <- initValues(values, filepath = 'src-data/zotero.json') })
  
  # stop the R session & set file upload max size
  session$onSessionEnded(stopApp)
  options(shiny.maxRequestSize=100*1024^2) 

  ## LOAD PAGE ##
  output$load_dt <- DT::renderDT({
    isolate({
      MData <- as.data.frame(apply(values$Morig, 2, function(x) { substring(x,1,150) }), stringsAsFactors = F)
      MData$DOI <- paste0('<a href=\"http://doi.org/', MData$DI, '\" target=\"_blank\">', MData$DI, '</a>')
      MData <- MData[c("DOI", names(MData)[-length(names(MData))])]
      
      #
      DT::datatable(MData, escape = F, rownames = F, extensions = c("Buttons")
                    , options = list(pageLength = -1, dom = 'Bfrtip', buttons = list('pageLength','csv','pdf','copy','print')
                                     , lengthMenu = list(c(25,50,100,-1), c('25 rows', '50 rows', '100 rows','Show all'))
                                     , columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(MData))-1))))  
                    , class = 'cell-border compact stripe')  %>% 
        formatStyle(names(MData),  backgroundColor = 'white', textAlign = 'center',fontSize = '70%') 
    })
  })
  
  output$load_tex <- renderText({
    M <- values$Morig
    filename <- paste0('tmp/', digest::digest(M), '.tex')
    Hmisc::latex(M, longtable = T, ctable=F, landscape = F, rowlabel = "", where='!htbp', file = filename, rowname = NULL, append = F)
    paste(readLines(filename), collapse = '\n')
  })
  
  ## FILTER PAGE ##
  output$filterTreeReviewType <- renderTree({ types })
  output$filterTreeReviewContext <- renderTree({ contexts })
  
  output$filterTextDim <- renderUI({
    dimMatrix <- paste("Documents", dim(values$M)[1], "of", dim(values$Morig)[1])
    textInput("filterTextDim", "Number of Documents", value=dimMatrix)
  })
  
  output$filterSelectType <- renderUI({
    artType = sort(unique(values$Morig$DT))
    selectInput("filterSelectType", "Document Type", choices = artType, selected = artType, multiple =TRUE)
  })
  
  output$filterSelectType2 <- renderUI({
    artType = sort(unique(values$Morig$DT2))
    selectInput("filterSelectType2", "Document Type (second classification)", choices = artType, selected = artType, multiple =TRUE)
  })
  
  output$filterSliderPY <- renderUI({
    sliderInput("filterSliderPY", "Publication Year", min = min(values$Morig$PY,na.rm=T), sep="",
                max = max(values$Morig$PY,na.rm=T), value = c(min(values$Morig$PY,na.rm=T), max(values$Morig$PY,na.rm=T)))
  })
  
  output$filterSliderTC <- renderUI({
    sliderInput("filterSliderTC", "Total Citation", min = min(values$Morig$TC, na.rm=T),
                max = max(values$Morig$TC, na.rm=T), value = c(min(values$Morig$TC, na.rm=T), max(values$Morig$TC,na.rm=T)))
  })
  
  output$dataFiltered <- DT::renderDT({
    M <- values$Morig
    B <- bradford(M)$table
    M <- subset(M, M$PY>=input$filterSliderPY[1] & M$PY<=input$filterSliderPY[2])
    M <- subset(M, M$TC>=input$filterSliderTC[1] & M$TC<=input$filterSliderTC[2])
    M <- subset(M, M$DT %in% input$filterSelectType)
    M <- subset(M, M$DT2 %in% input$filterSelectType2)
    
    # filter by contexts and type
    selected_by_context <- getSelectionST(input$filterTreeReviewContext)
    if (length(selected_by_context) > 0) M <- subset(M, M$key %in% selected_by_context)
    
    selected_by_type <- getSelectionST(input$filterTreeReviewType)
    if (length(selected_by_type) > 0) M <- subset(M, M$key %in% selected_by_type)
    
    # initialize display
    values$M <- M
    Mdisp <- as.data.frame(apply(values$M, 2, function(x){ substring(x,1,150) }), stringsAsFactors = FALSE)    
    if (dim(Mdisp)[1]>0){
      DT::datatable(Mdisp, rownames = FALSE, extensions = c("Buttons"),
                    options = list(pageLength = -1, dom = 'Bfrtip',
                                   buttons = c('pageLength','copy', 'csv', 'excel', 'pdf', 'print'),
                                   lengthMenu = list(c(25,50,100,-1),c('25 rows', '50 rows', '100 rows','Show all')),
                                   columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(Mdisp))-1)))),
                    class = 'cell-border compact stripe') %>%
        formatStyle(names(Mdisp),  backgroundColor = 'white',textAlign = 'center', fontSize = '70%')
    } else {
      Mdisp <- data.frame(Message="Empty collection",stringsAsFactors = FALSE, row.names = " ")
    }
  })
  
  
  ## CLASSIFICATION PAGE ##
  output$classification_by_type_table <- DT::renderDataTable({
    values$c2type <- tree2latex_df(types, items, keys = values$M$key, categories = getSelectionCat(input$filterTreeReviewType))
    values$c2type
  }
  , options = list(pageLength = -1, dom = 'Bfrtip',
                   buttons = c('pageLength','copy', 'csv', 'excel', 'pdf', 'print'),
                   lengthMenu = list(c(25,50,100,-1), c('25 rows', '50 rows', '100 rows','Show all')),
                   columnDefs = list(list(className = 'dt-center')))
  )
  
  output$classification_by_type_tex_fields <- renderUI({
    c2type_names = sort(unique(colnames(values$c2type)))
    selectInput("classification_by_type_tex_fields", "Selected fields", choices = c2type_names, selected = c2type_names, multiple =TRUE)
  })
  
  output$classification_by_type_tex <- renderText({
    df <- values$c2type[c(input$classification_by_type_tex_fields)]
    filename <- paste0('tmp/', digest::digest(df), '.tex')
    Hmisc::latex(df, longtable = T, ctable=F, landscape = F, rowlabel = "", where='!htbp', file = filename, rowname = NULL, append = F)
    paste(readLines(filename), collapse = '\n')
  })
  
  #
  output$classification_by_sciforum_table <- DT::renderDataTable({
    M <- values[['M']]
    df <- do.call(rbind, lapply(unique(M[['DT']]), FUN = function(t) {
      citekeys <- items$citekey[items$key %in% M$key[which(M$DT == t)]]
      return(data.frame(name = t, refs = paste(paste0("\\cite{", citekeys, "}"), collapse = '; '), freq = length(citekeys)))
    }))
    df[['pct']] <- round(df$freq/sum(df$freq) * 100, 2)
    
    values$c2forum <- df
    values$c2forum
  }
  , options = list(pageLength = -1, dom = 'Bfrtip',
                   buttons = c('pageLength','copy', 'csv', 'excel', 'pdf', 'print'),
                   lengthMenu = list(c(25,50,100,-1), c('25 rows', '50 rows', '100 rows','Show all')),
                   columnDefs = list(list(className = 'dt-center')))
  )
  
  output$classification_by_sciforum_tex_fields <- renderUI({
    c2forum_names = sort(unique(colnames(values$c2forum)))
    selectInput("classification_by_sciforum_tex_fields", "Selected fields", choices = c2forum_names, selected = c2forum_names, multiple =TRUE)
  })
  
  output$classification_by_sciforum_tex <- renderText({
    df <- values$c2forum[c(input$classification_by_sciforum_tex_fields)]
    filename <- paste0('tmp/', digest::digest(df), '.tex')
    Hmisc::latex(df, longtable = T, ctable=F, landscape = F, rowlabel = "", where='!htbp', file = filename, rowname = NULL, append = F)
    paste(readLines(filename), collapse = '\n')
  })
  
  #
  output$classification_by_type_sciforum_table <- DT::renderDT({
    M <- values[['M']]
    df <- NULL
    for (dt in unique(M$DT)) {
      keys <- M$key[which(M$DT == dt)]
      tdf <- tree2latex_df(types, items, keys = keys, categories = getSelectionCat(input$filterTreeReviewType))
      colnames(tdf) <- c('name', paste0(colnames(tdf)[!colnames(tdf) %in% 'name'], '.', dt))
      if (is.null(df)) { df <- tdf } else {
        df <- merge(df, tdf, by = 'name', suffixes = c('',paste0('.',dt)))
      }
    }
    values$c2type_forum <- df
    values$c2type_forum
  })
  
  output$classification_by_type_sciforum_tex_fields <- renderUI({
    c2type_forum_names = sort(unique(colnames(values$c2type_forum)))
    selectInput("classification_by_type_sciforum_tex_fields", "Selected fields", choices = c2type_forum_names, selected = c2type_forum_names, multiple =TRUE)
  })
  
  output$classification_by_type_sciforum_tex <- renderText({
    df <- values$c2type_forum[c(input$classification_by_type_sciforum_tex_fields)]
    filename <- paste0('tmp/', digest::digest(df), '.tex')
    Hmisc::latex(df, longtable = T, ctable=F, landscape = F, rowlabel = "", where='!htbp', file = filename, rowname = NULL, append = F)
    paste(readLines(filename), collapse = '\n')
  })
  
  
  #
  output$evolution_by_type_sciforum_table <- DT::renderDT({
    M <- values[['M']]
    
    keys_in_cats <- tree2List(types, cats = getSelectionCat(input$filterTreeReviewType), keys = getSelectionST(input$filterTreeReviewType))
    values$e2type_forum <- do.call(rbind, lapply(names(keys_in_cats), FUN = function(ncat) {
      tM <- M[which(M$key %in% keys_in_cats[[ncat]]),]
      return(do.call(rbind, lapply(unique(tM$DT), FUN = function(dt) {
        return(do.call(rbind, lapply(unique(tM$PY), FUN = function(py) {
          keys <- tM$key[which(tM$DT == dt & tM$PY == py)]
          return(data.frame(cat = ncat, type = dt, year = py, freq = length(keys), stringsAsFactors = FALSE))
        })))
      })))
    }))
    
    values$e2type_forum
  })
  
  output$e2TypeForumBarCharts <- renderUI({
    
    min_py <- min(values$Morig$PY, na.rm=T)
    max_py <- max(values$Morig$PY, na.rm=T)
    
    #for (ncat  in unique(values$e2type_forum$cat)) {
    #  df <- values$e2type_forum[which(values$e2type_forum$cat == ncat),]
    #  p <- dfAnnual2plotly(df, min_py = min_py, max_py = max_py)
    #}
    
    
    do.call(verticalLayout, lapply(unique(values$e2type_forum$cat), FUN = function(ncat) {
      df <- values$e2type_forum[which(values$e2type_forum$cat == ncat),]
      verticalLayout(
        h3(em(paste('Plot for', ncat)))
        , dfAnnual2plotly(df, min_py = min_py, max_py = max_py
                          , width = input$e2TypeForumPlotWidth
                          , height = input$e2TypeForumPlotHeight
                          , ymin = input$e2TypeForumPlotYMin
                          , ymax = input$e2TypeForumPlotYMax
                          , fontsize = input$e2TypeForumPlotFontSize
                          , innerfontsize = input$e2TypeForumPlotInnerFontSize
                          ), br(), br()
      )
    }))
    
  })
  
  output$ae2TypeForumBarChart <- renderPlotly({
    df <- values$e2type_forum
  })
  
  callModule(ClassificationByContext, "class-context"
             , reactive({ values$M }), reactive({ input$filterTreeReviewContext }))
  
}

### RUN APPLICATIONS ###
shinyApp(ui = ui, server = server)
