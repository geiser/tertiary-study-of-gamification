if (!require(rpivotTable)){ install.packages('rpivotTable') }

library(rpivotTable)
library(pivottabler)
library(plotly)
library(RColorBrewer)
library(rstatix)



duplicatedCitekey <- unique(c("HernandezMoreno2019",
                              "Larson2019",
                              "SilvaRodriguesLeal2019",
                              "PeixotoSilva2017",
                              "SouzaVeadoMoreiraFigueiredoCosta2017", 
                              "AlhammadMoreno2018",
                              "SouzaVeadoMoreiraFigueiredoCosta2018",
                              "OsatuyiOsatuyiDeLaRosa2018",
                              "LimaDavis2018",
                              "GentryGauthierEhrstromWortleyLilienthalCarDauwels-OkutsuNikolaouZaryCampbellCar2019",
                              "SantosSa-CoutoVieira-Marques2019", 
                              "MagistaDorraPean2018",
                              "DosSantosStradaBottino2019",
                              "HernandezMoreno2019",
                              "Larson2019",
                              "SilvaRodriguesLeal2019",
                              "PeixotoSilva2017",
                              "SouzaVeadoMoreiraFigueiredoCosta2017",
                              "AlhammadMoreno2018",
                              "SouzaVeadoMoreiraFigueiredoCosta2018",
                              "OsatuyiOsatuyiDeLaRosa2018",
                              "LimaDavis2018",
                              "GentryGauthierEhrstromWortleyLilienthalCarDauwels-OkutsuNikolaouZaryCampbellCar2019",
                              "SantosSa-CoutoVieira-Marques2019", 
                              "MagistaDorraPean2018",
                              "DosSantosStradaBottino2019"))


q1 <- function(x) { return(quantile(x, na.rm=T)[2]) }
q3 <- function(x) { return(quantile(x, na.rm=T)[4]) }

getOutliers <- function (data, dvs, ivs) {
  library(rstatix)
  library(dplyr)
  dat <- data
  for (iv in ivs) {
    if (is.numeric(dat[[iv]])) {
      dat[[iv]] <- sapply(dat[[iv]], FUN = as.character)
    }
  }
  dat <- group_by_at(dat, vars(ivs))
  do.call(rbind, lapply(dvs, FUN = function(dv) {
    outliers <- identify_outliers(dat, variable = dv)
    if (nrow(outliers) > 0) {
      return(cbind(var = dv, outliers))
    }
  }))
}

source(paste0(getwd(),'/lib/df2DT.R'))
source(paste0(getwd(),'/lib/barMapping.R'))
source(paste0(getwd(),'/lib/lineMapping.R'))

mappingNumSelectedStudiesUI <- function(id, label="", title=label) {
  ns <- NS(id)
  tabPanel(
    label, h3(title)
    , sidebarLayout(
      sidebarPanel(width = 3, uiOutput(ns('selectFilterBy')), uiOutput(ns('optionPanel')))
      , mainPanel(tabsetPanel(
        type="tabs", id=ns("selectedTabPanel")
        , tabPanel("Data table", DT::DTOutput(ns("sourceDT")), value="data")
        , tabPanel("Pivot table", DT::DTOutput(ns("pivotDT")), value="pivot")
        , tabPanel("Charts", verticalLayout(
          radioButtons(ns('chartType'), 'Chart type', choices = c('pie', 'stacked-bar', 'bar', 'line'), selected = 'pie', inline = T)
          , flowLayout(actionButton(ns("doPlot"), "Plot"), actionButton(ns("doClear"), "Clear"))
          , plotlyOutput(ns("plotlyChart")))
          , value = "charts")
        , tabPanel("Latex table", verbatimTextOutput(ns("latexDT")), value="latex")
      ))
    )
  )
}

mappingNumSelectedStudiesMD <- function(input, output, session, data, mainField, shinyTrees = list()
                          , def.param = list(filterField = 'Context', groupField = 'Context')
                          , pctExpression = NULL, numericField = NULL) {
  
  ns <- session$ns
  
  output$selectFilterBy <- renderUI({
    choices <- sort(unique(data[[mainField]]))
    selected <- choices
    if (!is.null(def.param$filterValues)) {
      choices <- unique(c(def.param$filterValues, choices))
      selected <- def.param$filterValues
    }
    selectInput(ns("selectedFilterBy"), 'Show/Display Data', choices=choices, selected=selected, multiple=T)
  })
  
  
  
  output$dataFilterPanel <- renderUI({
    if (input$isDataFilter) {
      selected <- def.param$filterField
      if (is.null(selected)) selected = 'Context'
      verticalLayout(
        selectInput(ns("dataFilterField"), "Field for filter data", choices=colnames(data), selected = selected)
        , uiOutput(ns("selectDataFilterPanel"))
      )
    }
  })
  
  output$selectDataFilterPanel <- renderUI({
    if (input$isDataFilter) {
      choices <- sort(unique(data[[input$dataFilterField]]))
      selectInput(ns("dataFilterValue"), "Filter values", choices=choices, selected=choices, multiple=T)
    }
  })
  
  df <- reactive({
    dr <- data[data[[mainField]] %in% input$selectedFilterBy,]
    
    columns <- c('key','title','authors', 'citekey', 'date', mainField, numericField)
    if (input$isDataGroup) {
      columns <- c(input$dataGroupField, columns)
      if (length(input$dataGroupValue) > 0) {
        dr <- dr[dr[[input$dataGroupField]] %in% input$dataGroupValue,]
      }
    }
    if (input$isDataFilter && !is.null(input$dataFilterField) && !is.na(input$dataFilterField)) {
      columns <- c(columns, input$dataFilterField)
      if (length(input$dataFilterValue) > 0) {
        dr <- dr[dr[[input$dataFilterField]] %in% input$dataFilterValue,]
      }
    }
    
    ##
    if (input$isAvoidDuplicate) {
      td <- data.frame()
      included_keys <- c()
      for (value in input$duplicateSort) {
        idx <- which(dr[[mainField]] == value & !(dr$key %in% included_keys))
        td <- rbind(dr[idx,], td); included_keys <- c(dr$key[idx], included_keys)
      }
      dr <- td
    }
    
    unique(dr[,columns])
  })
  
  output$sourceDT <- DT::renderDataTable({
    df2DT(df())
  })
  
  ## control panel UI
  
  output$outliersPanel <- renderUI({
    if (input$selectedTabPanel != 'data'){
      choices <- unique(data[[input$selectedRefsFrom]])
      verticalLayout(
        selectInput(ns("outliers"), "Outliers", choices=choices, multiple = T),
        actionLink(ns("identifyingExtremeOutliers"), "Identifying extreme outliers"),
        actionLink(ns("identifyingOutliers"), "Identifying outliers")
      )
    }
  })
  
  
  observeEvent(input$identifyingExtremeOutliers, {
    dat <- merge(df(), data[,c("key", input$selectedRefsFrom)], by = "key")
    
    ivs <- c(mainField)
    if (input$isDataGroup && !is.null(input$dataGroupField)) {
      ivs <- c(ivs, input$dataGroupField)
      dat <- merge(dat, data[,c('key', input$dataGroupField)], key = 'key')
    }
    outliers <- unique(getOutliers(dat, numericField, ivs))
    outliers <- outliers[[input$selectedRefsFrom]][which(outliers$is.extreme)]
    updateSelectInput(session, "outliers", selected = unique(outliers))
  })
  
  observeEvent(input$identifyingOutliers, {
    dat <- merge(df(), data[,c("key", input$selectedRefsFrom)], by = "key")
    
    ivs <- c(mainField)
    if (input$isDataGroup && !is.null(input$dataGroupField)) {
      ivs <- c(ivs, input$dataGroupField)
      dat <- merge(dat, data[,c('key', input$dataGroupField)], key = 'key')
    }
    outliers <- unique(getOutliers(dat, numericField, ivs))
    outliers <- outliers[[input$selectedRefsFrom]][which(outliers$is.outlier)]
    updateSelectInput(session, "outliers", selected = unique(outliers))
  })
  
  output$optionPanel <- renderUI({
    
    vlayout <- verticalLayout(
      checkboxInput(ns('isDataFilter'), 'Is the data filtered?', value=F)
      , uiOutput(ns('dataFilterPanel'))
    )
    
    #if (input$selectedTabPanel == 'charts') {
    #  vlayout <- verticalLayout(
    #    checkboxInput(ns('isAvoidDuplicate'), "Are the classification avoid duplicates?", value=F)
    #    , uiOutput(ns('avoidDuplicatePanel'))
    #    , vlayout
    #  )
    #}
    
    #if (input$selectedTabPanel != 'data') {
      vlayout <- verticalLayout(
        selectInput(ns("selectedRefsFrom"), "Use 'refs' as", choices=colnames(data), selected='citekey')
        , checkboxInput(ns('isAvoidDuplicate'), "Are the classification avoid duplicates?", value=F)
        , uiOutput(ns('avoidDuplicatePanel'))
        , vlayout
      )
    #}
    
    
    if (!is.null(numericField) && input$selectedTabPanel != 'data') {
      exp_choices <- c('median(#, na.rm=T)','mean(#, na.rm=T)','min(#, na.rm=T)','max(#, na.rm=T)'
                       ,'sum(#, na.rm=T)','q1(#)','q3(#)','mad(#, na.rm=T)','sd(#, na.rm=T)')
      multiple <- (input$selectedTabPanel != 'charts')
      vlayout <- verticalLayout(
        selectInput(ns("summariseExpression"), "Summarise expression", choices = exp_choices, multiple = multiple, selected = exp_choices[1])
        , vlayout
      )
    }
    
    vlayout <- verticalLayout(
      vlayout
      , checkboxInput(ns('isDataGroup'), 'Is the data grouped?', value=F)
      , uiOutput(ns('dataGroupPanel'))
      , checkboxInput(ns('isGroupByDate'), 'Is the data also grouped by data?', value=F)
      , uiOutput(ns("outliersPanel"))
    )
    
    if (input$selectedTabPanel == 'charts') {
      
      textpositions <- c("inside", "outside", "auto", "none")
      if (input$chartType == 'line') {
        textpositions <- c("top left", "top center", "top right", "middle left", "middle center"
                           , "middle right", "bottom left", "bottom center", "bottom right")
      }
      
      vlayout <- verticalLayout(
        vlayout, h4("Options for graph chart")
        , selectInput(ns("selectTextPosition"), "Text position", choices = textpositions, multiple = F)
        , selectInput(ns("legendPosition"), "Legend", choices=c('outside','inside','bottom','none'), multiple=F)
        , checkboxInput(ns('isShortedLegend'), "Is the legend name shorted?", value=F)
        , textInput(ns("iname"), 'Str pattern for the legend', 'item (#)')
        , numericInput(ns("plotFontSize"), "Plot font size:", 12, min=4, step=2)
        , numericInput(ns("plotWidth"), "Plot width:", 800, min=20, step=20)
        , numericInput(ns("plotHeight"), "Plot height:", 400, min=20, step=20)
      )
      if (input$chartType == 'bar' || input$chartType == 'stacked-bar' || input$chartType == 'line') {
        vlayout <- verticalLayout(
          vlayout
          , numericInput(ns("plotYMin"), "Plot min(y):", 0, min = 0)
          , numericInput(ns("plotYMax"), "Plot max(y):", 100)
        )
      } else if (input$chartType == 'pie') {
        vlayout <- verticalLayout(
          vlayout
          , selectInput(ns("pieTextinfo"), 'Textinfo', choices=c('label','percent','value'), selected='value', multiple=T)
        )
      }
    }
    vlayout
  })
  
  ## methods for pivottable tab
  
  output$avoidDuplicatePanel <- renderUI({
    if (input$isAvoidDuplicate) {
      removeDuplicateSort <- def.param$removeDuplicateSort
      choices <- unique(input$selectedFilterBy)
      choices <- choices[order(stringr::str_count(choices, "\\+"), decreasing = T)]
      if (!is.null(removeDuplicateSort)) {
        choices <- unique(c(removeDuplicateSort[removeDuplicateSort %in% choices], choices))
      }
      selectInput(ns("duplicateSort"), "Sort to remove duplicates", choices=choices, selected=choices, multiple=T)
    }
  })
  
  output$dataGroupPanel <- renderUI({
    if (input$isDataGroup) {
      selected <- def.param$groupField
      if (is.null(selected)) selected = 'date'
      verticalLayout(
        selectInput(ns("dataGroupField"), "Field for groupping data", choices=colnames(data), selected = selected)
        , uiOutput(ns("selectDataGroupPanel"))
      )
    }
  })
  
  output$selectDataGroupPanel <- renderUI({
    if (input$isDataGroup) {
      if (input$dataGroupField %in% names(shinyTrees)) {
        verticalLayout(
          uiOutput(ns("treeShinySelectedPanel"))
          , checkboxInput(ns('isSelectedInputUsed'), 'Is selected input used?', value=F)
        )
      } else {
        choices <- sort(unique(data[[input$dataGroupField]]))
        selectInput(ns("dataGroupValue"), "Values for groupping", choices=choices, selected=choices, multiple=T)
      }
    }
  })
  
  output$treeShinySelectedPanel <- renderUI({
    if (input$isSelectedInputUsed || input$selectedTabPanel == 'data') {
      choices <- sort(unique(data[[input$dataGroupField]]))
      selectInput(ns("dataGroupValue"), "Values for groupping", choices=choices, selected=choices, multiple=T)
    } else {
      verticalLayout(span("Values for groupping"), shinyTree(ns("dataGroupValueTree"), checkbox = TRUE))
    }
  })
  
  groupValues <- reactive({
    if (input$dataGroupField %in% names(shinyTrees)) {
      if (input$isSelectedInputUsed) {
        toReturn <- input$dataGroupValue
      } else {
        toReturn <- get_selected(input$dataGroupValueTree)
      }
    } else {
      toReturn <- input$dataGroupValue
    }
    toReturn
  })
  
  output$dataGroupValueTree <- renderTree({ shinyTrees[[input$dataGroupField]] })
  
  pivotDf <- reactive({
    library(data.table)
    selectedRefsFrom <- 'key'
    if (length(input$selectedRefsFrom) > 0) {
      selectedRefsFrom <- input$selectedRefsFrom
    }
    dat <- setorderv(data, cols = c("date","citekey"), c(1,1))
    dr <- dat[dat$key %in% df()$key,]
    dr <- dr[dr[[mainField]] %in% input$selectedFilterBy,]
    dr <- dr[!dr[[selectedRefsFrom]] %in% input$outliers,]
    
    columns <- c('key', mainField, selectedRefsFrom, 'date')
    if (!is.null(numericField)) columns <- c(columns, numericField)
    
    ##
    if (input$isAvoidDuplicate) {
      td <- data.frame()
      included_keys <- c()
      for (value in input$duplicateSort) {
        idx <- which(dr[[mainField]] == value & !(dr$key %in% included_keys))
        td <- rbind(dr[idx,], td); included_keys <- c(dr$key[idx], included_keys)
      }
      dr <- td
    }
    
    ##
    if (input$isDataGroup) {
      columns <- c(columns, input$dataGroupField)
      dr <- dr[dr[[input$dataGroupField]] %in% groupValues(),]
    }
    if (selectedRefsFrom == 'citekey' || selectedRefsFrom == 'citationKey') {
      dr_selectedRefsFrom <- as.vector(sapply(dr[[selectedRefsFrom]], FUN = function(ref) {
        paste0('\\cite{', ref ,'}', ifelse(ref %in% duplicatedCitekey, "*", ""), ";")
      }))
      dr[[selectedRefsFrom]] <- dr_selectedRefsFrom 
    }
    if (selectedRefsFrom != 'citekey' && selectedRefsFrom != 'citationKey') {
      refsExpression <- paste0("paste(", selectedRefsFrom, ", collapse='; ')")
    } else {
      refsExpression <- paste0("paste(", selectedRefsFrom, ", collapse=' ')")
    }
    dr <- unique(dr[,columns])
    
    pt <- PivotTable$new()
    pt$addData(dr)
    pt$addRowDataGroups(mainField, addTotal=F)
    if (input$isDataGroup && !is.null(input$dataGroupField)) {
      #pt$addColumnDataGroups(input$dataGroupField, addTotal=F)
      pt$addRowDataGroups(input$dataGroupField, addTotal=F)
    }
    if (input$isGroupByDate) {
      pt$addRowDataGroups('date', addTotal=F)
    }
    pt$defineCalculation(calculationName="refs", summariseExpression=refsExpression)
    if (is.null(numericField)) {
      pt$defineCalculation(calculationName="val", summariseExpression="n()")
      if (is.null(pctExpression)) pctExpression = paste0("(100*values$val)/", nrow(dr))
      pt$defineCalculation(calculationName="pct", type="calculation", basedOn=c("val")
                           , format="%.2f %%", calculationExpression=pctExpression)
    } else {
      pt$defineCalculation(calculationName="n", summariseExpression="n()")
      for (sumExpression in input$summariseExpression) {
        sumExpression <- stringr::str_replace(sumExpression, "\\#", paste0('`',numericField,'`'))
        pstr <- stringr::str_replace_all(sumExpression, "[^[:alnum:]]", " ")
        calcName <- stringr::str_split(pstr, " ")[[1]][1]
        pt$defineCalculation(calculationName=calcName, format="%.2f", summariseExpression=sumExpression)
      }
    }

    pt$evaluatePivot()
    
    toReturn <- as.data.frame(pt$asMatrix())
    colnames(toReturn) <- toReturn[1,]
    toReturn[seq(2,nrow(toReturn)),]
    #dr <- as.data.frame(pt$asMatrix()[seq(ifelse(input$isDataGroup,3,2), nrow(pt$asMatrix())),])
    #colnames(dr) <- pt$asMatrix()[1,]
    #if (input$isDataGroup) {
    #  colnames(dr) <- paste(colnames(dr), pt$asMatrix()[2,])
    #}
    
    ##
    #do.call(rbind, lapply(c(input$selectedFilterBy, 'Total'), FUN = function(x) {
    #  rbind(dr[dr[[1]] == x,])
    #}))
  })
  
  output$pivotDT <- DT::renderDataTable({
    withProgress(message = 'Making pivotal table', detail = 'This may take a while ...', {
      return(df2DT(pivotDf()))
    })
  })
  
  ## methods for chart tab
  
  chartDf <- reactiveVal()
  
  observeEvent(input$doClear, { chartDf(NULL) })
  observeEvent(input$selectedTabPanel, { chartDf(NULL) })
  observeEvent(input$chartType, { chartDf(NULL) })
  
  observeEvent(input$doPlot, {
    dr <- data[data$key %in% df()$key,]
    dr <- dr[dr[[mainField]] %in% input$selectedFilterBy,]
    dr <- dr[!dr[[input$selectedRefsFrom]] %in% input$outliers,]
    
    columns <- c('key', mainField)
    if (!is.null(numericField)) columns <- c(columns, numericField)
    
    ##
    if (input$isAvoidDuplicate) {
      td <- data.frame()
      included_keys <- c()
      for (value in input$duplicateSort) {
        idx <- which(dr[[mainField]] == value & !(dr$key %in% included_keys))
        td <- rbind(dr[idx,], td)
        included_keys <- c(dr$key[idx], included_keys)
      }
      dr <- td
    }
    
    ##
    if (input$isDataGroup) {
      columns <- c(columns, input$dataGroupField)
      dr <- dr[dr[[input$dataGroupField]] %in% groupValues(),]
    }
    dr <- unique(dr[,columns])
    
    pt <- PivotTable$new()
    pt$addData(dr)
    pt$addRowDataGroups(mainField, addTotal=F)
    if (input$isDataGroup && !is.null(input$dataGroupField)) {
      pt$addColumnDataGroups(input$dataGroupField)
    }
    if (is.null(numericField)) {
      pt$defineCalculation(calculationName="val", summariseExpression="n()")
    } else {
      summariseExpression = stringr::str_replace(input$summariseExpression, "\\#", paste0('`',numericField,'`'))
      pt$defineCalculation(calculationName="val", format="%.2f", summariseExpression=summariseExpression)
    }
    pt$evaluatePivot()
    
    cdf <- pt$asTidyDataFrame() 
    cdf <- cdf[!cdf$isTotal,]
    cdf[['group']] <- cdf$RowLevel01
    if (input$isDataGroup) {
      cdf[['group']] <- paste(cdf[['group']], cdf$ColumnLevel01, sep=':')
    }
    
    chartDf(do.call(rbind, lapply(input$selectedFilterBy, FUN = function(x) {
      rbind(cdf[cdf$RowLevel01 == x,])
    })))
    
  })
  
  
  output$plotlyChart <- renderPlotly({
    withProgress(message = 'Making plot', detail = 'This may take a while ...', {
      cdf <- chartDf(); if (is.null(cdf)) return() 
      
      x <- c('val')
      if (input$isDataGroup) x <- groupValues()
      
      if (input$chartType == 'pie') {
        textinfo <- paste0(input$pieTextinfo, collapse='+')
        values <- cdf$rawValue
        if (!is.null(numericField)) values <- round(values, 2) 
        fig <- plot_ly(labels=cdf$group, values=values, type='pie', textinfo=textinfo
                       , width=input$plotWidth, height=input$plotHeight, sort = F
                       , textfont=list(size = input$plotFontSize))
        fig <- layout(fig, margin=list(l=75,r=75, b=125, t=125, pad=4))
      } else if (input$chartType == 'line') {
        fig <- lineMapping(cdf, input$selectedFilterBy, numericField, x
                           , input$plotWidth, input$plotHeight, input$plotFontSize
                           , input$isShortedLegend, input$iname, input$selectTextPosition
                           , input$legendPosition, input$plotYMin, input$plotYMax)
      } else {
        barmode <- 'group'
        if (input$chartType == 'stacked-bar') barmode <- 'stack'
        fig <- barMapping(cdf, input$selectedFilterBy, numericField, x
                          , input$plotWidth, input$plotHeight, input$plotFontSize
                          , input$isShortedLegend, input$iname, input$selectTextPosition
                          , input$legendPosition, barmode, input$plotYMin, input$plotYMax)
      }
      return(fig)
    })
  })
  
  ## methods for latex tab
  
  output$latexDT <- renderText({
    withProgress(message = 'Making latex frame', detail = 'This may take a while ...', {
      filename <- paste0(getwd(), '/tmp/', ns("latexDT"), digest::digest(pivotDf()),'.tex')
      Hmisc::latex(pivotDf(), longtable=T, ctable=F, landscape=F, rowlabel="", where='!htbp', file=filename, rowname=NULL, append=F)
      paste(readLines(filename), collapse = '\n')
    })
  })
  
}
