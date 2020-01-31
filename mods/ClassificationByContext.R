
reviewByContext2df <- function(selected_keys, selected_contexts, one_by_context = T) {
  contexts <- md2list(paste0(getwd(),"/src-data/mom/context.xml"), rnode = 'From all contexts')
  items <- jsonlite::fromJSON(paste0(getwd(),'/src-data/zotero.json'))$items
  
  categories <- unique(stringr::str_split(paste0(selected_contexts, collapse = ";"), ";")[[1]])
  keys_in_contexts <- tree2List(contexts, cats=categories, keys=selected_keys)
  
  rdf <- data.frame()
  for (ctxs in selected_contexts) {
    keys <- selected_keys
    for(ctx in stringr::str_split(ctxs,";")[[1]]) {
      keys <- intersect(keys, keys_in_contexts[[ctx]])
    }
    if (one_by_context) keys <- setdiff(keys, rdf$key)
    idx <- which(items$key %in% keys)
    
    tdf <- items[idx,which(sapply(1:ncol(items), FUN = function(i) { typeof(items[,i]) })!='list')]
    rdf <- rbind(rdf, cbind(tdf, context=rep(ctxs, times=length(idx))))
  }
  rownames(rdf) <- NULL
  return(rdf)
}

reviewByContext2pivot_df <- function(df=NULL, selected_keys=c(), selected_contexts=c(), latex=F) {
  if (is.null(df)) df <- reviewByContext2df(selected_keys, selected_contexts)
  df <- do.call(rbind, lapply(unique(df[['context']]), FUN = function(ctx) {
    idx <- which(df[['context']] == ctx)
    citekeys <- df[['citekey']][idx]
    if (latex) citekeys <- paste0("\\cite{", citekeys, "}")
    refs <- paste0(citekeys, collapse = ';')
    return(data.frame(context = ctx, refs = refs, freq=length(idx)))
  }))
  df[['pct']] <- round(100*df[['freq']]/sum(df[['freq']]), 2)
  return(rbind(df, data.frame(context='', refs='', freq=sum(df[['freq']]), pct=100)))
}


ClassificationByContextUI <- function(id) {
  ns <- NS(id)
  
  tagList(fluidRow(
    column(2, verticalLayout(
      h3(em(strong("Filtering")))
      , uiOutput(ns("filterSelectContexts"))
      , checkboxInput(ns("uniquecb"), label="Allow duplicates by context", value=F)
    ))
    , column(10 , tabsetPanel(
        type = "tabs"
        , tabPanel("Table", uiOutput(ns("ftable")), shinycssloaders::withSpinner(DT::DTOutput(ns("table"))))
        , tabPanel("Pivot Table", shinycssloaders::withSpinner(DT::DTOutput(ns("ptable"))))
        , tabPanel("Latex", uiOutput(ns("fields")), verbatimTextOutput(ns("latex")))
      ))
  ))
}

ClassificationByContext <- function(input, output, session, M, context_tree) {
  ns <- session$ns
  df <- reactive({ reviewByContext2df(M()$key, input$filterSelectContexts, input$uniquecb) })
  options <- list(pageLength = -1, dom = 'Bfrtip'
                  , buttons = c('pageLength','copy', 'csv', 'excel', 'pdf', 'print')
                  , lengthMenu = list(c(25,50,100,-1), c('25 rows', '50 rows', '100 rows','Show all'))
                  , columnDefs = list(list(className = 'dt-center')))
  
  output$filterSelectContexts <- renderUI({
    scontexts <- getSelectionCat(context_tree(), default = names(tree2List(contexts)))
    scontexts <- c(do.call(c, lapply(1:ncol(combn(scontexts, 2)), FUN = function(col) {
     return(paste0(combn(scontexts, 2)[,col], collapse = ';')) 
    })), scontexts)
    selectInput(ns("filterSelectContexts"), "Contexts", multiple=T, choices=scontexts, selected=scontexts)
  })
  
  #
  output$ftable <- renderUI({
    choices <- colnames(df())
    selectInput(ns("ftable"), "Contexts", multiple=T, choices=choices, selected=choices)
  })
  
  output$table <- DT::renderDataTable({ df()[,input$ftable] }, options=options)
  output$ptable <- DT::renderDataTable({ reviewByContext2pivot_df(df = df()) }, options=options)
  
  output$latex <- renderText({
    df <- reviewByContext2pivot_df(df=df(), latex=T)
    filename <- paste0(getwd(), '/tmp/', digest::digest(df), '.tex')
    Hmisc::latex(df, longtable=T, ctable=F, landscape=F, rowlabel="", where='!htbp', file=filename, rowname=NULL, append=F)
    paste(readLines(filename), collapse = '\n')
  })
}
