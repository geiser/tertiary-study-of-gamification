
source(paste0(getwd(),'/../lib/md2list.R'))
source(paste0(getwd(),'/../lib/getSelectionST.R'))

#' Define UI for application that demonstrates a simple Tree editor
#' @author Jeff Allen \email{jeff@@trestletech.com}
ui <- shinyUI(
  pageWithSidebar(
    # Application title
    headerPanel("shinyTree with checkbox controls"),
    
    sidebarPanel(
      helpText(HTML("An example of a shinyTree with the <code>checkbox</code> parameter enabled to allow users to more easily make multiple selections in the tree.
                  <hr>Created using <a href = \"http://github.com/trestletech/shinyTree\">shinyTree</a>."))
    ),
    mainPanel(
      # Show a simple table.
      shinyTree("tree", checkbox = TRUE)
      , htmlOutput("summary")
      
    ))
)

#' Define server logic required to generate a simple tree
#' @author Jeff Allen \email{jeff@@trestletech.com}
server <- shinyServer(function(input, output, session) {
  log <- c(paste0(Sys.time(), ": Interact with the tree to see the logs here..."))
  
  output$tree <- renderTree({
    md2list(paste0(getwd(),"/../src-data/pmom/type.xml"), rnode = 'All types of reviews')
  })
  
  output$summary <-renderText({
    selection <- getSelectionST(input$tree)
    print(selection)
  })
  
})

# Run the application 
shinyApp(ui = ui, server = server)
