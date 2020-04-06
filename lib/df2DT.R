df2DT <- function(df, pageLength = 25) {
  return(
    DT::datatable(df, escape = F, rownames = F, extensions = c("Buttons")
                  , options = list(pageLength = pageLength, dom = 'Bfrtip', filter = 'top'
                                   , buttons = list('pageLength','csv','pdf','copy','print')
                                   , lengthMenu = list(c(25,50,100,-1), c('25 rows', '50 rows', '100 rows','Show all'))
                                   , columnDefs = list(list(targets = 0:(length(names(df))-1))))  
                  , class = 'cell-border compact stripe')
  )
}
