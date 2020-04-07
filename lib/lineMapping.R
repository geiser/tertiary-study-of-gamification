

lineMapping <- function(cdf, items, numericField=NULL, x='val', width=800, height=400, plotfontsize=12
                            , isShortedLegend = F, pattern_name = 'item(#)', textposition='inside'
                            , legendposition='inside', plotYMin=0, plotYMax=50) {
  library(RColorBrewer)  
  
  colors <- unique(c(brewer.pal(9, 'Set1')[-1], brewer.pal(8, 'Dark2')
                     , brewer.pal(10, 'Set3'),  brewer.pal(8,'Accent')
                     , brewer.pal(9, 'Pastel1'), brewer.pal(8, 'Pastel2')
                     , brewer.pal(8, 'Set2')))
  
  msymbols <- c('circle', 'square', 'diamond', 'cross', 'x', 'triangle-up', 'pentagon'
                , 'star', 'hexagram', 'circle-open-dot', 'square-open-dot', 'diamond-open-dot'
                , 'cross-open-dot', 'x-open-dot', 'triangle-up-open-dot', 'pentagon-open-dot'
                , 'star-open-dot', 'hexagram-open-dot')
  dashs <- c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot") 
  
  fig <- plot_ly(type='scatter', mode='lines+markers', width=width, height=height)
  
  c_count <- 0; y_acc <- rep(0, length(x))
  for (item in items) {
    c_count <- c_count+1
    tdf <- cdf[(cdf$ColumnLevel01 %in% x) & cdf$RowLevel01 == item,]
    if (isShortedLegend) {
      item <- stringr::str_replace_all(pattern_name, "\\#", as.character(c_count))
    }
    
    y <- sapply(x, FUN = function(i) {
      if (i %in% tdf$ColumnLevel01) {
        values <- tdf$rawValue[tdf$ColumnLevel01 == i]
        if (!is.null(numericField)) values <- round(values, 2)
        return(values)
      } else {
        return(0)
      }
    })
    
    y[is.na(y)] <- 0
    y_acc <- y_acc + y
    
    c_msymbols <- c_count %% length(msymbols)
    
    text <- y; text[y == 0] <- NA
    color <- colors[c_count %% length(colors)]
    fig <- add_trace(fig, x=x, y=y, text=text, name=item, textfont=list(size=plotfontsize)
                     , textposition=textposition
                     , line = list(dash = dashs[c_msymbols %% length(dashs)])
                     , marker=list(color=color, symbol=msymbols[c_msymbols], size=plotfontsize-6))
  }
  
  
  fig <- layout(fig, yaxis=list(range=c(plotYMin, plotYMax)))
  
  if (legendposition == 'inside') {
    fig <- layout(fig, legend = list(x = 0.01, y = 0.99))
  } else if (legendposition == 'bottom') {
    fig <- layout(fig, legend = list(orientation = 'h'))
  } else if (legendposition == 'none') {
    fig <- layout(fig, showlegend = FALSE)
  }
  #
  return(fig)
  
}
