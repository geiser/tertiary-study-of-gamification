
barMapping <- function(cdf, items, numericField=NULL, x='val', width=800, height=400, plotfontsize=12
                     , isShortedLegend = F, pattern_name = 'item(#)', textposition='inside'
                     , legendposition='inside', barmode = 'group', plotYMin=0, plotYMax=50) {
  library(RColorBrewer)
  
  colors <- unique(c('#E6194B', '#3CB44B', '#FFE119', '#4363D8', '#F58231'
                     , '#911EB4', '#42D4F4', '#F032E6', '#BFEF45', '#FABEBE'
                     , '#469990', '#E6BEFF', '#9A6324', '#800000', '#AAFFC3'
                     , '#808000', '#FFD8B1', '#000075', '#A9A9A9'
                     , brewer.pal(8, 'Dark2'), brewer.pal(9, 'Pastel1'), brewer.pal(8,'Accent')
                     , brewer.pal(9, 'Set1'), brewer.pal(8, 'Pastel2'), brewer.pal(8, 'Set2')
                     , brewer.pal(12, 'Paired'), brewer.pal(12, 'Set3')))
  
  c_count <- 0
  y_acc <- rep(0, length(x))
  
  fig <- plot_ly(type='bar', width=width, height=height)
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
    
    text <- y; text[y == 0] <- NA;
    color <- colors[c_count %% length(colors)]
    fig <- add_trace(fig, x=x, y=y, text=text, name=item, marker=list(color=color)
                     , textposition=textposition, textfont=list(size=plotfontsize))
  }
  
  fig <- layout(fig, barmode=barmode, yaxis=list(range=c(plotYMin, plotYMax)))
  
  if (barmode == 'stack') {
    fig <- add_trace(fig, x=x, y=y_acc, text=y_acc, type='scatter', showlegend=F
                     , mode='mark+text', textfont=list(size=plotfontsize+2), textposition='top center') 
  }
  
  if (legendposition == 'inside') {
    fig <- layout(fig, legend = list(x = 0.01, y = 0.99))
  } else if (legendposition == 'bottom') {
    fig <- layout(fig, legend = list(orientation = 'h'))
  } else if (legendposition == 'none') {
    fig <- layout(fig, showlegend = FALSE)
  }
  
  return(fig)
}
