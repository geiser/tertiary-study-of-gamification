

dfAnnual2plotly <- function(df, min_py = min(df$PY), max_py = max(df$PY)
                            , width = 640, height = 320, ymin = 0, ymax = 100
                            , fontsize = 12, innerfontsize = 8) {
  p <- plot_ly(type = 'bar', width = width, height = height, ymin = ymin, ymax = ymax)
  y_acc <- rep(0, (max_py-min_py)+1)
  for (dt in unique(df$type)) {
    tdf <- do.call(rbind, lapply(seq(min_py, max_py), FUN = function(py) {
      idx <- which(df$year == py & df$type == dt)
      freq <- ifelse(length(idx) > 0, df$freq[idx], 0)
      return(data.frame(x = py, y = freq, stringsAsFactors = F))
    }))
    y_acc <- y_acc + tdf$y
    p <- add_trace(p, x = tdf$x, y = tdf$y, text = tdf$y, name = dt
                   , textfont = list(size = innerfontsize)
                   , textposition = 'inside', ymin = ymin, ymax = ymax)
  }
  p <- layout(p, yaxis = list(title = ''), barmode = 'stack')
  p <- add_trace(p, x=seq(min_py, max_py), y=y_acc, text = y_acc, type = 'scatter', mode = 'mark+text'
                 , textfont = list(size = fontsize)
                 , ymin = ymin, ymax = ymax,  textposition = 'top center', showlegend=F)
  
  return(p)
}
