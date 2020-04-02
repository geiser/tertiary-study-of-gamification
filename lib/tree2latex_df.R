

tree2latex_df <- function(treeList, items, keys = items$key, categories = NULL, s = '') {
  
  aslatex_df <- function(treeList, items, keys = items$key, s = '') {
    do.call(rbind, lapply(names(treeList), FUN = function(name) {
      t <- treeList[[name]]
      refs <- as.vector(unlist(sapply(names(t), FUN = function(label) {
        if (startsWith(label, 'Key:')) {
          key <- qdap::Trim(gsub('Key:\\s*', '', label))
          if (key %in% keys) return(paste0("\\cite{", items$citekey[which(items$key == key)], "}"))
        } 
      })))
      freq <- length(refs)
      
      df <- do.call(rbind, lapply(names(t), FUN = function(label) {
        if (!startsWith(label, 'Key:')) return(aslatex_df(t, items, keys = keys, s = paste0(s, '  '))) 
      }))
      return(distinct(rbind(data.frame(name = paste0(s, name), refs = paste0(refs, collapse = '; '), freq = freq, stringsAsFactors = FALSE), df)))
    }))
  }
  
  df <- aslatex_df(treeList, items, keys, s)
  df[['pct']] <- round(df$freq/sum(df$freq) * 100, 2)
  
  if (length(categories) > 0) { df <- df[qdap::Trim(df$name) %in% categories,] }
  return(df)
}
