
tree2List <- function(treeList, key = 'Key', cats = c(), keys = c()) {
  
  treeList2df <- function(treeList, key = 'Key') {
    do.call(rbind, lapply(names(treeList), FUN = function(name) {
      t <- treeList[[name]]
      keys <- as.vector(unlist(sapply(names(t), FUN = function(label) {
        if (startsWith(label, 'Key:')) return(qdap::Trim(gsub(paste0(key,':\\s*'), '', label)))
      })))
      
      df <- do.call(rbind, lapply(names(t), FUN = function(label) {
        if (!startsWith(label, 'Key:')) return(treeList2df(t, key))
      }))
      
      return(distinct(rbind(data.frame(name = name, keys = paste0(c(keys, df$keys), collapse = ';'), stringsAsFactors = FALSE), df)))
    }))
  }
  
  toreturn <- list()
  df <- treeList2df(treeList, key)
  for (i in seq(1,nrow(df))) {
    cat <- df$name[i]
    if ((length(cats) == 0) ||  (cat %in% cats)) {
      skeys <- unique(strsplit(df$keys[i], ';')[[1]])
      if (length(keys) > 0) skeys <- skeys[skeys %in% keys]
      if (length(skeys) > 0) toreturn[[cat]] <- skeys
    }
  }
  
  return(toreturn)
}
