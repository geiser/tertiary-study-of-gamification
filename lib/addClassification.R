
findParents <- function(item, cdf, ignore = c()) {
  if (item %in% cdf$item) {
    parents <- c()
    for (parent in cdf$parent[cdf$item %in% item]) {
      if (!is.na(parent) && !(parent %in% ignore)) {
        parents <- c(parent, findParents(parent, cdf, ignore = ignore), parents)
      }
    }
    return(unique(parents))
  }
}

addClassification <- function(df, classification, field, include.intersection = T
                              , root = classification$df$item[is.na(classification$df$parent)]) {
  cdf <- classification$df#[!(classification$df$item %in% root),]
  cdf[['group']] <- cdf$parent
  
  pdf <- do.call(rbind, lapply(cdf$item[cdf$isLeaf], FUN = function(item) {
    parents <- findParents(item, cdf, ignore = root)
    return(data.frame(item = rep(item, length(parents)), group = parents))
  }))
  mdf <- unique(rbind(cdf[cdf$isLeaf, c('item','group')], pdf))
  
  if (include.intersection) {
    ddf <- do.call(rbind, lapply(cdf$item[duplicated(cdf$item)], FUN = function(item) {
      group <- paste0(cdf$parent[cdf$item == item], collapse = ' + ')  
      return(data.frame(item = item, group = group))
    }))
    mdf <- unique(rbind(ddf, mdf))
  }
  
  colnames(mdf) <- c('key', field)
  return(merge(df, mdf, by='key', all=F, all.x=T))
}
