
changeItem2keyDF <- function(df, items, notes, field = 'RO') {
  
  notes <- notes[which(stringr::str_starts(notes$field, paste0(field, "\\(.*\\)"))),]
  
  tmpDF <- do.call(rbind, lapply(seq(1,nrow(notes)), FUN = function(i) {
    note <- notes[i,]
    item <- items[items$itemID == notes$itemID[i],]
    attachments <- item$attachments[[1]]
    return(do.call(rbind, lapply(seq(1, nrow(attachments)), FUN = function(j) {
      attachment <- attachments[j,]
      plink <- paste0(attachment$uri,'/file?page=', note$page)
      lreturn <- list(key=note$key, hyperlink=plink)
      lreturn[[field]] <- note$field 
      return(as.data.frame(lreturn))
    })))
  }))
  
  df_leafs <- merge(df[df$isLeaf,], tmpDF, by = c('hyperlink', field), all.x = T)
  df_leafs[["item"]] <- df_leafs[["key"]]
  
  toReturn <- unique(rbind(df_leafs[,c('isLeaf','item','parent')]
                           , df[!df$isLeaf, c('isLeaf','item','parent')]))
  
  return(toReturn)  
}
