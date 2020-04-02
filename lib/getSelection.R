
getSelectionST <- function(tree, key = 'Key') {
  selected <- shinyTree::get_selected(tree, format = 'names')
  toreturn <- as.vector(sapply(selected, FUN = function(x) {
    if (stringr::str_starts(x[1], paste0(key,':'))) {
      return(qdap::Trim(gsub(paste0(key,':\\s*'), '', x[1])))
    }
  }))
  return(unlist(toreturn))
}

getSelectionCat <- function(tree, key = 'Key', root = 'From all', default = c()) {
  selected <- shinyTree::get_selected(tree, format = 'names')
  toreturn <- unlist(as.vector(sapply(selected, FUN = function(x) {
    if (!stringr::str_starts(x[1], paste0(key,':'))) {
      return(x[1])
    }
  })))
  if (length(toreturn) < 1) { toreturn <- default }
  if (length(toreturn) > 0) {
    toreturn <- toreturn[!startsWith(toreturn, root)]
  }
  return(toreturn)
}
