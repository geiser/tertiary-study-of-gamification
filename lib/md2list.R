
md2list <- function(filepath, key = 'Key', rnode = NULL, ignore = c('without classification')) {
  
  mdtopic2st <- function(topic) {
    if (("deleted" %in% names(topic$.attrs) &&
         as.logical(topic$.attrs[['deleted']]))) return(NULL)
    
    to_return <- list()
    if (length(topic$subTopics) > 0) {
      for (i in which(names(topic$subTopics) == 'topic')) {
        subtopic <- topic$subTopics[i]$topic
        label <- subtopic$text
        
        if (length(label) > 0 && stringr::str_starts(label, "Key:")) {
          value <- stringr::str_extract(label, paste0(key,":\\s+[0-9A-Z]+"))
          label <- value
        } else {
          value <- mdtopic2st(subtopic)
        }
        if (!is.null(value) && length(label) > 0 && length(value) > 0) {
          to_return[[label]] <- value
        }
      }
    }
    
    return(to_return)
  }
  
  to_return <- list()
  mdlist <- XML::xmlToList(XML::xmlParse(filepath))
  for (i in which(names(mdlist$topics) == 'topic')) {
    topic <- mdlist$topics[i]$topic
    if (!(tolower(topic$text) %in% ignore)) {
      value <- mdtopic2st(topic)
      if (!is.null(value)) to_return[[topic$text]] <- value 
    }
  }
  if (!is.null(rnode)) {
    to_return2 <- list()
    to_return2[[rnode]] <- to_return
    return(to_return2)
  }
  return(to_return)
}
