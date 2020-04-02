
md2tlist <- function(filepath, key = 'Key', rnode = NULL, ignore = c('without classification')
                     , replace_labels = list(), extract_pattern = list(), include_hyperlink = F) {
  
  mdtopic2st <- function(topic) {
    
    if (("deleted" %in% names(topic$.attrs) && as.logical(topic$.attrs[['deleted']]))) return(NULL)
    
    to_return <- list()
    if (length(topic$subTopics) > 0) {
      for (i in which(names(topic$subTopics) == 'topic')) {
        subtopic <- topic$subTopics[i]$topic
        label <- subtopic$text
        
        if (include_hyperlink && !is.null(subtopic$hyperlink)) {
          hyperlink <- as.list(subtopic$hyperlink)$url
        }
        
        if (length(label) > 0 && stringr::str_starts(label, paste0(key,":"))) {
          value <- stringr::str_extract(label, paste0(key,":\\s+[a-z0-9A-Z]+"))
          if (include_hyperlink) attr(value, 'hyperlink') <- hyperlink
          for (name_str_pattern in names(extract_pattern)) {
            attr(value, name_str_pattern) <- stringr::str_extract(label, extract_pattern[[name_str_pattern]])   
          }
          
          label <- value
        } else {
          value <- mdtopic2st(subtopic)
        }
        
        if (!is.null(value) && length(label) > 0 && length(value) > 0) {
          if (label %in% names(replace_labels)) {
            label <- replace_labels[[label]]
          }
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
      if (!is.null(value)) {
        label <- topic$text
        if (label %in% names(replace_labels)) {
          label <- replace_labels[[label]]
        }
        to_return[[label]] <- value
      }
    }
  }
  
  if (!is.null(rnode)) {
    to_return2 <- list()
    to_return2[[rnode]] <- to_return
    return(to_return2)
  }
  
  return(to_return)
}
