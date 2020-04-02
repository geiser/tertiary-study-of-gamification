
tlist2df <- function(tlist, key = 'Key', parent = NA, def.attributes = list()) {
  toReturn <- data.frame()
  
  for (lname in names(tlist)) {
    isLeaf <- F
    item <- lname
    if (startsWith(lname, paste0(key, ':'))) {
      isLeaf <- T
      item <- qdap::Trim(gsub(paste0(key,':\\s*'), '', lname))
      if (!is.null(attributes(tlist[[lname]]))) {
        for (nattribute in names(attributes(tlist[[lname]]))) {
          if (nattribute %in% names(def.attributes)) {
            def.attributes[[nattribute]] <- attributes(tlist[[lname]])[[nattribute]]
          }
        }
      }
    }
    toReturn <- rbind(toReturn, rbind(
      tlist2df(tlist[[lname]], key = key, parent = lname, def.attributes = def.attributes)
      , as.data.frame(c(def.attributes, list(item = item, parent = parent, isLeaf = isLeaf)))
    ))
  }
  
  return(toReturn)
}
