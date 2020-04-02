
annotations2df <- function(annotations, field, value = 'value', key = 'key', include.intersection = F) {
  df <- annotations[annotations$field == field, c(key, value)]
  colnames(df) <- c('key', field)

  if (field == 'Paper type') {
    df[[field]][which(df[[field]] == 'Book chapter (workshop)')] <- 'Workshop (book chapter)'
    df[[field]][which(df[[field]] == 'Book chapter (conference)')] <- 'Conference (book chapter)'
  } else if (field == 'Number of selected studies') {
    df[[field]] <- as.numeric(stringr::str_extract(df[[field]], '[0-9]+'))
  } else if (field == 'Type of selected studies') {
    emp_and_nonemp_idx <- !is.na(
      stringr::str_extract(tolower(df[[field]]), '.*empirical.+and.+non-empirical.*'))
    emp_and_nonemp_idx <- emp_and_nonemp_idx | !is.na(
      stringr::str_extract(tolower(df[[field]]), '.*empirical.+and.+conceptual.*'))
    emp_and_nonemp_idx <- emp_and_nonemp_idx | !is.na(
      stringr::str_extract(tolower(df[[field]]), '.*non-empirical.+and.+empirical.*'))
    emp_and_nonemp_idx <- emp_and_nonemp_idx | !is.na(
      stringr::str_extract(tolower(df[[field]]), '.*conceptual.+and.+empirical.*'))
    emp_idx <- !emp_and_nonemp_idx & !is.na(
      stringr::str_extract(tolower(df[[field]]), '.*empirical.*'))
    nonemp_idx <- !(emp_idx | emp_and_nonemp_idx)
    
    df <- unique(rbind(data.frame(key=df[[key]][nonemp_idx], val='non-empirical')
                       , data.frame(key=df[[key]][emp_idx], val='empirical')
                       , data.frame(key=df[[key]][emp_and_nonemp_idx], val='empirical')
                       , data.frame(key=df[[key]][emp_and_nonemp_idx], val='non-empirical')))
    colnames(df) <- c('key', field)
  }
  
  if (include.intersection) {
    ddf <- do.call(rbind, lapply(df$key[duplicated(df$key)], FUN = function(item) {
      group <- paste0(df[[field]][df$key == item], collapse = ' + ')  
      return(data.frame(key = item, group = group))
    }))
    colnames(ddf) <- c('key', field)
    df <- unique(rbind(ddf, df))
  }
  
  return(df)
}


addAnnotations <- function(df, annotations, field, include.intersection = F) {
  return(merge(df, annotations2df(annotations, field, include.intersection = include.intersection), by='key', all=F, all.x=T))
}
