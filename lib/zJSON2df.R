
source(paste0(getwd(),'/lib/znotes2df.R'))

zpostprocessing <- function(DATA) {
  DATA$AU = gsub("\\s+", " ", DATA$AU)
  if ("TC" %in% names(DATA)) DATA$TC <- as.numeric(sub("\\D*(\\d+).*", "\\1", DATA$TC))
  if ("PY" %in% names(DATA)) DATA$PY <- as.numeric(sub("\\D*(\\d+).*", "\\1", DATA$PY))
  if ("UT" %in% names(DATA)) DATA$UT <- gsub(":", "", DATA$UT, fixed = TRUE)
  if (!("RP" %in% names(DATA)) & ("C1" %in% names(DATA))) {
    DATA$RP <- unlist(lapply(strsplit(DATA$C1, "\\."), function(l) l[1]))
  }
  if ("ID" %in% names(DATA)) {
    DATA$ID <- gsub("   ", ";", DATA$ID)
    DATA$ID <- gsub(",", ";", DATA$ID)
  }
  if ("DE" %in% names(DATA)) {
    DATA$DE <- gsub("   ", ";", DATA$DE)
    DATA$DE <- gsub(",", ";", DATA$DE)
  }
  if (("SO" %in% names(DATA)) & ("BO" %in% names(DATA))) {
    ind <- which(is.na(DATA$SO))
    DATA$SO[ind] <- DATA$BO[ind]
  }
  if ("PN" %in% names(DATA)) {
    DATA$PN <- as.numeric(gsub("[^0-9]", "", DATA$PN))
  }
  DATA <- data.frame(lapply(DATA, toupper), stringsAsFactors = FALSE)
  return(DATA)
}

zJSON2df <- function(filepath) {
  library(bibliometrix)
  library(tidyr)
  library(dplyr)
  library(jsonlite)
  
  json <- fromJSON(filepath)
  notes <- znotes2df(filepath)
  
  toreturn <- data.frame(key = json$items$key)
  
  # authors
  dAU <- do.call(rbind, lapply(seq(1,nrow(json$items)), FUN = function(i) {
    item <- json$items[i,]
    df <- item$creators[[1]][which(item$creators[[1]]$creatorType == 'author'),]
    df[['firstName']] <- gsub("[^:A-Z:]","", df$firstName)
    df[['name']] <- qdap::Trim(paste(df$lastName, df$firstName))
    return(data.frame(key = item$key, AU = paste(df$name, collapse = ";")))
  }))
  toreturn <- merge(toreturn, dAU, all.x = T)
  
  # titles
  dTI <- data.frame(key=json$items$key, TI=json$items$title)
  toreturn <- merge(toreturn, dTI, all.x = T)
  
  # publication source
  dSO <- data.frame(key=json$items$key, SO=json$items$publicationTitle)
  dSO[['JI']] <- as.vector(sapply(dSO[['SO']], FUN = function(x) { abbrevr::AbbrevTitle(x) }))
  toreturn <- merge(toreturn, dSO, all.x = T)
  
  # abstracts
  dAB <-data.frame(key=json$items$key, AB=json$items$abstractNote)
  toreturn <- merge(toreturn, dAB, all.x = T)
  
  # keywords - author keywords (DE), keywords associated to DBs (ID)
  dDE <- do.call(rbind, lapply(seq(1,nrow(json$items)), FUN = function(i) {
    item <- json$items[i,]
    
    de_keywords <- NA
    for (x in notes$value[which(notes$key == item$key & notes$field == 'Author keywords')]) {
      keywords <- stri_split_regex(x, "[;,.]")[[1]]
      keywords <- as.vector(sapply(keywords, FUN = function(k) {
        return(SemNetCleaner::singularize(qdap::Trim(tolower(k))))
      }))
      keywords <- paste(keywords, collapse = ';')
      if (is.na(de_keywords)) {
        de_keywords <- keywords
      } else {
        de_keywords <- paste0(de_keywords, ';', keywords)
      }
    }
    
    id_keywords <- NA
    for (x in notes$value[which(notes$key == item$key & notes$field == 'Keywords')]) {
      keywords <- stri_split_regex(x, "[;,.]")[[1]]
      keywords <- as.vector(sapply(keywords, FUN = function(k) {
        return(SemNetCleaner::singularize(qdap::Trim(tolower(k))))
      }))
      keywords <- paste(keywords, collapse = ';')
      if (is.na(id_keywords)) {
        id_keywords <- keywords
      } else {
        id_keywords <- paste0(id_keywords, ';', keywords)
      }
    }
    
    return(data.frame(key = item$key, DE = de_keywords, ID =id_keywords))
  }))
  toreturn <- merge(toreturn, dDE, all.x = T)
  
  # language
  dLA <- data.frame(key=json$items$key, LA=json$items$language)
  toreturn <- merge(toreturn, dLA, all.x = T)
  
  # publication type
  dDT <- data.frame(key=json$items$key, DT2=json$items$itemType)
  dDT[['DT2']] <- as.vector(sapply(dDT$DT2, FUN = function(x) {
    switch(x
           , 'bookSection' = return('Book section')
           , 'journalArticle' = return('Journal article')
           , 'conferencePaper' = return('Conference paper'))
  }))
  pnotes <- merge(notes[which(notes$field == 'Paper type'), c('key','value')], dDT, by='key', all.x = T)
  colnames(pnotes) <- c('key', 'DT', 'DT2')
  toreturn <- merge(toreturn, pnotes, all.x = T)
  
  # total citations
  dTC <- notes[which(notes$field == 'Cited by'), c('key','value')]
  colnames(dTC) <- c('key', 'TC')
  dTC[['TC']] <- as.integer(as.character(dTC[['TC']]))
  toreturn <- merge(toreturn, dTC, all.x = T)
  
  # cited references
  dCR <- notes[which(notes$field == 'References'), c('key','value')]
  colnames(dCR) <- c('key', 'CR')
  toreturn <- merge(toreturn, dCR, all.x = T)
  
  # author address
  dC1 <- notes[which(notes$field == 'Affiliation'), c('key','value')]
  colnames(dC1) <- c('key', 'C1')
  toreturn <- merge(toreturn, dC1, all.x = T)
  
  # Digital Object Identifier (DOI)
  dDI <- data.frame(key=json$items$key, DI=json$items$DOI) 
  toreturn <- merge(toreturn, dDI, all.x = T)
  
  # Published Year
  dPY <- data.frame(key=json$items$key, PY=json$items$date)
  dPY[['PY']] <- as.integer(as.character(dPY[['PY']]))   
  toreturn <- merge(toreturn, dPY, all.x = T)
  
  # Bibliographic Database
  dDB <- data.frame(key=json$items$key, DB=json$items$archive)
  dDB[['DB']] <- as.character(dDB[['DB']])
  toreturn <- merge(toreturn, dDB, all.x = T)
  
  return(zpostprocessing(toreturn))
}

