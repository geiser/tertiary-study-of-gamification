
source(paste0(getwd(),'/lib/znotes2df.R'))

zRDF2df <- function(filepath, prefix = paste(
  'PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>'
  , 'PREFIX z: <http://www.zotero.org/namespaces/export#>'
  , 'PREFIX dcterms: <http://purl.org/dc/terms/>'
  , 'PREFIX bib: <http://purl.org/net/biblio#>'
  , 'PREFIX foaf: <http://xmlns.com/foaf/0.1/>'
  , 'PREFIX link: <http://purl.org/rss/1.0/modules/link/>'
  , 'PREFIX dc: <http://purl.org/dc/elements/1.1/>'
  , 'PREFIX prism: <http://prismstandard.org/namespaces/1.2/basic/>'
  , 'PREFIX vcard: <http://nwalsh.com/rdf/vCard#>')) {
  if (!(require(magrittr))){ install.packages("magrittr"); require(magrittr, quietly=TRUE)}
  if (!(require(rdflib))){ devtools::install_github("ropensci/rdflib"); require(rdflib, quietly=TRUE)}
  if (!(require(abbrevr))){ devtools::install_github('patrickbarks/abbrevr') }
  if (!(require(SemNetCleaner))){ install.packages("SemNetCleaner") }
  library(bibliometrix)
  library(tidyr)
  library(dplyr)
  
  rdf <- rdf_parse(filepath, format = 'rdfxml')
  notes <- znotes2df(rdf, prefix = prefix)
  
  toreturn <- rdf_query(rdf, paste(prefix, "SELECT ?ref WHERE { ?ref bib:authors ?authors. }"))
  
  # authors
  dAU <- rdf_query(rdf, paste(
    prefix
    , "SELECT ?ref ?surname ?givenname"
    , "WHERE {"
    , "?ref bib:authors ?authors ."
    , "?authors ?p ?person ."
    , "?person a foaf:Person ; foaf:surname ?surname ; foaf:givenName ?givenname."
    ,"}"
  ))
  dAU[['givenname']] <- gsub("[^:A-Z:]","", dAU$givenname)
  dAU[['name']] <- qdap::Trim(paste(dAU[['surname']], dAU[['givenname']]))
  dAU <- summarise(AU = paste(name, collapse = ";"), group_by(dAU, ref))
  
  toreturn <- merge(toreturn, dAU, all.x = T)
  
  # titles
  dTI <- rdf_query(rdf, paste(
    prefix
    , "SELECT ?ref ?TI"
    , "WHERE { ?ref bib:authors ?authors; dc:title ?TI. }"
  ))
  dTI[['TI']] <- as.character(dTI[['TI']])
  
  toreturn <- merge(toreturn, dTI, all.x = T)
  
  # publication source
  dSO <- rdf_query(rdf, paste(
    prefix
    , "SELECT ?ref ?SO"
    , "WHERE {"
    , "{ ?ref bib:authors ?authors; dcterms:isPartOf ?journal. ?journal a bib:Journal; dc:title ?SO} UNION"
    , "{ ?ref bib:authors ?authors; dcterms:isPartOf ?book. ?book a bib:Book; dc:title ?SO}"
    , "}"))
  dSO[['JI']] <- as.vector(sapply(dSO[['SO']], FUN = function(x) { abbrevr::AbbrevTitle(x) }))
  dSO[['SO']] <- as.character(dSO[['SO']]); dSO[['JI']] <- as.character(dSO[['JI']])
  
  toreturn <- merge(toreturn, dSO, all.x = T)
  
  # abstracts
  dAB <- rdf_query(rdf, paste(
    prefix
    , "SELECT ?ref ?AB"
    , "WHERE { ?ref bib:authors ?authors; dcterms:abstract ?AB . }"
  ))
  dAB[['AB']] <- as.character(dAB[['AB']])
  
  toreturn <- merge(toreturn, dAB, all.x = T)
  
  # keywords - all keywords are considered author keywords
  dDE <- rdf_query(rdf, paste(
    prefix
    , "SELECT ?ref ?keyword"
    , "WHERE { ?ref bib:authors ?authors; dc:subject ?keyword . }"
  )) # all is considered author keyword
  dDE <- dDE[!startsWith(dDE$keyword, '_:'),]
  dDE[['keyword']] <- as.vector(sapply(qdap::Trim(dDE$keyword), FUN = function(x) { 
    return(SemNetCleaner::singularize(x))
  }))
  dDE <- summarise(DE = paste(keyword, collapse = ";"), group_by(distinct(dDE), ref))
  dDE[['DE']] <- as.character(dDE[['DE']])
  
  toreturn <- merge(toreturn, dDE, all.x = T)
  toreturn[['ID']] <- rep(NA, nrow(toreturn))
  
  # language
  dLA <- rdf_query(rdf, paste(
    prefix
    , "SELECT ?ref ?LA"
    , "WHERE { ?ref bib:authors ?authors; z:language ?LA . }"
  ))
  dLA[['LA']] <- as.character(dLA[['LA']])
  
  toreturn <- merge(toreturn, dLA, all.x = T)
  
  # publication type
  dDT <- rdf_query(rdf, paste(
    prefix
    , "SELECT ?ref ?DT2"
    , "WHERE { ?ref bib:authors ?authors; z:itemType ?DT2 . }"
  ))
  
  dDT[['DT2']]<- as.character(sapply(dDT$DT2, FUN = function(x) { switch(x
                                                            , 'bookSection' = return('Book section')
                                                            , 'journalArticle' = return('Journal article')
                                                            , 'conferencePaper' = return('Conference paper'))  }))
  
  tnotes <- notes[which(notes$field == 'Paper type'), c('ref','value')]
  tnotes <- merge(tnotes, dDT, by='ref', all.x = T)
  colnames(tnotes) <- c('ref', 'DT', 'DT2')
  tnotes[['DT']] <- as.character(tnotes[['DT']])
  tnotes[['DT2']] <- as.character(tnotes[['DT2']])
  
  #View(tnotes); cat(' \n\n until to assign M \n\n'); stop()
  
  toreturn <- merge(toreturn, tnotes, all.x = T)
  
  
  
  # total citations
  dTC <- notes[which(notes$field == 'Cited By'), c('ref','value')]
  colnames(dTC) <- c('ref', 'TC')
  dTC[['TC']] <- as.integer(dTC[['TC']])
  
  toreturn <- merge(toreturn, dTC, all.x = T)
  
  # cited references
  dCR <- notes[which(notes$field == 'References'), c('ref','value')]
  colnames(dCR) <- c('ref', 'CR')
  dCR[['CR']] <- as.character(dCR[['CR']])
  
  toreturn <- merge(toreturn, dCR, all.x = T)

  # Author Address
  dC1 <- notes[which(notes$field == 'Affiliation'), c('ref','value')]
  colnames(dC1) <- c('ref', 'C1')
  dC1[['C1']] <- as.character(dC1[['C1']])
  
  toreturn <- merge(toreturn, dC1, all.x = T)
  
  # Digital Object Identifier (DOI)
  dDI <- rdf_query(rdf, paste(
    prefix
    , "SELECT ?ref ?DI"
    , "WHERE { ?ref bib:authors ?authors; dcterms:isPartOf ?ipo. ?ipo dc:identifier ?DI. }"
  ))
  dDI <- dDI[stringr::str_starts(dDI$DI,'DOI'),]
  dDI[['DI']] <- qdap::Trim(stringr::str_remove(dDI$DI,'DOI'))
  dDI[['DI']] <- as.character(dDI[['DI']])
  
  toreturn <- merge(toreturn, dDI, all.x = T)
  
  # Published Year
  dPY <- rdf_query(rdf, paste(
    prefix
    , "SELECT ?ref ?PY"
    , "WHERE { ?ref bib:authors ?authors; dc:date ?PY. }"
  ))
  dPY[['PY']] <- as.numeric(dPY[['PY']])

  toreturn <- merge(toreturn, dPY, all.x = T)
  
  # Bibliographic Database
  dDB <- rdf_query(rdf, paste(
    prefix
    , "SELECT ?ref ?DB"
    , "WHERE { ?ref bib:authors ?authors; z:archive ?DB. }"
  ))  
  dDB[['DB']] <- as.character(dDB[['DB']])
  
  toreturn <- merge(toreturn, dDB, all.x = T)

  M <- toreturn[,setdiff(colnames(toreturn), c('ref'))]
  M$RP <- rep(NA,nrow(M))
  M$C1 <- toupper(M$C1)
  M$DB <- toupper(M$DB)
  M$AU <- toupper(M$AU)
  M$SO <- toupper(M$SO)
  M$JI <- toupper(M$JI)
  #M[is.na(M)] <- ''
  
  ####### 
  if ("PY" %in% names(M)){M$PY=as.numeric(M$PY)} else {M$PY=NA}
  if ("TC" %in% names(M)){M$TC=as.numeric(M$TC)} else {M$TC=NA}
  if (!("CR" %in% names(M))){M$CR="none"}
  M$AU=gsub(intToUtf8(8217),intToUtf8(39),M$AU)
  
  ## AU_UN field creation
  if ("C1" %in% names(M)){
    cat("\nGenerating affiliation field tag AU_UN from C1:  ")
    
    M <- metaTagExtraction(M, Field="AU_UN")
    cat("Done!\n\n")
  } else{
    M$C1=NA
    M$AU_UN=NA}
  
  ### SR field creation
  suppressWarnings(M <- metaTagExtraction(M, Field="SR"))
  
  ## AU normalization
  M$AU=unlist(lapply(strsplit(M$AU,";"), function(x){
    x=trimws(trimES(gsub("[[:punct:]]"," ",x)))
    x=paste(x,collapse=";")
  }))
  
  # identify duplicated SRs 
  SR=M$SR
  tab=table(SR)
  tab2=table(tab)
  ind=as.numeric(names(tab2))
  ind=ind[which(ind>1)]
  if (length(ind)>0){
    for (i in ind){
      indice=names(which(tab==i))
      for (j in indice){
        indice2=which(SR==j)
        SR[indice2]=paste(SR[indice2],as.character(1:length(indice2)),sep=" ")
      }
    }
  }
  
  row.names(M) <- SR
  
  return(M)
}

