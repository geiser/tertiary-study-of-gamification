
source(paste0(getwd(),'/lib/zRDF2df.R'))
source(paste0(getwd(),'/lib/zJSON2df.R'))

zconvert2df <- function (filepath, format = "json") {
  
  md5file <- paste0(dirname(filepath),'/',as.character(tools::md5sum(filepath)), '.rds')
  if (file.exists(md5file)) return(read.data(file = md5file))
  
  cat("\nConverting your ", filepath, " collection into a bibliographic dataframe\n\n")
  if (length(setdiff(format, c("json", "rdf"))) > 0) {
    cat("\n 'format' argument is not properly specified")
    cat("\n 'format' argument has to be a character string matching 'json or 'rdf'.\n")
  }
  if (format == 'json'){
    M <- zJSON2df(filepath)
  } else if (format == 'rdf') {
    M <- zRDF2df(filepath)
  }
  
  #
  if ("PY" %in% names(M)) { M$PY = as.numeric(M$PY) } else { M$PY = NA }
  if ("TC" %in% names(M)) { M$TC = as.numeric(M$TC) } else { M$TC = NA }
  if (!("CR" %in% names(M))) { M$CR = "none" }
  cat("Done!\n\n")
  
  if ("C1" %in% names(M)) {
    cat("\nGenerating affiliation field tag AU_UN from C1:  ")
    M <- metaTagExtraction(M, Field = "AU_UN")
    cat("Done!\n\n")
  } else {
    M$C1 = NA
    M$AU_UN = NA
  }
  
  suppressWarnings(M <- metaTagExtraction(M, Field = "SR"))
  M$AU = unlist(lapply(strsplit(M$AU, ";"), function(x) {
    x = trimws(trimES(gsub("[[:punct:]]", " ", x)))
    x = paste(x, collapse = ";")
  }))
  SR = M$SR
  tab = table(SR)
  tab2 = table(tab)
  ind = as.numeric(names(tab2))
  ind = ind[which(ind > 1)]
  if (length(ind) > 0) {
    for (i in ind) {
      indice = names(which(tab == i))
      for (j in indice) {
        indice2 = which(SR == j)
        SR[indice2] = paste(SR[indice2], as.character(1:length(indice2)), sep = " ")
      }
    }
  }
  row.names(M) <- SR
  
  M[['DT']] <- Hmisc::capitalize(tolower(M[['DT']]))
  M[['DT2']] <-Hmisc::capitalize(tolower(M[['DT2']]))  
  
  M[['DT']][which(M[['DT']] == 'Book chapter (workshop)')] <- 'Workshop (book chapter)'
  M[['DT']][which(M[['DT']] == 'Book chapter (conference)')] <- 'Conference (book chapter)'
  
  if (!file.exists(md5file)) saveRDS(M, file = md5file)
  return(M)
}
