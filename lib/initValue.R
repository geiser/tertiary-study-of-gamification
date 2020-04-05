

source('lib/znotes2df.R')
source('lib/md2tlist.R')
source('lib/tlist2df.R')

initValue <- function(key, src, tmp = F, md5file = NULL) {
  
  
  getAuthorsFromItem <- function(dA) {
    dA <- dA[which(dA$creatorType == 'author'),]
    dA[['firstName']] <- gsub("[^:A-Z:]","", dA$firstName)
    dA[['name']] <- qdap::Trim(paste(dA$lastName, dA$firstName))
    return(paste(dA$name, collapse = ";"))
  }
  
  if (tmp) {
    if (is.null(md5file))
		md5file <- paste0('tmp/',key,'_',as.character(tools::md5sum(src)), '.rds')
    if (file.exists(md5file)) return(SemNetCleaner::read.data(file = md5file))
  }
  
  if (key == 'items') {
    val <- jsonlite::fromJSON(src)$items
    val[['authors']] <- sapply(val$creators, FUN = getAuthorsFromItem)
  } else if (key == 'annotations') {
    val <- znotes2df(src, 'json')
  } else if (key == 'reviewsByContext') {
    val <- md2tlist(src, rnode = 'From all contexts')
    val <- list(list = val, df = tlist2df(val))
  } else if (key == 'reviewsByType') {
    replace_labels <- list()
    replace_labels[['Descriptive or Mapping Review']] <- 'Mapping Review'
    replace_labels[['Structured Narrative']] <- 'Narrative Review'
    val <- md2tlist(src, rnode = 'All types of reviews', replace_labels = replace_labels)
    val <- list(list = val, df = tlist2df(val))
  } else if (key == 'reviewsByReviewObjective') {
    extract_pattern <- list(RO="RO\\([0-9\\.]+\\)")
    val <- md2tlist(src, key = "ID", rnode = 'All review objectives'
                    , extract_pattern = extract_pattern, include_hyperlink = T)
    val <- list(list = val, df = tlist2df(val, key = 'ID', def.attributes = list(hyperlink = NA, RO = NA)))
  } else if (key == 'reviewsByReviewQuestion') {
    extract_pattern <- list(RQ="RQ\\([0-9\\.]+\\)")
    val <- md2tlist(src, key = "ID", rnode = 'All review questions'
                    , extract_pattern = extract_pattern, include_hyperlink = T)
    val <- list(list = val, df = tlist2df(val, key = 'ID', def.attributes = list(hyperlink = NA, RQ = NA)))
  }
  
  if (tmp){ if (!file.exists(md5file)) saveRDS(val, file = md5file) }
  return(val)
}

