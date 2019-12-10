
library(xml2)
library(jsonlite)

source('htm2txt.R')
source('znotes2df.R')


filepath <- 'src-data/zotero.json'
notes <- znotes2df(filepath = filepath)
zjson <- fromJSON(filepath)

## Research question
rq_notes <- notes[which(stringr::str_starts(notes$field, "RQ\\(.*\\)")),]


rq_dat <- rm_stopwords(tolower(rq_notes$value))
rq_dat <- do.call(c, lapply(rq_dat, FUN = function(x) { paste0(stemmer(x), collapse = '') }))

cl <- hclust(stringdist::stringdistmatrix(rq_dat, method = "cosine", useNames = "strings"))
rq_notes <- rq_notes[c(cl$order),]

##
lt_result <- lapply(seq(1,nrow(rq_notes)), FUN = function(j) {
  rq_row <- rq_notes[j,]
  is_inferred <- ''; if (rq_row$inferr) is_inferred <- '(inferred)'
  
  item <- zjson$items[which(zjson$items$itemID == rq_row$itemID),]
  
  attachment_list <- list()
  for (i in seq(1, nrow(item$attachments[[1]]))) {
    plink <- paste0(item$attachments[[1]][i,'uri'],'/file?page=', rq_row$page)
    filename <- item$attachments[[1]][i,'title']
    attachment_list <- append(attachment_list, list(node = structure(list(), LINK=plink, TEXT=filename)))
  }
  
  link <- item$uri
  if (length(attachment_list) == 1) {
    link <- attr(attachment_list$node, "LINK")
    attachment_list <- list()
  }
  
  rt_list <- list(node=structure(
    attachment_list
    , LINK=link
    , TEXT=paste(
      'ID:', rq_row$ID
      ,'<br/>', rq_row$value, is_inferred
      ,'<br/><br/>Title:',item$title
      )
    )
  )
  
  return(rt_list)
})

xml <- as_xml_document(
  list(map=structure(list(node=structure(lt_result, TEXT="Without classification")), version="1.1.0")))
write_xml(xml,'mm/research-question.mm')

