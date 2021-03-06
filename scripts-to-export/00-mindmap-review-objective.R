
library(xml2)
library(jsonlite)

source(paste0(getwd(), '/lib/htm2txt.R'))
source(paste0(getwd(), '/lib/znotes2df.R'))
source(paste0(getwd(), '/lib/initValue.R'))

filepath <- 'src-data/zotero.json'
notes <- znotes2df(filepath = filepath)
zjson <- fromJSON(filepath)

## Review objectives
ro_notes <- notes[which(stringr::str_starts(notes$field, "RO\\(.*\\)")),]

ro_dat <- rm_stopwords(tolower(ro_notes$value))
ro_dat <- do.call(c, lapply(ro_dat, FUN = function(x) { paste0(stemmer(x), collapse = '') }))

cl <- hclust(stringdist::stringdistmatrix(ro_dat, method = "cosine", useNames = "strings"))
ro_notes <- ro_notes[c(cl$order),]

review_objective_df <- initValue('reviewsByReviewObjective', 'src-data/mom/review-objective.xml')$df

##
lt_result <- lapply(seq(1,nrow(ro_notes)), FUN = function(j) {
  ro_row <- ro_notes[j,]
  is_inferred <- ''; if (ro_row$inferr) is_inferred <- '(inferred)'
  item <- zjson$items[which(zjson$items$itemID == ro_row$itemID),]
  
  attachment_list <- list()
  for (i in seq(1, nrow(item$attachments[[1]]))) {
    plink <- paste0(item$attachments[[1]][i,'uri'],'/file?page=', ro_row$page)
    
    idxs <- which(review_objective_df$RO == ro_row$field & review_objective_df$hyperlink == plink)
    if (length(idxs) > 0) { return() }
    
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
      'ID:', ro_row$ID
      ,'<br/>', paste0(ro_row$field, ':'), ro_row$value, is_inferred
      ,'<br/><br/>Article Title:',item$title
    )
  )
  )
  
  return(rt_list)
})
lt_result <- lt_result[!sapply(lt_result, FUN = is.null)]

xml <- as_xml_document(
  list(map=structure(list(node=structure(lt_result, TEXT="Without classification")), version="1.1.0")))
write_xml(xml,'src-data/mm/review-objective3.mm')
