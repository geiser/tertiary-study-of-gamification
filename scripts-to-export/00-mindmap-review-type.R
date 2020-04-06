
library(xml2)
library(jsonlite)

source(paste0(getwd(), '/lib/htm2txt.R'))
source(paste0(getwd(), '/lib/znotes2df.R'))
source(paste0(getwd(), '/lib/initValue.R'))

filepath <- 'src-data/zotero.json'
notes <- znotes2df(filepath = filepath)
zjson <- fromJSON(filepath)

## Review type
rt_notes <- notes[which(notes$field == 'Review type'),]
ts_notes <- notes[which(notes$field == 'Type of selected studies'),]
type_df <- initValue('reviewsByContext', 'src-data/mom/type.xml')$df

lt_result <- lapply(seq(1,nrow(zjson$items)), FUN = function(j) {
  x <- zjson$items[j,]
  if (x$key %in% type_df$item) { return() }
  idx <- which(rt_notes$key == x$key)
  
  page <- rt_notes$page[idx]
  link <- x$uri 
  
  attachment_list <- list()
  for (i in seq(1,nrow(x$attachments[[1]]))) {
    plink <- paste0(x$attachments[[1]][i,'uri'],'/file?page=',page)
    filename <- x$attachments[[1]][i,'title']
    attachment_list <- append(attachment_list, list(node = structure(list(), LINK=plink, TEXT=filename)))
  }
  
  if (length(attachment_list) == 1) {
    link <- attr(attachment_list$node, "LINK")
    attachment_list <- list()
  }
  
  rt_list <- list(node=structure(
    attachment_list
    , LINK=link
    , TEXT=paste(
      'Key:', x$key
      ,'<br/><br/>Review type:',rt_notes$value[idx],'(by the authors);'
      ,'Types of selected studies:', ts_notes$value[idx]
      ,'<br/><br/>Title:',x$title
    )
  )
  )
  
  return(rt_list)
})
lt_result <- lt_result[!sapply(lt_result, FUN = is.null)]

xml <- as_xml_document(
  list(map=structure(list(node=structure(lt_result, TEXT="Without classification")), version="1.1.0")))
write_xml(xml,'src-data/mm/review-type3.mm')
