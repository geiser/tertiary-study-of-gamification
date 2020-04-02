
library(xml2)
library(jsonlite)

source(paste0(getwd(), '/lib/htm2txt.R'))
source(paste0(getwd(), '/lib/znotes2df.R'))
source(paste0(getwd(), '/lib/initValue.R'))


filepath <- 'src-data/zotero.json'
notes <- znotes2df(filepath = filepath)
zjson <- fromJSON(filepath)

## Review type
ctx_notes <- notes[which(notes$field == 'Context'),]
review_df <- initValue('reviewsByContext', 'src-data/mom/context.xml')$df

lt_result <- lapply(seq(1, nrow(zjson$items)), FUN = function(j) {
  x <- zjson$items[j,]
  if (x$key %in% review_df$item) { return() }
  idx <- which(ctx_notes$key == x$key)
  
  page <- ctx_notes$page[idx]
  link <- x$uri 
  
  attachment_list <- list()
  for (i in seq(1,nrow(x$attachments[[1]]))) {
    plink <- paste0(x$attachments[[1]][i,'uri'], '/file?page=', page)
    filename <- x$attachments[[1]][i,'title']
    attachment_list <- append(attachment_list, list(node = structure(list(), LINK=plink, TEXT=filename)))
  }
  
  if (length(attachment_list) == 1) {
    link <- attr(attachment_list$node, "LINK")
    attachment_list <- list()
  }
  
  return(list(node=structure(
    attachment_list
    , LINK=link
    , TEXT=paste(
      'Key:', x$key
      , '<br/><br/>Context:', ctx_notes$value[idx], '(by the authors);'
      , '<br/><br/>Title:', x$title
    ))
  ))
})
lt_result <- lt_result[!sapply(lt_result, FUN = is.null)]

filename <- paste0(getwd(), '/src-data/mm/context3.mm')
xml <- as_xml_document(
  list(map=structure(list(node=structure(lt_result, TEXT="Without classification")), version="1.1.0")))
write_xml(xml, filename)
