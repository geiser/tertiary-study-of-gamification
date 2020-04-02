source(paste0(getwd(), '/lib/htm2txt.R'))

znotesRDF2df <- function(rdf, prefix = paste(
  'PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>'
  , 'PREFIX z: <http://www.zotero.org/namespaces/export#>'
  , 'PREFIX dcterms: <http://purl.org/dc/terms/>'
  , 'PREFIX bib: <http://purl.org/net/biblio#>'
  , 'PREFIX foaf: <http://xmlns.com/foaf/0.1/>'
  , 'PREFIX link: <http://purl.org/rss/1.0/modules/link/>'
  , 'PREFIX dc: <http://purl.org/dc/elements/1.1/>'
  , 'PREFIX prism: <http://prismstandard.org/namespaces/1.2/basic/>'
  , 'PREFIX vcard: <http://nwalsh.com/rdf/vCard#>')) {
  
  dNotes <- rdf_query(rdf, paste(
    prefix
    , "SELECT ?ref ?note ?title"
    , "WHERE { ?ref bib:authors ?_; dc:title ?title; dcterms:isReferencedBy ?nref. ?nref a bib:Memo; rdf:value ?note. }"
  ))
  dNotes[['page']] <- as.integer(stringr::str_match(dNotes$note,"page=([0-9]*)")[,2])
  dNotes[['note']] <- htm2txt(dNotes$note)
  dNotes[['whereis']] <- stringr::str_extract(dNotes[['note']], "\\(note on.*\\)")
  dNotes[['note']] <- stringr::str_remove_all(dNotes[['note']], '\\(note on.*\\)')
  
  notes <- do.call(rbind, sapply(seq(1,nrow(dNotes)), FUN = function(i) {
    pnote <- strsplit(dNotes[['note']][[i]], "\\:")[[1]]
    if (length(pnote) > 1) {
      sfield <- qdap::Trim(pnote[[1]])
      svalue <- qdap::Trim(paste0(pnote[2:length(pnote)], collapse = ':'))
      if (stringr::str_detect(sfield, "Review type") ||
          stringr::str_detect(sfield, "RO\\(.*\\)") ||
          stringr::str_detect(sfield, "Context") ||
          stringr::str_detect(sfield, "Number of selected studies") ||
          stringr::str_detect(sfield, "Period \\(.*\\)") ||
          stringr::str_detect(sfield, "Database") ||
          stringr::str_detect(sfield, "Search string") ||
          stringr::str_detect(sfield, "Search strategy") ||
          stringr::str_detect(sfield, "Exclusion criteria") ||
          stringr::str_detect(sfield, "Inclusion criteria") ||
          stringr::str_detect(sfield, "Selection procedure") ||
          stringr::str_detect(sfield, "Quality") ||
          stringr::str_detect(sfield, "Preventive step") ||
          stringr::str_detect(sfield, "Data extraction") ||
          stringr::str_detect(sfield, "RQ\\(.*\\)") ||
          stringr::str_detect(sfield, "Flaw on") ||
          stringr::str_detect(sfield, "Practition") ||
          stringr::str_detect(sfield, "Research agenda") ||
          stringr::str_detect(sfield, "Paper type") ||
          stringr::str_detect(sfield, "Cited By") ||
          stringr::str_detect(sfield, "References") ||
          stringr::str_detect(sfield, "Affiliation")
      ) {
        inferred <- F
        if (stringr::str_detect(svalue, '\\(.*inferred.*\\)')) {
          inferred <- T; svalue <- stringr::str_remove_all(svalue, '\\(.*inferred.*\\)')
        }
        return(data.frame(ref=dNotes[['ref']][i], title=dNotes[['title']][i], field=sfield
                          , value=svalue, inferred=inferred, page=dNotes[['page']][i]))
      }
    }
  }))
  
  ## validating notes
  sapply(c('Paper type','Affiliation'), FUN = function(x) {
    ndx <- anyDuplicated(notes$ref[which(notes$field == x)])
    if (ndx > 0) {
      cat("ERROR. Duplicate field: ", x, " for ", as.character(notes$ref[ndx]), ' records with titles: \n', as.character(notes$title[ndx]),'\n')
      stop()
    }
  })
  return(notes)
}

znote2list <- function(note, id = '', fields = list(
  'Affiliation' = "affiliation"
  , 'Cited by' = "cited by"
  , 'Context' = c("context", "contexts")
  , 'Correspondence address' = "correspondence address"
  , 'Paper type' = "paper type"
  , 'Reseach agenda' = "research agenda"
  , 'Review type' = "review type"
  , 'Flaw on gamification research' = "flaw on gamification research"
  , 'Exclusion criteria' = "exclusion criteria"
  , 'Inclusion criteria' = "inclusion criteria"
  , 'Data extraction process' = "data extraction process"
  , 'Data extraction form' = c("data extraction form", "data extraction forms", "data extraction formulary")
  , 'RO' = "ro\\(.+\\)"
  , 'RQ' = "rq\\(.+\\)"
  , 'Answer to RQ' = "answer to rq\\(.+\\)"
  , 'Method of data synthesis to answer RQ' = "method of data synthesis to answer rq\\(.+\\)"
  , 'Method of presenting details to answer RQ' = "method of presenting detail.*to answer rq\\(.+\\)"
  , 'Search string' = c("search string", "search term", "search terms")
  , 'Selection procedure' = "selection procedure"
  , 'Search strategy' = "search strategy"
  , 'Keywords' = c("keyword", "keywords")
  , 'Author keywords' = c("author keyword", "author keywords")
  , 'Databases' = c("database", "databases")
  , 'References' = c("reference", "references")
  , 'Number of selected studies' = c("number of selected studies", "number of selected study")
  , 'Period (selected studies)' = "period.*\\(.*\\)"
  , 'Practitioner guideline' = c("practitioner guideline", "practitioner guidelines", "practition guideline", "practition guidelines") 
  , 'How to minimize bias and errors in quality assessment' = c("preventive.*quality assessment process"
                                                                , "preventive.*quality assessment procedure")
  , 'How to minimize bias and errors in data extraction' = c("preventive.*data extraction process"
                                                             , "preventive.*data extraction procedure")
  , "Type of selected studies" = c("type of selected study", "type of selected studies")
  )) {
  library(qdap)
  
  pnote <- strsplit(note, "[\\:\\：]")[[1]]
  page <- as.integer(stringr::str_match(note,"page=([0-9]*)")[,2])
  if (length(pnote) > 1) {
    sfield <- Trim(strip(htm2txt(pnote[[1]]), char.keep = c("(",")","."), digit.remove = F))
    svalue <- Trim(paste0(pnote[2:length(pnote)], collapse = ':'))
    for (nfield in names(fields)) {
      if (any(sapply(fields[[nfield]], FUN = function(x) { stringr::str_starts(sfield, x) }))) {
        # get is_inferred
        inferred <- F
        if (stringr::str_detect(svalue, '\\(inferred.*\\)')) {
          inferred <- T; svalue <- stringr::str_remove_all(svalue, '\\(inferred.*\\)')
        }
        # validating inferred
        if (stringr::str_detect(svalue,'\\(.+inferred.*\\)')) {
          cat('\nErro in note with ',id,' as nfield:', paste0('(',nfield,')'),' as sfield:', paste0('(',sfield,')'),' - keyword "inferred" should start with (inferred.*) \n','note:\n',note,'\n')
          stop()
        }
        # validating sfields
        if (any(sapply(fields[[nfield]]
                       , FUN = function(x) {
                         stringr::str_starts(sfield, paste0(x,' .+'))
                         }))) {
          cat('\nErro in note with ',id,' as nfield:', paste0('(',nfield,')'),' as sfield:', paste0('(',sfield,')'),'\n','note:\n',note,'\n')
          stop()
        }
        # adding counting to method
        toadd_n <- c('RQ','RO','Answer to RQ','Method of data synthesis to answer RQ','Method of presenting details to answer RQ')
        if (nfield %in% toadd_n) {
          nfield <- paste0(nfield, '(', stringr::str_match(sfield,"\\(([0-9\\.]+)\\)")[,2] , ')')
        }
        
        svalue <- Trim(stringr::str_remove_all(htm2txt(svalue), '\\(note on.*\\)'))
        return(list(field=nfield, value=svalue, inferred=inferred, page=page))
      }
    }
  }
}

znotesJSON2df <- function(json) {
  df <- do.call(rbind, lapply(seq(1,nrow(json$items)), FUN = function(i) {
    key <- json$items$key[i]
    itemID <- json$items$itemID[i]
    return(do.call(rbind, lapply(json$items$notes[i][[1]], FUN = function(x) {
      l <- znote2list(x, id=paste0('Key:',key,'-itemID:',itemID))
      if (!is.null(l)) {
        return(data.frame(key=key, itemID=itemID
                          , field=l$field, value=l$value, inferred=l$inferred, page=l$page))
      }
    })))
  }))
  df[['ID']] <- as.vector(sapply(seq(1,nrow(df)), FUN = function(x) {
    return(digest::digest(as.list(df[x,])))
  }))
  df$field <- as.character(df$field)
  df$value <- as.character(df$value)
  return(df)
}

znotes2df <- function(
  filepath, format = 'json'
  , only_one=c('Affiliation','Cited by','Paper type','Type of selected studies', 'References'#, 'Author Keywords'
               , 'Review type', 'Context', 'Databases', 'Search string'
               , 'Number of selected studies', 'Type of selected studies')
  , should_have=c(only_one,'RO\\(.+\\)','RQ\\(.+\\)')
  ) {
  library(jsonlite)
  obj <- fromJSON(filepath)
  
  if (format == 'rdf')
    notes <- znotesRDF2df(obj)
  else if (format=='json')
    notes <- znotesJSON2df(obj)
  
  ## Print warnings erros
  lapply(obj$items$key, FUN = function(k) {
    title <- obj$items$title[which(obj$items$key == k)]
    nfields <- notes$field[which(notes$key == k)]
    lapply(should_have, FUN = function(nfield) {
      cfield <- sum(stringr::str_starts(nfields, nfield))
      if (cfield == 0) {
        cat('\n WARN: Its missing the value for field:',nfield,'in the Key:',k,paste0('(',title,')'),'\n')
      }
      if ((nfield %in% only_one) && cfield > 1) {
        cat('\n WARN: There is more than value for field:',nfield,'in the Key:',k,paste0('(',title,')'),'\n')
      }    
    })
  })
  
  return(notes)
}
