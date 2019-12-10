source(paste0(getwd(),"/lib/zconvert2df.R"))

library(bibliometrix)

M <- zconvert2df(paste0(getwd(), '/src-data/zotero.json'))

results <- biblioAnalysis(M, sep = ";")
resume <- summary(object = results, k = 10, pause = FALSE)

plot(x = results, k = 10, pause = FALSE)

NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ". ")
