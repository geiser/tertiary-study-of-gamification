library(bibliometrix)

df <- convert2df(readFiles("zotero.bib"), dbsource = "scopus", format = "bibtex")
results <- biblioAnalysis(df, sep = ";")

resume <- summary(object = results, k = 10, pause = FALSE)

plot(x = results, k = 10, pause = FALSE)

NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ". ")


