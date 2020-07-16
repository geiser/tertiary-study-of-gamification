
source(paste0(getwd(),'/common/df2qqs.R'))
source(paste0(getwd(),'/common/dealing_with_groups.R'))

identify_nonnormal_by_residuals <- function(data, dv, ivs, wid = "row.pos", to_remove = c()) {
  data <- df2qqs(data, ivs)
  repeat {
    df <- group_by_at(data[which(!data[[wid]] %in% to_remove), c(wid,dv,ivs)], vars(ivs))
    rownames(df) <- df[[wid]]
    model  <- lm(as.formula(paste(dv,"~", paste0(ivs, collapse = "*"))), data = df)
    if (shapiro_test(residuals(model))[["p.value"]] > 0.05) {
      break()
    }
    nonNormalityIds <- names(car::qqPlot(residuals(model)))
    to_remove <- c(nonNormalityIds, to_remove)
  }
  return(to_remove)
}

identify_nonnormal_by_groups <- function(data, dv, ivs, wid = "row.pos", to_remove = c()) {
  gdat <- df2qqs(data[,c(wid,dv,ivs)], ivs)
  to_remove <- c(get_ids_of_min_per_groups(gdat, ivs, wid), to_remove)
  repeat{
    gdat <- group_by_at(gdat[which(!gdat[[wid]] %in% to_remove),], vars(ivs))
    to_remove <- c(get_ids_of_min_per_groups(gdat, ivs, wid), to_remove)
    gdat <- group_by_at(gdat[which(!gdat[[wid]] %in% to_remove),], vars(ivs))
    
    shapiro <- subset(shapiro_test(gdat, vars = dv), p < 0.05)
    if (nrow(shapiro) == 0) break()  
    for (i in seq(1:nrow(shapiro))) {
      df <- subset_by_tbl(gdat, shapiro[i,], ivs = ivs)
      rownames(df) <- df[[wid]]
      nonNormalityIds <- names(car::qqPlot(as.formula(paste('~', dv)), data = df))
      to_remove <- c(nonNormalityIds, to_remove)
    }
  }
  return(setdiff(unique(to_remove), NA))
}

identify_nonnormal <- function(data, dv, ivs, wid = "row.pos", to_remove = c()) {
  nonNormalityIds <- identify_nonnormal_by_residuals(data, dv, ivs, wid, to_remove)
  nonNormalityIds <- c(identify_nonnormal_by_groups(data, dv, ivs, wid, nonNormalityIds), nonNormalityIds)
  return(unique(nonNormalityIds))
}
