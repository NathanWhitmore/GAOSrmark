# Modelling averaging for closed capture Huggins analysis

closed_model_average <- function (site_species, year) {

  suppressMessages(conflict_prefer("filter", "dplyr"))
  suppressMessages(conflict_prefer("here", "here"))

  ModSet <- ModList

  # create matrix for estimates
  estm <- matrix(0, ncol=length(ModSet[[1]]$results$derived$`N Population Size`$estimate),
                 nrow=nrow(ModSet$model.table))

  # extract models weights
  wt <- ModSet$model.table$weight

  # create empty list for vcv matrices
  vcv <- vector("list", length=nrow(ModSet$model.table))

  # loop over each model
  for(i in 1:nrow(ModSet$model.table)){
    mod.num <- as.numeric(row.names(ModSet$model.table))
    x <- ModSet[[mod.num[i]]]$results
    estm[i, ] <- x$derived$`N Population Size`$estimate
    temp <- x$derived.vcv
    vcv[[i]] <- as.matrix(temp$'N Population Size')
  }

  # if have NaN in vcv matrix, model.average will error.
  # can change NaN's to zeros using rapply
  vcv <- rapply(vcv, f=function(x) ifelse(is.nan(x), 0, x), how="replace")

  # model.average function with what extracted in loop
  mod.ave <- model.average(list(estimate=estm, weight=wt, vcv=vcv))
  mod.ave$vcv <- NULL
  estimate <- mod.ave$estimate
  se <- mod.ave$se

  # correction for Mt+1
  f0 <- mod.ave$estimate-Mt1
  C <- exp(1.96*sqrt(log(1+(mod.ave$se/f0) ^2)))
  lcl <- Mt1+f0/C
  ucl <- Mt1+f0*C

  average.results <- data.frame(estimate, se, lcl, ucl, Mt1)
  average.results$year <- year

  # top model
  top.model <- str_remove(as.character(AICctable[1,1]),paste0(site_species,"."))

  # rename Mt1
  average.results <- average.results %>% rename(individuals = Mt1)


  # data analysis and estimator type
  average.results$analysis <- "closed capture"
  average.results$estimator <- "model average"
  average.results$site_species <- site_species

  # outputs to return
  outputs <- average.results


  # write files to folder
  write.csv(average.results, paste0(site_species, "_average_closed", ".csv"), row.names = FALSE)

  return(outputs)

}


