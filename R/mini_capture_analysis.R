# closed capture Huggins analysis


# closed capture Huggins analysis


mini_capture_analysis <- function (path, site_species, year) {

  suppressMessages(conflict_prefer("filter", "dplyr"))
  suppressMessages(conflict_prefer("here", "here"))


  # setwd to save
  setwd(paste0(here(), "/", path, "/Results"))


  # change to character
  ch <- data.frame(ch = as.character(encounter.his$ch))
  ch$ch <- as.character(ch$ch)

  # read the site_species.inp file
  Mt1 <<- length(ch$ch)



  # Huggins models
  pdotshared=list(formula=~1,share=TRUE)
  ptimeshared=list(formula=~time,share=TRUE)
  ptime.c=list(formula=~time+c,share=TRUE)
  ptimemixtureshared=list(formula=~time+mixture,share=TRUE)
  pmixture=list(formula=~mixture)

  message("Modelling beginning")

  # Candidate model set
  # Mo
  Mo <- mark(ch, model="Huggins", model.name=paste0(site_species, ".Mo"),
             model.parameters=list(p=pdotshared))
  # Mt
  Mt <- mark(ch, model="Huggins", model.name=paste0(site_species, ".Mt"),
             model.parameters=list(p=ptimeshared),adjust=TRUE)


  message("Modelling complete: Mo and Mt only \n")
  message("Estimates:")

  updated <- suppressWarnings(collect.models())

  # updated AICctable
  AICctable <<- suppressWarnings(model.table(updated))
  ModList <<- suppressWarnings(updated)


  # parsing top model
  name <- str_replace_all(AICctable[1,3], paste0(site_species,"."), "")
  mod <- eval(parse(text=name))

  # get derived estimates

  data <- mod$results$derived$`N Population Size`
  data$individuals <- length(ch$ch)
  data$year <- year

  top.estimates <<- data

  # data analysis and estimator type
  data$analysis <- "closed capture"
  data$estimator <- "top model"
  data$site_species <- site_species

  # saving path getwd()
  setwd(paste0(here(), "/", path))
  write.csv(AICctable[,-c(1,2)], paste0(site_species, "_AICctable_closed", ".csv"), row.names = FALSE)
  write.csv(data, paste0(site_species, "_estimates_closed", ".csv"), row.names = FALSE)


  return(data)
}













