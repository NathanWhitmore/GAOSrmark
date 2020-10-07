# closed capture Huggins analysis


closed_capture_analysis <- function (path, site_species, year) {

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
  # Mh - p different for mixture
  Mh <- mark(ch, model="HugHet", model.name=paste0(site_species, ".Mh"),
           model.parameters=list(p=pmixture),adjust=TRUE)
  # Mth - p different for time; mixture additive
  Mth <- mark(ch, model="HugFullHet", model.name=paste0(site_species, ".Mth"),
            model.parameters=list(p=ptimemixtureshared),adjust=TRUE)

  message("Modelling complete\n")
  message("Estimates:")

  # read model files for estimability issues
  #  removal <- search.output.files(suppressWarnings(collect.models()),
  #                                 "Attempted ordering of parameters by estimatibility")

  updated <- suppressWarnings(collect.models())

  # remove model (estimability switch)
  #  updated <- if (is.null(removal) == TRUE) {
  #    suppressWarnings(collect.models())
  #  } else {
  #    remove.mark(suppressWarnings(collect.models()), removal)
  #  }


  # updated AICctable
  AICctable <<- suppressWarnings(model.table(updated))
  ModList <<- suppressWarnings(updated)

  # parsing top model
  name <- str_replace_all(AICctable[1,1], paste0(site_species,"."), "")
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

  # saving path
  setwd(paste0(here(), "/", path))
  write.csv(AICctable, paste0(site_species, "_AICctable_closed", ".csv"), row.names = FALSE)
  write.csv(data, paste0(site_species, "_estimates_closed", ".csv"), row.names = FALSE)


  return(data)
}






