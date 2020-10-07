# retrieve average robust design model for an amalgamated graph

find_robust <- function(period, species, site, average){

  suppressMessages(conflict_prefer("here", "here"))

  period <- period
  species <- species
  site <- site

  if (average == FALSE) {
    my.path <- paste0(here(),"/Sloop_robust_design/", period,"/", species,"/", site)
    my.file <- list.files(path = my.path, pattern = "estimates_robust.csv")
  } else {
    my.path <- paste0(here(),"/Sloop_robust_design/", period,"/", species,"/", site)
    my.file <- list.files(path = my.path, pattern = "average_robust.csv")
  }


  read.csv(paste0(my.path,"/", my.file))

}
