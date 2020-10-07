find_closed <- function(period, species, site, average){

  suppressMessages(conflict_prefer("here", "here"))

  period <- period
  species <- species
  site <- site

  if (average == FALSE) {
    my.path <- paste0(here(),"/Closed_capture/", period,"/", species,"/", site)
    my.file <- list.files(path = my.path, pattern = "estimates_closed.csv")
  } else {
    my.path <- paste0(here(),"/Closed_capture/", period,"/", species,"/", site)
    my.file <- list.files(path = my.path, pattern = "average_closed.csv")
  }

  read.csv(paste0(my.path,"/", my.file))

}
