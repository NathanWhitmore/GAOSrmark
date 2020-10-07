# checks folder structure
# useful for understanding paths and empty folders

sloop_folder_structure <- function (folder) {


  suppressMessages(conflict_prefer("here", "here"))

   list.dirs(paste0(here(),"/",folder))

}






