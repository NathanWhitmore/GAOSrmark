synopsis <- function(folder_name, site_species, data){

  suppressMessages(conflict_prefer("filter", "dplyr"))
  suppressMessages(conflict_prefer("here", "here"))

  setwd(paste0(here(),"/",folder_name))

  data.graph <- data %>%
    pivot_longer(cols = c(estimate, individuals),
                 names_to = "measure",
                 values_to = "population") %>%
    mutate(measure = fct_recode(measure, "no. of individuals" = "individuals"))

  graph <- ggplot()+
    geom_smooth(data = data.graph, aes(x=year, y=population),
                fill=NA, colour="red", linetype="dotted")+
    geom_point(data = data.graph, aes(x=year, y=population, colour=analysis, shape = measure), alpha= 0.8, size=4)+
    geom_errorbar(data =data.graph, aes(x=year, ymin=lcl, ymax=ucl, colour=analysis), width=0.2)+
    ggtitle(site_species)+
    theme(axis.title.x = element_text(margin= margin(9,0,0,0)))+
    theme(axis.title.y = element_text(margin= margin(0,9,0,0)))+
    theme(plot.subtitle = element_text(face=3, size=10))+
    scale_y_continuous(breaks = pretty_breaks(10))+
    scale_color_manual(values=c("violetred","forestgreen"))+
    scale_x_continuous(breaks = seq(from=min(data.graph$year), to=max(data.graph$year), by=1))+
    scale_shape_manual(values = c(16,10))+
    guides(color = guide_legend(override.aes = list(linetype = 0)))+
    theme(aspect.ratio = 1)+
    labs(subtitle = paste0("(", data.graph$estimator, "s)"))

  # save amalgamate files
  ggsave(paste0(site_species, "_synopsis", ".png"),
         width=10, height =10, units ="cm", scale =2)
  write.csv(data, paste0(data$site_species[1], "_synopsis", ".csv"), row.names = FALSE)

  return(graph)

}
