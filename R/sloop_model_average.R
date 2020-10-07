# calculates model averages for sloop data using
# data held in memory

sloop_model_average <- function (start_year) {

  suppressMessages(conflict_prefer("filter", "dplyr"))
  suppressMessages(conflict_prefer("here", "here"))

  ModSet <- ModList

  # reading intervals
  intervals.ch <- scan(paste0(site_species, "_surveys.txt"), nlines=1)
  intervals.ch[intervals.ch ==0] <- NA
  intervals <- na.omit(intervals.ch) %>% round(1)
  year <- round(c(start_year, cumsum(intervals) + start_year),0)

  # separate encounter history into yearly sessions
  sep.history <- input.ch[1] %>%
    separate(ch, into = str_c("Session", 1:length(sec.periods)),
             sep=cumsum(sec.periods) )

  sep.history[] <- sapply(sep.history, as.numeric)
  sep.history[sep.history==0] <- NA
  raw.counts <- colSums(!is.na(sep.history))

  # count known individual captured - normally known as 'Mt+1'
  Mt1 <- colSums(!is.na(sep.history))

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

  # manual colours for legend
  # colors <- c("model average" = "black", "no. individuals" = "forestgreen")

  colors <- c( "top model" = "purple", "model average" = "black",
               "no. individuals" = "forestgreen")
  shapes <- c("top model" = 16, "model average" = 16, "no. individuals" = 10)


  ave.graph <<- ggplot() +
    geom_smooth(data = top.estimates, aes(x=year, y=estimate), fill=NA, colour="red", linetype="dotted")+
    geom_point(data = top.estimates, aes(x=year -0.1, y=estimate, colour="top model",
                                         shape ="top model"), size=3)+
    geom_errorbar(data = top.estimates, aes(x=year -0.1, ymin=lcl,
                                            ymax=ucl, colour="top model"),
                  width=0.08)+
    geom_point(data = average.results, aes(x=year , y=Mt1, colour="no. individuals",
                                           shape ="no. individuals"), size=3)+
    geom_point(data = average.results, aes(x=year , y=estimate, colour="model average",
                                           shape = "model average"),
               size=3)+
    geom_errorbar(data = average.results, aes(x=year , ymin=lcl,
                                              ymax=ucl, colour="model average"),
                  width=0.08)+

    ggtitle(site_species)+
    labs(subtitle = paste("Model average vs", top.model, "(top-ranked model)"))+

    scale_shape_manual(values = shapes )+
    scale_color_manual(values = colors)+
    theme(plot.subtitle = element_text(face=3, size=10))+
    labs(color  = "Legend",  shape = "Legend")+
    guides(color = guide_legend(override.aes = list(linetype = 0)))+
    scale_y_continuous(breaks = pretty_breaks(10))+
    scale_x_continuous(breaks = seq(from=min(top.estimates$year)-1,
                                    to=max(top.estimates$year), by =1))+
    xlab("\nYear")+
    ylab("Estimate\n")



  # outputs to return
  outputs <- list(average.results, ave.graph)

  # data analysis and estimator type
  average.results$analysis <- "robust design"
  average.results$estimator <- "model average"
  average.results$site_species <- site_species

  # write files to folder
  ggsave(paste0(site_species, "_average_robust", ".png"),  width=10, height =10, units ="cm", scale =2)
  write.csv(average.results, paste0(site_species, "_average_robust", ".csv"), row.names = FALSE)



  return(outputs)

}
