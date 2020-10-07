# call this with a site_species string (e.g. Airport_grand) to get back a collection of candidate models
# the site_species.inp and site_species_surveys.txt files extracted from Sloop should be in the current directory

sloop_robust_design <- function (path, site_species, start_year) {

  suppressMessages(conflict_prefer("filter", "dplyr"))
  suppressMessages(conflict_prefer("here", "here"))

  # set path to find species and year combination
  setwd(paste0(here(),"/", path))

  # make site_species findable for sloop_model_average
  site_species <<- site_species
  start_year <<- start_year

  # read the site_species.inp file
  input.ch <- convert.inp(site_species)

  # read the site_species_surveys.txt first line of inter-survey gaps (in years)
  intervals.ch <<- scan(paste0(site_species, "_surveys.txt"), nlines=1)


  # build up the various components of the candidate models

  # Our models are robust design Huggins without and with full heterogeneity (RDHuggins (Mo and Mt) or RDHFHet (M+h and Mt+h))

  # all models have S depend on time (i.e. survival differs between years)
  S.time <- list(formula=~time)

  # Mo models have detection probability vary between years (sessions), but fixed within a survey series for a year
  # share=TRUE for p terms indicates that c=p (we assume photography has no impact on animals so captuer = recapture)
  p.session <- list(formula=~session, share=TRUE)

  # our models in previous years have ignored temporary emigration (i.e. fixed Gamma'' to zero); these are the no movement (nm) models
  GammaDoublePrime.fixed <- list(fixed=0)
  GammaPrime.fixed <- list(fixed=1)


  # Mt models have detection probability vary between years (sessions) and within a survey series for a year (time)
  p.time.session <- list(formula=~-1+session:time,share=TRUE)


  # for the RDHFHet models we need to add the mixture component to p (for M+h we use split detectabilities that are common across all surveys in the year)
  # pi is the mixture proportions; making it per session is consistent with previous analysis
  p.session.mixture <- list(formula=~session+mixture, share=TRUE)
  pi.session <- list(formula=~session)


  # Mt+h model adds the mixture (two detecability classes) per survey, but we keep pi (mixture proportion) per session as in previous years analysis
  # Question: Does anything other than model complexity drive us to vary pi by session rather than session:time?
  # Answer: We belive some animals are hyper-vigilant, others not.
  #	  This split should remain constant through a (closed) session, but may vary between years as the population changes.
  p.time.session.mixture=list(formula=~-1+session:time+mixture,share=TRUE)


  # Three of the four classical temporary movement models for robust design models make sense:
  #	nm (above) - not so much biologically plausible as a potentially parsimonious simplified model
  #	random movement (rm) - probability of moving out equals that of moving in (Gamma''=Gamma', achieved by shared=TRUE) and varies by year
  GammaDoublePrime.random=list(formula=~time,share=TRUE)
  # 	Markovian movement (Mm) - probabilities of moving in and out differ (but each is fixed across years, keeping parameters manageable)
  GammaDoublePrime.dot=list(formula=~1)
  GammaPrime.dot=list(formula=~1)

  #	the "even flow" model is ignored (net in/out is zero) as no basis to suspect we have resource constrained conditions
  #	(i.e. conditions under which an animal leaving makes space for another to come in)
  # Which of nm, rm, Mm is most biologically plausible probably varies by site
  # (e.g. constrained by fence might favour nm, much more habitat adjacent to study site than in it might favour Mm)
  # Note: above list isn't exhaustive
  #	(e.g. could fix Gamma''=Gamma' across years GammaDoublePrime.dotshare=list(formula=~1,share=TRUE); intermediate between nm and rm?)
  # Note: not all Gamma parameters can be estimated in all models (e.g. first/last years), this does not automatically cast doubt on those models

  print("Running model")

  # save to results folder
   setwd(paste0(here(),"/", path,"/Results"))

  Mo.nm <- mark(data=input.ch, model="RDHuggins", model.name=paste0(site_species, ".Mo.nm"), time.intervals=intervals.ch, model.parameters=list(S=S.time, p=p.session, GammaDoublePrime=GammaDoublePrime.fixed, GammaPrime=GammaPrime.fixed))
  Mt.nm <- mark(data=input.ch, model="RDHuggins", model.name=paste0(site_species, ".Mt.nm"), time.intervals=intervals.ch, model.parameters=list(S=S.time, p=p.time.session, GammaDoublePrime=GammaDoublePrime.fixed, GammaPrime=GammaPrime.fixed))
  Mh.nm <- mark(data=input.ch, model="RDHFHet", model.name=paste0(site_species, ".Mh.nm"), time.intervals=intervals.ch, model.parameters=list(S=S.time, p=p.session.mixture, pi=pi.session, GammaDoublePrime=GammaDoublePrime.fixed, GammaPrime=GammaPrime.fixed))
  Mht.nm <- mark(data=input.ch, model="RDHFHet", model.name=paste0(site_species, ".Mht.nm"), time.intervals=intervals.ch, model.parameters=list(S=S.time, p=p.time.session.mixture, pi=pi.session, GammaDoublePrime=GammaDoublePrime.fixed, GammaPrime=GammaPrime.fixed))

  Mo.rm <- mark(data=input.ch, model="RDHuggins", model.name=paste0(site_species, ".Mo.rm"), time.intervals=intervals.ch, model.parameters=list(S=S.time, p=p.session, GammaDoublePrime=GammaDoublePrime.random))
  Mt.rm <- mark(data=input.ch, model="RDHuggins", model.name=paste0(site_species, ".Mt.rm"), time.intervals=intervals.ch, model.parameters=list(S=S.time, p=p.time.session, GammaDoublePrime=GammaDoublePrime.random))
  Mh.rm <- mark(data=input.ch, model="RDHFHet", model.name=paste0(site_species, ".Mh.rm"), time.intervals=intervals.ch, model.parameters=list(S=S.time, p=p.session.mixture, pi=pi.session, GammaDoublePrime=GammaDoublePrime.random))
  Mht.rm <- mark(data=input.ch, model="RDHFHet", model.name=paste0(site_species, ".Mht.rm"), time.intervals=intervals.ch, model.parameters=list(S=S.time, p=p.time.session.mixture, pi=pi.session, GammaDoublePrime=GammaDoublePrime.random))

  Mo.Mm <- mark(data=input.ch, model="RDHuggins", model.name=paste0(site_species, ".Mo.Mm"), time.intervals=intervals.ch, model.parameters=list(S=S.time, p=p.session, GammaDoublePrime=GammaDoublePrime.dot, GammaPrime=GammaPrime.dot))
  Mt.Mm <- mark(data=input.ch, model="RDHuggins", model.name=paste0(site_species, ".Mt.Mm"), time.intervals=intervals.ch, model.parameters=list(S=S.time, p=p.time.session, GammaDoublePrime=GammaDoublePrime.dot, GammaPrime=GammaPrime.dot))
  Mh.Mm <- mark(data=input.ch, model="RDHFHet", model.name=paste0(site_species, ".Mh.Mm"), time.intervals=intervals.ch, model.parameters=list(S=S.time, p=p.session.mixture, pi=pi.session, GammaDoublePrime=GammaDoublePrime.dot, GammaPrime=GammaPrime.dot))
  Mht.Mm <- mark(data=input.ch, model="RDHFHet", model.name=paste0(site_species, ".Mht.Mm"), time.intervals=intervals.ch, model.parameters=list(S=S.time, p=p.time.session.mixture, pi=pi.session, GammaDoublePrime=GammaDoublePrime.dot, GammaPrime=GammaPrime.dot))

  print("Done model")

  # session count for model averaging later
  input.ch <<- input.ch
  sec.periods  <<- Mo.nm$nocc.secondary


  # read model files for estimability issues

#  removal <- search.output.files(suppressWarnings(collect.models()),"Attempted ordering of parameters by estimatibility")
  updated <- suppressWarnings(collect.models())

  # If removal of estimatable parameters required
#  updated <- if (is.null(removal) == TRUE) {
#    suppressWarnings(collect.models())
#  } else {
#    suppressWarnings(remove.mark(collect.models(), removal))
#  }


  # updated AICctable
  AICctable <<- suppressWarnings(model.table(updated))
  ModList <<- suppressWarnings(updated)

  # get additional variables for graphing
  base <-  start_year

  # parsing top model
  name <- str_replace_all(AICctable[1,1], paste0(site_species,"."), "")
  mod <- eval(parse(text=name))

  # get derived estimates
  data <- mod$results$derived$`N Population Size`

  # years
  setwd(paste0(here(),"/", path))

  # no of individuals
  # reading intervals
  intervals.ch <- scan(paste0(site_species, "_surveys.txt"), nlines=1)
  intervals.ch[intervals.ch ==0] <- NA
  intervals <- na.omit(intervals.ch) %>% round(1)
  year <- c(start_year, cumsum(intervals) + start_year)

  # separate encounter history into yearly sessions
  sep.history <- input.ch[1] %>%
    separate(ch, into = str_c("Session", 1:length(sec.periods)),
             sep=cumsum(sec.periods) )

  sep.history[] <- sapply(sep.history, as.numeric)
  sep.history[sep.history==0] <- NA
  raw.counts <- colSums(!is.na(sep.history))

  # count known individual captured - normally known as 'Mt+1'
  Mt1 <- colSums(!is.na(sep.history))
  data$individuals <- Mt1

  # use base to as .inp doesn't have named years
  data$year <- round(c(base, cumsum(intervals) + base),0)

  graph <-  ggplot()+
    geom_smooth(data = data, aes(x=year, y=estimate), fill=NA, colour="red", linetype="dotted")+
    geom_point(data = data, aes(x=year, y=estimate), size=3, alpha=0.5)+
    geom_errorbar(data =data, aes(x=year, ymin=lcl, ymax=ucl), width=0.08)+
    ggtitle(site_species)+
    labs(subtitle = paste("top-ranked model =", name,"\nmodel weight =",
                          format(round(AICctable[1,5],3), nsmall=3)))+
    theme(axis.title.x = element_text(margin= margin(9,0,0,0)))+
    theme(axis.title.y = element_text(margin= margin(0,9,0,0)))+
    theme(plot.subtitle = element_text(face=3, size=10))+
    scale_y_continuous(breaks = pretty_breaks(10))+
    scale_x_continuous(breaks = seq(from=min(data$year)-1, to=max(data$year), by =1))+
    theme(axis.title.x = element_text(margin= margin(9,0,0,0)))+
    theme(axis.title.y = element_text(margin= margin(0,9,0,0)))+
    theme(plot.subtitle = element_text(face=3, size=10))+
    xlab("Year")+
    ylab("Estimate")


  # data analysis and estimator type
  data$analysis <- "robust design"
  data$estimator <- "top model"
  data$site_species <- site_species

  # saving
  ggsave(paste0(site_species, "_estimates_robust", ".png"), width=10, height =10, units ="cm", scale =2)
  write.csv(AICctable, paste0(site_species, "_AICctable_robust", ".csv"), row.names = FALSE)
  write.csv(data, paste0(site_species, "_estimates_robust", ".csv"), row.names = FALSE)

  outputs <- list(AICctable, graph)

  # outputs
  top.estimates <<- data

  return(outputs)
}








