# useful for visualising monitoring and other field activity

monitoring_overview <- function (file_name){

  suppressMessages(conflict_prefer("here", "here"))
  suppressMessages(conflict_prefer("filter", "dplyr"))

  df <- read.csv(paste0(here(), "/Closed_capture/",file_name))

  df <- df %>% filter(meta.Event == "PhotoID")

  if (is.null(df$meta.Date) == TRUE) {
    df$meta.Date =NA
  } else {
    df$meta.Date
  }

# subset
cutdown <- data.frame(df$sloop_id,
                      df$individual_id,
                      df$meta.Est.size.class,
                      df$meta.Capture.time,
                      df$meta.Date,
                      df$meta.Event,
                      df$meta.Site)


# rename
colnames(cutdown) <- c("sloop_id",
                       "skink_ID",
                       "size_class",
                       "capture_date",
                       "recent_date",
                       "event",
                       "site")


# standardise dates
cutdown$capture_date <- ymd(substr(cutdown$capture_date, 1, 10))
cutdown$recent_date <- dmy(cutdown$recent_date)
cutdown$date <- coalesce(cutdown$recent_date, cutdown$capture_date)
cutdown$month <- month(cutdown$date, label=TRUE)
cutdown$year <- year(cutdown$date)

# remove unnecessary
cutdown <- cutdown[,-c(4,5)]

# photo-resight
# filter by site

photo <- cutdown %>%
  mutate(site = fct_recode(site, "Unnamed" = ""))

# filter by year
photo <- photo %>% filter (year >= 2006)

# sampling months
graph <- ggplot()+
  geom_point(data = photo, aes(x = month, y = year, colour = site), shape = 15, size = 5)+
  scale_y_continuous(breaks=seq(from=min(photo$year), to =max(photo$year), by=1))+
  facet_wrap(.~site)+
  theme(legend.position = "none")+
  xlab("\nMonth")+
  ylab("Year\n")+
  theme(panel.grid.minor.y = element_blank())

return(graph)

}
