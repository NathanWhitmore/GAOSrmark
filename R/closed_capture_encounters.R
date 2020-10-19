# extracts encounter history from sloop extract

closed_capture_encounters <-function(file.name,
                                     site,
                                     include1s,
                                     start_date,
                                     finish_date){

  # resolve conflicts and suppress warnings
  suppressMessages(conflict_prefer("here", "here"))
  suppressMessages(conflict_prefer("filter", "dplyr"))

  df <- read.csv(paste0(here(),"/Closed_capture/",file.name))
  Site <- site
  start_date <- start_date
  finish_date <- finish_date
  include1s <- include1s

# correction date in size-class column
df$meta.Est.size.class
df$meta.Est.size.class <- str_replace_all(df$meta.Est.size.class,"\\?","")

df <- suppressWarnings(df %>%
  mutate(meta.Est.size.class = fct_recode(meta.Est.size.class,
                    "3" = "2-3",
                    "2" = "1-2",
                    "4" = "3-4")))

df <- suppressWarnings(df %>%
  mutate(meta.Est.size.class = fct_recode(meta.Est.size.class,
                                          "3" = "2-Mar",
                                          "2" = "1-Feb",
                                          "4" = "3-Apr")))



df$meta.Est.size.class <- as.numeric(as.character(df$meta.Est.size.class))
df$meta.Est.size.class

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


# filter by site
photo <- cutdown %>%
  filter(site == Site, 
         date >= ymd(start_date) &
           date <= ymd(finish_date))



# remove 1s subroutine
# 1. calculate mean size
photo <- photo %>%
  group_by (skink_ID) %>%
  mutate(mean_size = mean(size_class))

# 2. remove confirmed size 1s
photo <- if (include1s == TRUE) {
  photo
} else {
  photo %>% filter(mean_size > 1)
}

if (include1s == TRUE) {
  message("Size 1s: included")
} else {
  message("Size 1s: excluded")
}




# building encounter history

# make smaller data frame
small <- data.frame(as.factor(photo$skink_ID), photo$date)
colnames(small) <- c("skink_ID", "date")
small$counter <- 1


# make encounter history
encounter <- small %>%
  arrange(date) %>%
  distinct() %>%   # remove duplicates
  pivot_wider(names_from = date, values_from = counter)

# substitute zeros for missing values
# encounter <- encounter %>%
#  mutate_if(is.factor, as.character)%>%
#   replace(is.na(.), "0")

encounter <- encounter %>%
  mutate_if(is.factor, as.character)

encounter[is.na(encounter)] <- 0



# time intervals
sessions <- colnames(encounter)[2:ncol(encounter)]
sessions <- ymd(sessions)

# encounter histories
history <- encounter %>%
  unite("ch", start_date: finish_date , remove = TRUE, sep="")

message("Encounter history: saved in memory as 'encounter.his'")
encounter.his <<- history

message("Individuals recorded:")
return(dim(encounter.his)[1])

}















