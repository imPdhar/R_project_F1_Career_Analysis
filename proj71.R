#Plan; intro dataset methods results discussion conclusion appendix
#Things to remember for chunks
#Introduction
#Formula One....Why are the stats interesting?Why is it worth exploring?
 
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(scales)
library(ggfortify)

install.packages("GGally")
library(GGally)
install.packages("khroma")
library(khroma)
mypal <- colour("okabeito")(8)
mypal <- mypal[c(2:8, 1)]
names(mypal) <- NULL

palette( mypal )


#Loading in the csv files, using '_0' to indicate an untouched original file
setwd("C:/Users/Aoife/OneDrive/Desktop/Project")
drivers_0 <- read.csv("drivers.csv")
races_0 <- read.csv("races.csv")
results_0 <- read.csv("results.csv")
circuits_0 <- read.csv("circuits.csv")
constructor_results_0 <- read.csv("constructor_results.csv")
constructor_standings_0 <- read.csv("constructor_standings.csv")
constructors_0 <- read.csv("constructors.csv")
driver_standings_0 <- read.csv("driver_standings.csv")
lap_times_0 <- read.csv("lap_times.csv")
pit_stops_0 <- read.csv("pit_stops.csv")
qualifying_0 <- read.csv("qualifying.csv")
races_0 <- read.csv("races.csv")
status_0 <- read.csv("status.csv")

#Our methods

#There are many approaches to the tasks we have set out for ourselves.combining csvsmetricsnormalising etc.

#EDA - Checking different potential plots etc

#Stats based things ...

#1. Exploratory Data Analysis

#Initial check of the dataset

#Like any data set, there is always the possibility of missing data or other issues.Our first step was some initial exploratory data analysis of the individual csv files.

datasets <- list(
  drivers_0, races_0, results_0, circuits_0,
  constructor_results_0, constructor_standings_0,
  constructors_0, driver_standings_0, lap_times_0,
  pit_stops_0, qualifying_0
)

names(datasets) <- c(
  "drivers", "races", "results", "circuits",
  "constructor_results", "constructor_standings",
  "constructors", "driver_standings", "lap_times",
  "pit_stops", "qualifying"
)

lapply(datasets, function(df) {
  sapply(df, function(x)
    sum(is.na(x) | x == "" | x == "\\N" | x == "###" | x == "null")
  )
})


#We found missing values in;races.csv - only relating to times of day and free practice dates.results.csv - position, time, fastest lap, fastest speed - issues with historic data.constructor_results.csv - status issue.qualifying.csv - related to backdated data that was not recorded at the time or was not part of the gp weekend.

#To check the severity of these issues, we compare some missing values to total values.

#checking large issue with status - 12608 missing values
#length(constructor_results_0$status)  #12625 => 12608/12625 missing
#we simply won't make use of this column as its not contextually significant and lacks foundations

#Generally this shows a strong data set, where there are no issues regarding the files and attributes we plan to make use of.

#We also explore other areas to catch a glimpse into the size and shape of the dataset.

#Check how many seasons we have
#range(races_0$year)

#How many unique values
length(unique(circuits_0$circuitId))  #77
length(unique(drivers_0$driverId))  #861
length(unique(constructors_0$constructorId))  #212

#CHECK THIS -- not sure if its good or bad
summary(results_0$grid)
summary(results_0$positionOrder)


#We also had to consider the possibility of needing to normalise our data. To explore this possibility it was necessary to check how the championship has changed over the years, including number of races, number of drivers, points awarded, etc.
#(These code blocks don't need to be in the report but the graphs can show skills and prove why we need to normalise things later => building foundations)

#Number of races per season
races_0 %>%
  count(year) %>%
  ggplot(aes(x = year, y = n)) +
  geom_col(fill = "firebrick3")  +
  geom_bar(
    stat = "identity"
  ) +
  labs(title = "Number of Races per Season (1950-2024)")


races_0 %>%
  count(year) %>%
  ggplot(aes(x = year, y = n)) + 
  theme_minimal() +
  geom_bar(stat = "identity", fill = "firebrick3") +
  labs(title = "Number of Races per Season (1950-2024)")


#Number of drivers per season
#another ggplot?
#purely for our foundations - to show we started strong


#Number of points awarded
#WEB SCRAPE FROM WIKIPEDIA ===========
#https://en.wikipedia.org/wiki/List_of_Formula_One_World_Championship_points_scoring_systems
#showing more r skills !! 


summary(select(results_0, positionOrder, points, grid, laps))
#output needs checking for laps -- 200 laps?
#or we ignore completely 


#EDA - bringing in the data - checking shapes and distributions

#We can change this section to better suit what we do later on,

#What's here atm is meh
  
top_circuits <- races_0 %>%
  group_by(name) %>%
  summarise(race_count = n()) %>%
  arrange(desc(race_count)) %>%
  slice(1:15)
  
ggplot(top_circuits, aes(x = reorder(name, race_count), y = race_count)) +
  geom_col(fill = "firebrick3") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Most Frequently Used Circuits (1950–2024)",
    x = "Circuit",
    y = "Number of Races Held"
  )
  
  
most_wins_driver <- results_0 %>%
  filter(positionOrder == 1) %>%
  left_join(drivers_0, by = "driverId") %>%
  mutate(driver_name = paste(forename, surname)) %>%
  count(driver_name, sort = TRUE) %>%
  slice(1:10)
  
head(most_wins_driver)

ggplot(most_wins_driver, aes(x = reorder(driver_name, n), y = n)) +
  geom_col(fill = "firebrick3") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top 10 Drivers by Total Wins (1950-2024)",
    x = "Driver",
    y = "Wins"
  )


top_constructor_wins <- results_0 %>%
  filter(positionOrder == 1) %>%
  left_join(constructors_0, by = "constructorId") %>%
  count(name, sort = TRUE) %>%
  slice(1:10)
  
ggplot(top_constructor_wins, aes(x = reorder(name, n), y = n)) +
  geom_col(fill = "blue4") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top 10 Constructors by Total Wins",
    x = "Constructor",
    y = "Wins"
  )

#Cleaning and re-organising the data (removing the _0 from some)
  
#The next step we took was to clean and organise the data.Given that the data was separated into so many smaller files, the first step was to merge data files so as to allow for smoother calculations later on. It also allowed us the opportunity to eliminated duplicate columns, or data that would only accumulate as garbage which would not directly affect our analysis.
  
# i.e. The original drivers csv file contained {driverId, driverRef, number, code, forename, surname, dob, nationality, url}.--- But our analysis would not benefit from factors such as date of birth or car number, so we opted to select the favorable data attributes and create a new csv file, containing the drivers' full name and driverId.This aided in our ability to follow the data and general efficiency.

#drivers listed by driverId and full name
drivers <- drivers_0 %>%
  select(driverId, forename, surname) %>%  
  mutate(driver_name = paste(forename, surname)) %>%
  select(driverId, driver_name)

#results
results <- results_0 %>%
  select(raceId, driverId, constructorId, positionOrder, points) %>% 
  filter(!is.na(positionOrder))  # remove rows where position is NA

#joining drivers, races, and results
drivers_results <- results %>%
  left_join(drivers, by = "driverId") %>%
  left_join(races_0 %>% select(raceId, year), by = "raceId")

#creating a summary of the above, ordered by year and alphabetically by driver
drivers_results_summary <- drivers_results %>%
  group_by(driver_name, year) %>%
  summarise(
    total_points = sum(points, na.rm = TRUE),
    races_won = sum(positionOrder == 1, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(year))  

#create a csv file to store this data
write_csv(drivers_results_summary, "driver_resuslts_summary.csv")


head(drivers_results)
head(drivers_results_summary)


head(drivers)

#Or also creating a table of the results, combining data from drivers.csv, races.csv, and results.csv


head(results)

#Our next step was to


library(dplyr)

#driver_name <- c(drivers$forename, drivers$surname)

driver_summary_filt <- drivers_results %>%
  filter(!is.na(driver_name)) %>%
  group_by(driver_name, year) %>%
  summarise(
    races_won = sum(positionOrder == 1, na.rm = TRUE),
    total_races = n_distinct(raceId),
    .groups = "drop"
  ) %>%
  arrange(desc(races_won))


# Find top 10 drivers by total races won
top10_drivers <- driver_summary_filt %>%
  group_by(driver_name) %>%
  summarise(total_wins = sum(races_won, na.rm = TRUE)) %>%
  arrange(desc(total_wins)) %>%
  slice_head(n = 10) %>%
  pull(driver_name)

# Filter data for only those top 10
driver_summary_top10 <- driver_summary_filt %>%
  filter(driver_name %in% top10_drivers)

# Reorder the x-axis so the most successful driver appears last
driver_summary_top10 <- driver_summary_top10 %>%
  mutate(driver_name = factor(driver_name,
                              levels = top10_drivers))

# Plot
ggplot(driver_summary_top10, aes(x = driver_name, y = races_won, fill = factor(year))) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(option = "C", direction = -1) +
  labs(
    title = "Number of Races Won by Top 10 Drivers per Year",
    x = "Driver",
    y = "Races Won",
    fill = "Year"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )



driver_summary_filt$year <- factor(driver_summary_filt$year,
                                   levels = rev(sort(unique(driver_summary_filt$year))))


ggplot(driver_summary_filt, aes(x = driver_name, y = races_won, fill = factor(year))) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(option = "C", direction = -1) +
  labs(
    title = "Number of Races Won by Driver per Year",
    x = "Driver",
    y = "Races Won",
    fill = "Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5))



driver_summary_atl1 <- drivers_results_summary %>%
  filter(races_won > 0)

ggplot(driver_summary_atl1, aes(x = driver_name, y = races_won, fill = factor(year))) +
  geom_bar(stat = "identity") +
  labs(
    title = "Number of Races Won by Driver per Year",
    x = "Driver",
    y = "Races Won",
    fill = "Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

#==========GETTING CARRIED AWAY WITH THE WHOLE GOAT THING ================

#---- Compute driver-level performance metrics ----

#uses total points but not normalised => favors more recent drivers


goat_metrics <- results_0 %>%
  left_join(drivers_0 %>% mutate(driver_name = paste(forename, surname)), by = "driverId") %>%
  left_join(races_0 %>% select(raceId, year), by = "raceId") %>%
  group_by(driver_name) %>%
  summarise(
    races_started = n(),
    wins = sum(positionOrder == 1, na.rm = TRUE),
    podiums = sum(positionOrder <= 3, na.rm = TRUE),
    points_total = sum(points, na.rm = TRUE),
    dnfs = sum(grepl("Accident|Collision|Engine|Gearbox|Transmission|Clutch|Hydraulics|Electrical|Spun off|Radiator|Suspension|Brakes|Differential|
                     Overheating|Mechanical|Tyre|Puncture", status_0, ignore.case = TRUE)),
    races_finished = races_started - dnfs,
    win_rate = wins / races_started,
    podium_rate = podiums / races_started,
    points_per_race = points_total / races_started,
    finish_rate = races_finished / races_started
  ) %>%
  filter(races_started > 20) %>% # exclude one-off drivers
  arrange(desc(points_per_race))

head(goat_metrics)


#Top drivers by win rate


goat_metrics %>%
  slice_max(order_by = win_rate, n = 15) %>%
  ggplot(aes(x = reorder(driver_name, win_rate), y = win_rate)) +
  geom_col(fill = "firebrick3") +
  coord_flip() +
  labs(
    title = "Top 15 Drivers by Career Win Rate",
    x = "Driver",
    y = "Win Rate"
  ) +
  theme_minimal()


#This is still before normalising eras => favors recent drivers BUT this is where i built from

#Should podiums be only 2nd, 3rd place?? since we account for wins separately


library(scales)

#Compute per-race stats
driver_stats <- results_0 %>%
  left_join(drivers_0 %>% mutate(driver_name = paste(forename, surname)), by = "driverId") %>%
  left_join(races_0 %>% select(raceId, year), by = "raceId") %>%
  group_by(driver_name, year) %>%
  summarise(
    races = n(),
    wins = sum(positionOrder == 1, na.rm = TRUE),
    podiums = sum(positionOrder <= 3,  na.rm = TRUE),
    points = sum(points, na.rm = TRUE),
    .groups = "drop"
  )

#Add season context (races per year)
season_lengths <- races_0 %>%
  group_by(year) %>%
  summarise(races_in_season = n(), .groups = "drop")

driver_stats1 <- driver_stats %>%
  left_join(season_lengths, by = "year") %>%
  mutate(
    win_rate = wins / races,  #only accounts for races they started (accounts for substitutions/partial seasons)
    season_win_share = wins / races_in_season, #accounts for all races whether they took part of not
    points_per_race = points / races
  )

#Aggregate per driver (career totals, normalised)
driver_summary <- driver_stats1 %>%
  group_by(driver_name) %>%
  summarise(
    total_races = sum(races, na.rm = TRUE),
    total_wins = sum(wins, na.rm = TRUE),
    total_points = sum(points, na.rm = TRUE),
    avg_win_rate = sum(wins, na.rm = TRUE) / sum(races, na.rm = TRUE),
    avg_points_per_race = sum(points, na.rm = TRUE) / sum(races, na.rm = TRUE),
    avg_season_win_share = sum(wins, na.rm = TRUE) / sum(races_in_season, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(total_races > 10)


#THIS is checking fangio - he doesn't have a avg win rate of 1 => error below!
driver_stats1 %>% filter(str_detect(driver_name, regex("Fangio", ignore_case = TRUE))) %>% print(n = Inf)
  
  
  #Normalise across all drivers
driver_summary1 <- driver_summary %>%
  mutate(
    across(c(avg_win_rate, avg_points_per_race, avg_season_win_share),
           ~ (.-mean(.)) / sd(.), .names = "z_{col}"),  #usinf z scores to find dist from avg
    goat_score = (avg_win_rate + avg_points_per_race + avg_season_win_share) / 3
  ) %>%
  arrange(desc(goat_score)
  )
  
  
head(driver_summary1, 20)
  
  
  
#checking for errors before it tries to run it
stopifnot(all(c("driverId","raceId","positionOrder","points") %in% names(results_0)))
stopifnot(all(c("driverId","forename","surname") %in% names(drivers_0)))
stopifnot(all(c("raceId","year") %in% names(races_0)))
  
  #Clean types and positionOrder & points
results_clean <- results_0 %>%
  mutate(
    positionOrder = as.numeric(positionOrder),
    points = as.numeric(points),
  )

  #Remove duplicate driver-race rows if they exist - looking for double entries
results_nod <- results_0 %>%
  mutate(
    positionOrder = as.numeric(positionOrder),
    points = as.numeric(points),
    laps = as.numeric(laps)
  ) %>%
  # keep only valid race results (some rows are scrappy or placeholder)
  filter(!is.na(driverId), !is.na(raceId)) %>%
  # keep the most informative record per race-driver combo
  arrange(raceId, driverId, desc(laps), desc(points)) %>%
  distinct(raceId, driverId, .keep_all = TRUE)

dupes <- results_nod %>%
  group_by(raceId, driverId) %>%
  filter(n() > 1) %>%
  ungroup()

if(nrow(dupes) > 0) {
  message("Warning: duplicate driver-race rows found. We'll keep the first occurrence per (raceId, driverId).")
  results_clean <- results_clean %>%
    arrange(raceId, driverId) %>%
    distinct(raceId, driverId, .keep_all = TRUE)
}
  
results_f <- results_nod %>%
  mutate(driverId = as.integer(driverId)) %>%
  left_join(
    drivers_0 %>%
      mutate(driverId = as.integer(driverId),
             driver_name = paste(forename, surname)),
    by = "driverId"
  ) %>%
  left_join(
    races_0 %>%
      mutate(raceId = as.integer(raceId)) %>%
      select(raceId, year),
    by = "raceId"
  )

  
  
  
results_f %>%
  filter(!is.na(driver_name)) %>%
  select(driver_name) %>%
  head()

driver_career <- results_f %>%
  group_by(driver_name) %>%
  summarise(
    total_races = n_distinct(raceId),
    total_wins = sum(positionOrder == 1, na.rm = TRUE),
    total_podiums = sum(positionOrder <= 3, na.rm = TRUE),
    total_points = sum(points, na.rm = TRUE),
    win_rate = total_wins / total_races,
    podium_rate = total_podiums / total_races,
    points_per_race = total_points / total_races,
    .groups = "drop"
  )
  
head(driver_career)
  
driver_career %>%
  filter(driver_name %in% c("Lewis Hamilton", "Max Verstappen", "Juan Fangio")) %>%
  knitr::kable()

  
#Big thoughts on sprint races being included in results - messing with the max points per race weekend, also fastest lap ======================================
    
 
  
 # era splits
results_era <- results_f %>%
  mutate(
    era = case_when(
      year < 1961 ~ "1950-1960 (8 pts win)",
      year < 1991 ~ "1961-1990 (9 pts win)",
      year < 2003 ~ "1991-2002 (10 pts win)",
      year < 2010 ~ "2003-2009 (10-8-6 pts system)",
      TRUE        ~ "2010+ (25 pts win)"
    )
  )
  
era_points_ref <- tibble(
  era = c(
    "1950-1960 (8 pts win)",
    "1961-1990 (9 pts win)",
    "1991-2002 (10 pts win)",
    "2003-2009 (10-8-6 pts system)",
    "2010+ (25 pts win)"
  ),
  max_points_for_win = c(8, 9, 10, 10, 25)
)

results_era <- results_era %>%
  left_join(era_points_ref, by = "era")


era_summary <- results_era %>%
  group_by(era) %>%
  summarise(max_points = max(points, na.rm = TRUE))

driver_career <- results_era %>%
  group_by(driver_name) %>%
  summarise(
    total_races = n_distinct(raceId),
    total_wins = sum(positionOrder == 1, na.rm = TRUE),
    total_podiums = sum(positionOrder <= 3, na.rm = TRUE),
    total_points = sum(points, na.rm = TRUE),
    avg_points_max = mean(max_points_for_win, na.rm = TRUE),
    win_rate = total_wins / total_races,
    podium_rate = total_podiums / total_races,
    points_per_race = total_points / total_races,
    points_per_race_norm = points_per_race / avg_points_max,
    .groups = "drop"
  )
  
driver_yearly <- results_era %>%
   group_by(driver_name, year) %>%
   summarise(
     races_in_year = n_distinct(raceId),
     wins_in_year = sum(positionOrder == 1, na.rm = TRUE),
     .groups = "drop"
   ) %>%
   arrange(driver_name, year)
 
# Example quick inspection for Fangio
driver_yearly %>% filter(str_detect(driver_name, regex("Fangio", ignore_case = TRUE))) %>% print(n = Inf)

  # Create normalized GOAT score
driver_summary <- driver_career %>%
  filter(total_races >= 5) %>% # exclude one-offs to reduce noise 
  mutate(
    s_win_rate = scales::rescale(win_rate, to = c(0,1)),
    s_points_per_race = scales::rescale(points_per_race, to = c(0,1)),
    s_podium_rate = scales::rescale(podium_rate, to = c(0,1)),
    goat_score = (s_win_rate + s_points_per_race + s_podium_rate) / 3
  ) %>%
  arrange(desc(goat_score))

  
  #Show top drivers
driver_summ <- driver_career %>%
  filter(total_races >= 20) %>%
  mutate(
    s_win_rate = rescale(win_rate, to = c(0, 1)),
    s_podium_rate = rescale(podium_rate, to = c(0, 1)),
    s_points_norm = rescale(points_per_race_norm, to = c(0, 1)),
    s_points_per_race_norm = rescale(points_per_race_norm, to = c(0,1)),
    s_longevity = rescale(log10(total_races), to = c(0, 1)),  # reward long careers
    goat_score = (0.4 * s_win_rate) + (0.2 * s_podium_rate) + 
      (0.2 * s_longevity) + (0.2 * s_points_per_race_norm)
    #FIX THIS ^
  ) %>%
  arrange(desc(goat_score))

driver_summ <- driver_summ %>%
  select(driver_name, goat_score, everything())
  
  
print(driver_summ, 40)
  
  
  
  
ggplot(driver_summ %>% slice_max(goat_score, n = 30),
       aes(x = reorder(driver_name, goat_score), y = goat_score, fill = goat_score)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(low = "firebrick4", high = "firebrick1") +
  labs(title = "Top 30 Greatest F1 Drivers (Era-Normalized GOAT Score)",
       x = "Driver", y = "GOAT Score") +
  theme_minimal()

  
  
  
  
  
  # final race id per year (highest round)
final_races <- races_0 %>%
  group_by(year) %>%
  summarise(final_race_id = raceId[which.max(round)], .groups = "drop")

# get the driver standing entries that correspond to the final race of each year
champions_by_year <- driver_standings_0 %>%
  inner_join(final_races, by = c("raceId" = "final_race_id")) %>%
  # position may be string "1" or numeric positionOrder; accept either
  filter(position == "1") %>%
  left_join(drivers_0 %>% 
              mutate(driver_name = paste(forename, surname)) %>%
              select(driverId, driver_name),
            by = "driverId") %>%
  select(year, driverId, driver_name)

champions_flag <- champions_by_year %>%
  mutate(has_championship = 1)


champions <- champions_by_year %>%
  group_by(driver_name) %>%
  summarise(
    championships_won = n(),                   # count how many championships
    years_won = paste(sort(year), collapse = ", "),  # list of years
    has_championship = 1
  ) %>%
  arrange(desc(championships_won))


print(champions)




#PCA for goat score

  
drivers_pca <- driver_summ %>%
  select(driver_name, s_podium_rate, s_win_rate, s_points_per_race_norm, s_longevity) %>%
  left_join(champions, by = "driver_name") %>%
  mutate(has_championship = coalesce(has_championship, 0))


names(drivers_pca)

pca1 <- prcomp(drivers_pca[, 2:5])
  
print(pca1)
  
  
  
summary(pca1)

  
round(pca1$rotation, 2)

  
round(pca1$sdev, 2)
  
  
plot(pca1, main = "Goat metrics")
  
  
plot(pca1, main = "Goat metrics", type = "l")
  
  
abbrev <- c("Podium", "Win", "Points", "Longevity")
  
par(mfrow = c(2,2))

for(i in 1:ncol(pca1$rotation)){
  barplot(pca1$rotation[,i], names.arg = abbrev, ylim = c(-1,1), col = as.factor(abbrev), 
          main = paste("PC", i, sep = ""))
}
  
  
  
  
pairs(drivers_pca[,2:5],
      main = "Pairs plots")
  
  
eigen(cor(drivers_pca[,2:5]))
  
  
head(drivers_pca)
  
  
drivers_pca <- drivers_pca %>%
  mutate(
    has_championship = factor(has_championship,
                              levels = c(0, 1),
                              labels = c("Non-Champion", "Champion"))
  )
  
pca_vars <- drivers_pca %>%
  select(s_podium_rate, s_win_rate, s_points_per_race_norm, s_longevity)
  
pca_result <- prcomp(pca_vars, scale. = TRUE)
  
drivers_pca <- bind_cols(drivers_pca, as.data.frame(pca_result$x))
  
summary(pca_result)
  
  
  
driver_summ <- driver_summ %>%
  mutate(driver_name = str_trim(tolower(driver_name)))
  
champions <- champions %>%
  mutate(driver_name = str_trim(tolower(driver_name))) %>%
  distinct(driver_name) %>%
  mutate(has_championship = 1)
  
drivers_pca <- driver_summ %>%
  select(driver_name, s_podium_rate, s_win_rate, s_points_per_race_norm, s_longevity) %>%
  left_join(champions, by = "driver_name") %>%
  mutate(
    has_championship = ifelse(is.na(has_championship), 0, has_championship),
    has_championship = factor(has_championship, levels = c(0, 1),
                              labels = c("Non-Champion", "Champion"))
  )
table(drivers_pca$has_championship, useNA = "ifany")
  
pca_vars <- drivers_pca %>%
  select(s_podium_rate, s_win_rate, s_points_per_race_norm, s_longevity)
  
pca_result <- prcomp(pca_vars, scale. = TRUE)
  
drivers_pca <- bind_cols(drivers_pca, as.data.frame(pca_result$x))
  
mypal <- c("Non-Champion" = "gray70", "Champion" = "gold")
  
ggpairs_pca <- ggpairs(
  data = drivers_pca,
  columns = c("PC1", "PC2", "PC3", "PC4"),
  mapping = aes(color = has_championship, shape = has_championship),
  diag = list(continuous = "blank"), # blank diagonal
  title = "Pairs Plot of PCA Components (Colored by Championship)"
)
  
for(i in 1:4){
  for(j in 1:4){
    ggpairs_drivers_pca[i,j] <- (ggpairs_drivers_pca[i,j] + scale_colour_manual(values = mypal) 
                                 + scale_fill_manual(values = mypal)
    )
  }
}
  
  
ggpairs_pca
  
  
  
  
library(tidyverse)
  
# Select the metrics
goat_metrics <- goat_metrics %>%
  select(driver_name, win_rate, podium_rate, points_per_race, finish_rate)

# Remove driver_name for PCA
pca_data <- goat_metrics %>% select(-driver_name)
  
  # Run PCA with 4 components
pca_model <- prcomp(pca_data, center = TRUE, scale. = TRUE)
summary(pca_model)
  
  # Extract the PCA-transformed data
pca_scores <- as.data.frame(pca_model$x)
pca_scores$driver_name <- goat_metrics$driver_name
  
  
  
library(GGally)
  
  # Make a 4x4 scatterplot matrix
p <- ggpairs(
  pca_scores,
  columns = 1:4,  # PC1, PC2, PC3, PC4
  upper = list(continuous = wrap("points", alpha = 0.6, size = 2)),
  lower = list(continuous = wrap("points", alpha = 0.6, size = 2)),
  diag = list(continuous = "blank")  # blank diagonal
)
  
p + theme_minimal() +
  labs(title = "Scatterplot Matrix of PCA Components in GOAT Score Metrics")

  
  
  
  # Select relevant columns for PCA
pca_data <- driver_summ %>%
  select(s_win_rate, s_points_per_race_norm, s_podium_rate)

  # Run PCA
pca_model <- prcomp(pca_data, center = TRUE, scale. = TRUE)
  
  # Summary of PCA
summary(pca_model)
  
  # View loadings to see contribution of each metric
pca_model$rotation
  
  # Extract PC1 as a "statistical GOAT score"
driver_summ$pca_goat <- pca_model$x[,1]
  
  # Rank drivers by PCA-based GOAT score
driver_summ <- driver_summ %>%
  arrange(desc(pca_goat)) %>%
  mutate(pca_rank = row_number())
  
  # Quick check of top 10 drivers by PCA GOAT
head(driver_summ %>% select(driver_name, pca_goat, pca_rank), 10)
  
  # Biplot with ggfortify
autoplot(pca_model, data = driver_summ, colour = 'pca_goat',
         loadings = TRUE, loadings.label = TRUE) +
  scale_colour_gradient(low = "lightblue", high = "firebrick3") +
  labs(title = "PCA of F1 Driver Performance Metrics",
       colour = "PC1 (GOAT Score)") +
  theme_minimal()
  
  
  
  
  # Scatter plot of your era-normalized GOAT vs PCA GOAT
ggplot(driver_summ, aes(x = goat_score, y = pca_goat)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick3") +
  labs(title = "Comparison: Era-Normalized GOAT vs PCA GOAT",
       x = "Original GOAT Score",
       y = "PCA GOAT Score") +
  theme_minimal()
  
  
  
  # Select metrics and driver names
metrics <- driver_summ %>%
  select(driver_name, s_win_rate, s_points_per_race_norm, s_podium_rate)
  
  # Convert to long format for faceting
metrics_long <- metrics %>%
  pivot_longer(
    cols = c(s_win_rate, s_points_per_race_norm, s_podium_rate),
    names_to = "metric",
    values_to = "value"
  )
  
  
  # Run PCA on normalized metrics
pca_model <- prcomp(metrics %>% select(-driver_name), center = TRUE, scale. = TRUE)
  
  # Extract scores and combine with driver names
scores <- as.data.frame(pca_model$x)
scores$driver_name <- metrics$driver_name
  
  # Extract loadings for plotting
loadings <- as.data.frame(pca_model$rotation)
loadings$metric <- rownames(loadings)
  
  # Merge scores and loadings for plotting
loadings_long <- loadings %>%
  pivot_longer(cols = c(PC1, PC2), names_to = "PC", values_to = "loading")
  
ggplot(loadings_long, aes(x = PC, y = loading, fill = metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~metric, scales = "free_y") +
  labs(
    title = "PCA Loadings of Driver Performance Metrics",
    x = "Principal Component",
    y = "Loading (Contribution)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

  
  
  # Use absolute PC1 loadings as weights
weights <- abs(pca_model$rotation[,1])
weights <- weights / sum(weights)  # normalize to sum 1

driver_summ$goat_weighted <- driver_summ$s_win_rate * weights["s_win_rate"] +
  driver_summ$s_points_per_race_norm * weights["s_points_per_race_norm"] +
  driver_summ$s_podium_rate * weights["s_podium_rate"]

# Rank drivers by weighted GOAT
driver_summ <- driver_summ %>%
  arrange(desc(goat_weighted)) %>%
  mutate(goat_weighted_rank = row_number())


#FINDING ALL DRIVERS WHO HAVE WON A CHAMPIONSHIP


# final race id per year (highest round)
final_races <- races_0 %>%
  group_by(year) %>%
  summarise(final_race_id = raceId[which.max(round)], .groups = "drop")

# get the driver standing entries that correspond to the final race of each year
champions_by_year <- driver_standings_0 %>%
  inner_join(final_races, by = c("raceId" = "final_race_id")) %>%
  # position may be string "1" or numeric positionOrder; accept either
  filter(position == "1") %>%
  left_join(drivers_0 %>% 
              mutate(driver_name = paste(forename, surname)) %>%
              select(driverId, driver_name),
            by = "driverId") %>%
  select(year, driverId, driver_name)
  

  # aggregate to get counts + list of years
champions <- champions_by_year %>%
  group_by(driver_name) %>%
  summarise(
    championships_won = n(),
    years = paste(sort(unique(year)), collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(desc(championships_won))

print(champions)

ggplot(champions, aes(x = reorder(driver_name, championships_won), 
                      y = championships_won, 
                      fill = championships_won)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # makes it horizontal for readability
  labs(
    title = "F1 World Championships by Driver",
    x = "Driver",
    y = "Number of Championships",
    fill = "Titles"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  )


#  ========================= STATISTICS ===========================
    
 #   ================== WHERE CODE GETS BANISHED =====================
    
'''  
  library(ggplot2)
  
  ggplot(driver_summary, aes(x = driver_name, y = races_won, fill = factor(year))) + geom_bar(stat = "identity") + labs( title = "Number of Races Won by Driver per Year", x = "Driver", y = "Races Won", fill = "Year" ) + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  this is still before normalising the points
  
  goat_metrics <- goat_metrics %>% mutate(across(c(win_rate, podium_rate, points_per_race, finish_rate), scales::rescale)) %>% mutate(goat_score = (win_rate + podium_rate + points_per_race + finish_rate) / 4) %>% arrange(desc(goat_score)) head(goat_metrics, 10)
  
  {r, echo = TRUE}
  
  driver_summary_filt <- driver_summ %>% filter(races_won > 1)
  
  ggplot(driver_summary_filt, aes(x = driver_name, y = races_won, fill = factor(year))) + geom_bar(stat = "identity") + labs( title = "Number of Races Won by Driver per Year", x = "Driver", y = "Races Won", fill = "Year" ) + theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  '''