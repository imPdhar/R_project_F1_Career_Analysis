library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(scales)
library(ggfortify)
library(Matrix)
library(lme4)
library(forecast)
library(lme4)
library(lmerTest)
library(ggplot2)

#Loading in the csv files, using '_0' to indicate an untouched original file
setwd("C:/Users/pruth/OneDrive/Desktop/UL/6071/Project")
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
sprint_results_0 <- read.csv("sprint_results.csv")

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


#===============================================================================
#checking for null values
lapply(datasets, function(df) {
  sapply(df, function(x)
    sum(is.na(x) | x == "" | x == "\\N" | x == "###" | x == "null")
  )
})

#How many unique values
length(unique(circuits_0$circuitId))  #77
length(unique(drivers_0$driverId))  #861
length(unique(constructors_0$constructorId))  #212

#some summary stats
summary(results_0$grid)
summary(results_0$positionOrder)


#===============================================================================

#CLEANING

#Results========================================================================

# Sprint weekends
common_race_ids <- intersect(results_0$raceId, sprint_results_0$raceId)
length(common_race_ids)  # how many races are in both

results_ns <- results_0 %>%
  left_join(
    races_0 %>% select(raceId, year),
    by = "raceId"
  )

#Confirm removal
length(unique(sprint_results_0$raceId))  #18
length(unique(results_0$raceId))  #1125
length(unique(results_ns$raceId)) #1125

#taking minimal result info
results <- results_ns %>%
  select(raceId, driverId, constructorId, positionOrder, points) %>% 
  filter(!is.na(positionOrder))  # remove rows where position is NA

#=== 
#Clean types and positionOrder & points
results_clean <- results_ns %>%      
  mutate(
    positionOrder = as.numeric(positionOrder),
    points = as.numeric(points),
  )

#Remove duplicate driver-race rows if they exist - looking for double entries
results_nod <- results_ns %>%
  mutate(
    positionOrder = as.numeric(positionOrder),
    points = as.numeric(points),
    laps = as.numeric(laps)
  ) %>%
  filter(!is.na(driverId), !is.na(raceId)) %>%
  arrange(raceId, driverId, desc(laps), desc(points)) %>%
  distinct(raceId, driverId, .keep_all = TRUE)

#checking for missed duplicates======
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
#=====================================

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
      select(raceId, year) %>%
      rename(race_year = year),   # rename to avoid conflict
    by = "raceId"
  ) %>%
  mutate(race_year = as.integer(race_year))

results_f %>%
  filter(!is.na(driver_name)) %>%
  select(driver_name) %>%
  head()

#scaling points to match current ===============================================
results_scaled <- results_f %>%
  mutate(
    era = case_when(
      year < 1961 ~ "1950-1960 (8 pts win)",
      year < 1991 ~ "1961-1990 (9 pts win)",
      year < 2003 ~ "1991-2002 (10 pts win)",
      year < 2010 ~ "2003-2009 (10-8-6 pts system)",
      TRUE ~ "2010+ (25 pts win)"
    ),
    max_points_era = case_when(
      year < 1961 ~ 8,
      year < 1991 ~ 9,
      year < 2003 ~ 10,
      year < 2010 ~ 10,
      TRUE ~ 25
    ),
    scaled_points = (points / max_points_era) * 25
  )

era_points <- tibble(
  era = c(
    "1950-1960 (8 pts win)",
    "1961-1990 (9 pts win)",
    "1991-2002 (10 pts win)",
    "2003-2009 (10-8-6 pts system)",
    "2010+ (25 pts win)"
  ),
  max_points_for_win = c(8, 9, 10, 10, 25)
)

results_scaled <- results_scaled %>%
  left_join(era_points, by = "era")

results_scaled %>%
  select(driver_name, year, era, points, scaled_points, max_points_for_win) %>%
  head(10)

era_summary <- results_scaled %>%
  group_by(era) %>%
  summarise(max_points = max(points, na.rm = TRUE),
            .groups = "drop")
#===============================================================================

#drivers results w/o points ====================================================
drivers_clean <- drivers_0 %>%
  mutate(driver_name = paste(forename, surname)) %>%
  select(driverId, driver_name)

race_winners <- results_ns %>%
  filter(positionOrder == 1) %>%
  left_join(races_0 %>% select(raceId, year), by = "raceId") %>%
  left_join(drivers_clean, by = "driverId")


driver_career %>%
  filter(driver_name == "Lewis Hamilton")
#===============================================================================

#driver career info - scaled ===================================================
driver_career <- results_scaled %>%
  group_by(driver_name) %>%
  summarise(
    total_races = n_distinct(raceId),
    total_wins = sum(positionOrder == 1, na.rm = TRUE),
    total_podiums = sum(positionOrder <= 3, na.rm = TRUE),
    total_points = sum(points, na.rm = TRUE),
    total_scaled_points = sum(scaled_points, na.rm = TRUE),
    avg_points_max = mean(max_points_for_win, na.rm = TRUE),
    win_rate = total_wins / total_races,
    podium_rate = total_podiums / total_races,
    points_per_race = total_points / total_races,
    points_per_race_norm = points_per_race / avg_points_max,
    scaled_points_per_race = total_scaled_points / total_races,
    .groups = "drop"
  )

career_atl1 <- results_scaled %>%    #drivers with at least 1 win ==============
group_by(driver_name) %>%
  filter(sum(positionOrder == 1, na.rm = TRUE)>0) %>%
  summarise(
    total_races = n_distinct(raceId),
    total_wins = sum(positionOrder == 1, na.rm = TRUE),
    total_podiums = sum(positionOrder <= 3, na.rm = TRUE),
    total_points = sum(points, na.rm = TRUE),
    total_scaled_points = sum(scaled_points, na.rm = TRUE),
    avg_points_max = mean(max_points_for_win, na.rm = TRUE),
    win_rate = total_wins / total_races,
    podium_rate = total_podiums / total_races,
    points_per_race = total_points / total_races,
    points_per_race_norm = points_per_race / avg_points_max,
    scaled_points_per_race = total_scaled_points / total_races,
    .groups = "drop"
  )

driver_yearly <- results_scaled %>%
  group_by(driver_name, year) %>%
  summarise(
    races_in_year = n_distinct(raceId),
    wins_in_year = sum(positionOrder == 1, na.rm = TRUE),
    podiums_in_year = sum(positionOrder <= 3, na.rm = TRUE),
    total_scaled_points = sum(scaled_points, ra.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(driver_name, year)

results_ns <- results_ns %>%
  dplyr::select(-dplyr::any_of(c("year", "year.x", "year.y"))) %>%
  dplyr::left_join(dplyr::select(races_0, raceId, year), by = "raceId") %>%
  dplyr::mutate(year = as.integer(.data$year))

# Now this works
results_ns %>%
  dplyr::filter(.data$year == 2023) %>%
  dplyr::summarise(total_races = dplyr::n_distinct(.data$raceId))
#===============================================================================

#===============================================================================

#reg changes ===================================================================
eras <- data.frame(
  era = c("1.5L Era", "Turbo Era", "Safety/Aero Era", "V10/V8 Era", 
          "Aero Overhaul", "Hybrid Era", "Ground Effect Era"),
  start = c(1961, 1984, 1994, 2005, 2009, 2014, 2022),
  end   = c(1983, 1993, 2004, 2008, 2013, 2021, 2025)
)

eras <- eras %>%
  mutate(start = start - 0.5,
         end   = end + 0.5)
#===============================================================================



#===============================================================================

#ENGINEERING

#drivers listed by driverId and full name
drivers <- drivers_0 %>%
  select(driverId, forename, surname) %>%  
  mutate(driver_name = paste(forename, surname)) %>%
  select(driverId, driver_name)

#joining drivers, races, and results
drivers_results <- results %>%
  left_join(drivers, by = "driverId") %>%
  left_join(races_0 %>% select(raceId, year), by = "raceId")

#creating a summary of the above, ordered by year and alphabetically by driver
dr_summary <- drivers_results %>%
  group_by(driver_name, year) %>%
  summarise(
    total_points = sum(points, na.rm = TRUE),
    races_won = sum(positionOrder == 1, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(year))  

#create a csv file to store this data
write_csv(dr_summary, "driver_resuslts_summary.csv")

head(dr_summary)

#driver_name, year, races_won, total_races
drs_filt <- drivers_results %>%     
  filter(!is.na(driver_name)) %>%
  group_by(driver_name, year) %>%
  summarise(
    races_won = sum(positionOrder == 1, na.rm = TRUE),
    total_races = n_distinct(raceId),
    .groups = "drop"
  ) %>%
  arrange(desc(races_won))

# Find top 10 drivers of all time by total races won
top10_drivers <- drs_filt %>%
  group_by(driver_name) %>%
  summarise(total_wins = sum(races_won, na.rm = TRUE)) %>%
  arrange(desc(total_wins)) %>%
  slice_head(n = 10) %>%
  pull(driver_name)

# Filter data for only those top 10
drs_top10 <- drs_filt %>%
  filter(driver_name %in% top10_drivers) %>%
  mutate(driver_name = factor(driver_name,
                              levels = top10_drivers))

ggplot(drs_top10, aes(x = driver_name, y = races_won, fill = factor(year))) +
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

drs_filt$year <- factor(drs_filt$year,
                        levels = rev(sort(unique(drs_filt$year))))

#at least 1 race win
drs_atl1 <- dr_summary %>%
  filter(races_won > 0)




#===============================================================================



#===============================================================================

#EDA beginning with plots to understand data shape

#races and circuits ============================================================
races_0 %>%       #csv doesn't include sprints
  count(year) %>%
  ggplot(aes(x = year, y = n)) +
  theme_minimal()  +
  geom_bar(stat = "identity", fill = "firebrick3") +
  labs(title = "Number of Races per Season (1950-2024)")

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
#===============================================================================

#drivers by total wins (ns) ====================================================
most_wins_driver <- results %>%
  filter(positionOrder == 1) %>%
  left_join(drivers_0, by = "driverId") %>%
  mutate(driver_name = paste(forename, surname)) %>%
  count(driver_name, sort = TRUE) %>%
  slice(1:10)

ggplot(most_wins_driver, aes(x = reorder(driver_name, n), y = n)) +
  geom_col(fill = "firebrick3") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top 10 Drivers by Total Wins (1950-2024)",
    x = "Driver",
    y = "Wins"
  )
#===============================================================================

#teams by total wins (ns) ======================================================
top_constructor_wins <- results %>%
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
#===============================================================================

# ages by season ===============================================================
#first race of the season
season_start <- races_0 %>%
  group_by(year) %>%
  summarise(
    first_race_date = min(as.Date(date)),
    .groups = 'drop'
  )

age_season <- results_ns %>%
  dplyr::left_join(dplyr::select(drivers_0, driverId, dob), by = "driverId") %>%
  dplyr::left_join(dplyr::select(races_0, raceId, year, date), by = "raceId") %>%
  # Robustly create a single 'year' (and 'date') from whatever exists
  dplyr::mutate(
    year = dplyr::coalesce(!!!dplyr::select(., dplyr::any_of(c("year", "year.x", "year.y", "year.r")))) |> as.integer(),
    date = dplyr::coalesce(!!!dplyr::select(., dplyr::any_of(c("date", "date.x", "date.y", "date.r"))))
  ) %>%
  # Now this succeeds because 'year' exists in x
  dplyr::left_join(season_start, by = "year") %>%
  dplyr::mutate(
    dob = as.Date(.data$dob),
    first_race_date = as.Date(.data$first_race_date),
    age = as.numeric(difftime(.data$first_race_date, .data$dob, units = "days")) / 365.25
  )


avg_age <- age_season %>%
  group_by(year) %>%
  summarise(
    avg_driver_age = mean(age, na.rm = TRUE),
    n_drivers = n_distinct(driverId),
    .groups = 'drop'
  )

ggplot(avg_age, aes(x = year, y = avg_driver_age)) +
  geom_line(color = "firebrick3", linewidth = 1.2) +
  geom_point(color = "black", size = 1.8) +
  scale_y_continuous(
    breaks = seq(26, 40, by = 2),     
    limits = c(26, 40),               
    minor_breaks = seq(26, 40, by = 0.5)  
  ) +
  labs(
    title = "Average Driver Age per F1 Season (1950–2024)",
    x = "Season",
    y = "Average Age (years)"
  ) +
  theme_minimal(base_size = 13) 
#===============================================================================

#Chaos index - unique race winner ===============================================
chaos <- race_winners %>%
  select(-dplyr::any_of(c("year", "year.x", "year.y", "year.r"))) %>%
  left_join(dplyr::select(races_0, raceId, year), by = "raceId") %>% 
  group_by(year) %>%
  summarise(unique_winners = n_distinct(driver_name), .groups = "drop") %>%
  mutate(decade = floor(year / 10) * 10)

ggplot() +
  geom_rect(
    data = eras, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = era),
    alpha = 0.25, color = NA
  ) +
  geom_line(data = chaos, aes(x = year, y = unique_winners), color = "black"
  ) +
  geom_text(data = chaos, aes(x = year, y = unique_winners + 0.2, 
                              label = unique_winners),
            size = 2,
            color = "black"
  ) +
  scale_fill_brewer(palette = "Set3", name = "Regulation Era") +
  theme_minimal(base_size = 13) +
  labs(
    title = "F1 Chaos Index — Unique Race Winners per Season",
    subtitle = "Shaded backgrounds show regulation eras; bars show competitiveness drops",
    x = "Year",
    y = "Unique Winners"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )
#===============================================================================



#WDC Champions =================================================================
final_races <- races_0 %>%
  group_by(year) %>%
  summarise(final_race_id = raceId[which.max(round)], .groups = "drop")

champions_by_year <- driver_standings_0 %>%
  inner_join(final_races, by = c("raceId" = "final_race_id")) %>%
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
#===============================================================================









#===============================================================================
#MIXED EFFECTS MODEL ===========================================================
#response variable; total_points_norm 
#fixed effects; 1
#random effects; (1|driver_name), (1|constructorId)  == allows each driver/team to have their own deviation from the overall mean

driver_season <- results_scaled %>%
  group_by(driver_name, race_year, constructorId) %>%
  summarise(
    total_points_norm = sum(scaled_points, na.rm = TRUE),
    races_entered = n_distinct(raceId),
    wins = sum(positionOrder == 1, na.rm = TRUE),
    podiums = sum(positionOrder <= 3, na.rm = TRUE),
    .groups = "drop"
  )

# Mixed-effects model: driver & team random effects
driver_team_model <- lmer(total_points_norm ~ 1 + (1 | driver_name) + (1 | constructorId), data = driver_season)

summary(driver_team_model)

#=========================================

#take out random effects
driver_effects <- ranef(driver_team_model)$driver_name %>%       #ranef() extracts random effects estimates
  rownames_to_column("driver_name") %>%
  rename(driver_skill = `(Intercept)`) %>%          #driver_skill = how much better/worse than average a driver performs, controlling for team strength
  arrange(desc(driver_skill))

constructor_effects <- ranef(driver_team_model)$constructorId %>%
  rownames_to_column("constructorId") %>%
  mutate(constructorId = as.integer(constructorId)) %>%   # convert to integer to match constructors_0
  rename(team_strength = `(Intercept)`) %>%         #team_strength = how much better/worse than average a team performs, controlling for driver ability
  left_join(
    constructors_0 %>% mutate(constructorId = as.integer(constructorId)) %>% select(constructorId, name),
    by = "constructorId"
  ) %>%
  arrange(desc(team_strength))

#==========================================

#Visualising drivers
top_drivers <- driver_effects %>% slice_max(driver_skill, n = 5)
ggplot(top_drivers, aes(x = reorder(driver_name, driver_skill), y = driver_skill)) +
  geom_col(fill = "firebrick3") +
  coord_flip() +
  labs(title = "Top 10 Drivers by Estimated Skill (Random Effect)",
       x = "Driver",
       y = "Estimated points more than the average driver") +
  theme_minimal()

#Visualising teams
top_teams <- constructor_effects %>% slice_max(team_strength, n = 5)
ggplot(top_teams, aes(x = reorder(name, team_strength), y = team_strength)) +
  geom_col(fill = "blue4") +
  coord_flip() +
  labs(title = "Top 10 Teams by Estimated Strength (Random Effect)",
       x = "Constructor",
       y = "Estimated driver points improvement with team y ") +
  theme_minimal()

#===============================================================================
#===============================================================================
#Constructor Names
constructors_0 <- constructors_0 %>%
  mutate(name = str_trim(tolower(name)))

#Identify Ferrari 
ferrari_id <- constructors_0 %>%
  filter(name == "ferrari") %>%
  pull(constructorId)

#Aggregate Season-Level Performance
ferrari_season <- driver_season %>%
  filter(constructorId %in% ferrari_id) %>%
  group_by(year) %>%
  summarise(
    total_points_norm = sum(total_points_norm, na.rm = TRUE),
    wins = sum(wins, na.rm = TRUE),
    races_entered = sum(races_entered, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(year > 1990) %>%
  arrange(year)

ferrari_season <- driver_season %>%
  filter(constructorId %in% ferrari_id) %>%
  group_by(year) %>%
  summarise(
    total_points_norm = sum(total_points_norm, na.rm = TRUE),
    wins = sum(wins, na.rm = TRUE),
    dnfs = sum(dnfs, na.rm = TRUE),
    races_entered = sum(races_entered, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(dnf_rate = dnfs / races_entered) %>%
  filter(year > 1990) %>%
  arrange(year)

#Add Total Races
season_lengths <- races_0 %>%
  group_by(year) %>%
  summarise(races_in_season = n(), .groups = "drop")

ferrari_season <- ferrari_season %>%
  left_join(season_lengths, by = "year") %>%
  mutate(points_per_race = total_points_norm / races_in_season)

#Build ARIMA Model on Points per Race
ferrari_ts <- ts(ferrari_season$points_per_race, start = min(ferrari_season$year))
ferrari_arima <- auto.arima(ferrari_ts)

checkresiduals(ferrari_arima)

#Forecast Next Season
ferrari_forecast <- forecast(ferrari_arima, h = 1)
print(ferrari_forecast)

#Visualisation of line plot
#Define regulation eras 
reg_eras <- data.frame(
  era = c("Safety/Aero Reforms", "V10/V8 Era", "Hybrid Era", "Ground Effect Era"),
  start = c(1994, 2000, 2014, 2022),
  end   = c(1999, 2013, 2021, 2025),
  fill  = c("#FFD580", "#F5A09D", "#A5C8E1", "#A7E3A3")  # richer tones
)

min_year <- min(ferrari_season$year)
max_year <- max(ferrari_forecast_df$year)

reg_eras$start[1] <- min_year
reg_eras$end[nrow(reg_eras)] <- max_year

ggplot() +
  # Regulation backgrounds
  geom_rect(
    data = reg_eras,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = era),
    alpha = 0.5  # richer opacity
  ) +
  # Ferrari performance
  geom_line(data = ferrari_season, aes(x = year, y = points_per_race),
            color = "firebrick", size = 1.2) +
  geom_point(data = ferrari_season, aes(x = year, y = points_per_race),
             color = "firebrick", size = 2) +
  # Forecast line & ribbon
  geom_line(data = ferrari_forecast_df,
            aes(x = year, y = points_per_race),
            color = "darkred", linetype = "dotdash", size = 1) +
  geom_ribbon(data = ferrari_forecast_df,
              aes(x = year, ymin = lower, ymax = upper),
              fill = "firebrick", alpha = 0.25) +
  # Scales, labels, theme
  scale_fill_manual(values = reg_eras$fill, name = "Regulation Era") +
  labs(
    title = "Ferrari Performance — Points per Race by Season",
    subtitle = "Shaded backgrounds indicate major regulation eras",
    x = "Year",
    y = "Points per Race"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )


# ── helpers
split_into_stints <- function(years_vec) {
  yrs <- sort(unique(years_vec))
  if (length(yrs) == 0) return(tibble(year = integer(), stint_id = integer()))
  tibble(year = yrs, stint_id = cumsum(c(1, diff(yrs) > 1)))
}
safe_auto_arima <- function(x) {
  if (length(x) < 2) return(NULL)
  tryCatch(auto.arima(x, stepwise = TRUE, approximation = FALSE), error = function(e) NULL)
}

# ── 1) yearly scaled points per driver (active years only, 1990–2024)
driver_yearly_scaled <- results_scaled %>%
  filter(year >= 1990, year <= 2024) %>%
  group_by(driverId, driver_name, year) %>%
  summarise(total_scaled_points = sum(scaled_points, na.rm = TRUE), .groups = "drop")

# ── 2) robust WDC list from final race standings
final_races <- races_0 %>%
  group_by(year) %>%
  summarise(final_race_id = raceId[which.max(round)], .groups = "drop")

wdc_by_year <- driver_standings_0 %>%
  semi_join(final_races, by = c("raceId" = "final_race_id")) %>%
  mutate(position = suppressWarnings(as.integer(position))) %>%
  filter(position == 1) %>%
  left_join(final_races, by = c("raceId" = "final_race_id")) %>%  # bring year in
  left_join(
    drivers_0 %>% mutate(driver_name = paste(forename, surname)) %>% select(driverId, driver_name),
    by = "driverId"
  ) %>%
  select(year, driverId, driver_name)

wdc_unique <- wdc_by_year %>% distinct(driverId, driver_name)

# ── 3) retired WDCs (<2024), exclude Hamilton + (later) Prost & Piquet (because their stints are too small and it looks weird)
driver_last_year <- results_scaled %>%
  distinct(driverId, driver_name, year) %>%
  group_by(driverId, driver_name) %>%
  summarise(last_active_year = max(year, na.rm = TRUE), .groups = "drop")

wdc_retired <- wdc_unique %>%
  left_join(driver_last_year, by = c("driverId","driver_name")) %>%
  filter(!is.na(last_active_year), last_active_year < 2024) %>%       # STRICTLY retired before 2024
  semi_join(driver_yearly_scaled %>% distinct(driverId), by = "driverId") %>%  # has data in window
  filter(driver_name != "Lewis Hamilton")

# sanity checks
stopifnot(!any(wdc_retired$driver_name %in% c("Max Verstappen","Fernando Alonso")))
stopifnot(all(wdc_retired$driverId %in% wdc_unique$driverId))

# ── 4) label names as "F. Surname"; exclude Prost & Piquet outright
name_map <- drivers_0 %>%
  mutate(driver_name = paste(forename, surname),
         label_name  = paste0(substr(forename, 1, 1), ". ", surname)) %>%
  select(driverId, driver_name, label_name)

# exclude_drivers <- c("Alain Prost", "Nelson Piquet")
wdc_retired_lbl <- wdc_retired %>%
  filter(!driver_name %in% exclude_drivers) %>%
  left_join(name_map, by = c("driverId","driver_name"))

# ── 5) split active years into stints and remove stints with <= 2 seasons
driver_stints <- results_scaled %>%
  filter(driverId %in% wdc_retired_lbl$driverId, year >= 1990, year <= 2024) %>%
  distinct(driverId, driver_name, year) %>%
  arrange(driverId, year) %>%
  group_by(driverId, driver_name) %>%
  group_modify(~ split_into_stints(.x$year)) %>%
  ungroup() %>%
  group_by(driverId, driver_name, stint_id) %>%
  summarise(
    start_year = min(year), end_year = max(year),
    years = list(sort(year)),
    n_years = dplyr::n(),
    .groups = "drop"
  ) %>%
  filter(n_years > 2) %>%          # <-- dropping short stints
  arrange(driver_name, start_year) %>%
  group_by(driverId, driver_name) %>%
  mutate(stint_num = row_number()) %>%
  ungroup() %>%
  left_join(name_map, by = c("driverId","driver_name")) %>%
  mutate(stint_label = paste0(label_name, " — stint ", stint_num, " (", start_year, "–", end_year, ")"))

driver_stints <- driver_stints %>%
  left_join(driver_last_year, by = c("driverId","driver_name")) %>%
  filter(end_year == last_active_year) %>%
  select(-last_active_year)

stopifnot(nrow(driver_stints) > 0)


# Simple line plot for Scaled points per season vs Year for retired WDCs
wdc_retired_yearly <- driver_yearly_scaled %>%
  filter(driverId %in% wdc_retired_lbl$driverId) %>%
  left_join(name_map, by = c("driverId", "driver_name")) %>%
  arrange(label_name, year)

p <- ggplot(wdc_retired_yearly, aes(x = year, y = total_scaled_points, group = label_name)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ label_name, scales = "free", ncol = 3) +
  scale_x_continuous(breaks = scales::pretty_breaks(4)) +
  labs(
    title = "Career Trajectories — Retired World Champions (1990–2024, excl. Hamilton)",
    subtitle = "Yearly scaled points",
    x = "Year", y = "Scaled points (per season)"
  ) +
  theme_minimal(base_size = 16) +
  theme(strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank())

print(p)

n_facets <- dplyr::n_distinct(driver_stints$stint_label)
ncol <- 3; nrow <- ceiling(n_facets / ncol)
W <- 4 * ncol; H <- 3.6 * nrow

ggplot2::ggsave("wdc_stints_forecast_filtered.pdf", plot = p, width = W, height = H, device = cairo_pdf)

#===================Arima similar to lewis=============
wdc_roster <- wdc_retired_lbl %>%
  distinct(driverId, driver_name, label_name) %>%
  arrange(label_name)

wdc_roster <- wdc_roster %>% bind_rows(drivers_0 %>% mutate(driver_name=paste(forename,surname), label_name=paste0(substr(forename,1,1),". ",surname)) %>% filter(driver_name=="Valtteri Bottas") %>% select(driverId,driver_name,label_name)) %>% distinct(driverId,.keep_all=TRUE) %>% arrange(label_name)


h <- 3

#pdf("retired_wdcs_all_drivers.pdf", width = 12, height = 9)
for (i in seq_len(nrow(wdc_roster))) {
  did <- wdc_roster$driverId[i]
  print(did)
  lbl <- wdc_roster$label_name[i]
  print(lbl)
  
  drv_season <- driver_yearly_scaled %>%
    dplyr::filter(driverId == did) %>%
    dplyr::arrange(year)
  
  if (nrow(drv_season) < 2) {
    message(lbl, ": fewer than 2 seasons available — skipping ARIMA.")
    next
  }
  
  start_year <- min(drv_season$year, na.rm = TRUE)
  end_year   <- max(drv_season$year, na.rm = TRUE)
  
  # time series of scaled points per season (frequency = 1 year)
  drv_ts <- ts(drv_season$total_scaled_points, start = start_year, frequency = 1)
  
  # Plot the raw TS
  print(
    autoplot(drv_ts) +
      ggtitle(paste0(lbl, " — Scaled Points per Season")) +
      xlab("Year") + ylab("Scaled points per season") +
      theme_minimal()
  )
  
  # Optional: summary of the TS
  print(summary(drv_ts))
  
  # Fit ARIMA and forecast h = 3 seasons
  drv_arima <- auto.arima(drv_ts, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
  print(summary(drv_arima))
  checkresiduals(drv_arima)
  
  drv_fc <- forecast(drv_arima, h = h, level = c(80, 95))
  
  # Forecast plot
  print(
    autoplot(drv_fc) +
      labs(
        title = paste0(lbl, " — ARIMA Forecast (Scaled Points per Season)"),
        x = "Year", y = "Scaled points per season"
      ) +
      theme_minimal()
  )
}
dev.off()

