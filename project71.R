
library(tidyverse)
library(dplyr)
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
library(performance)



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
    races_0 %>% 
      dplyr::select(raceId, year),
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
  mutate(
    race_year = as.integer(race_year),
    dnf = if_else(statusId != 1, 1, 0)   
    )


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

results_scaled <- results_f %>%
  mutate(
    era = case_when(
      race_year < 1961 ~ "1950-1960 (8 pts win)",
      race_year < 1991 ~ "1961-1990 (9 pts win)",
      race_year < 2003 ~ "1991-2002 (10 pts win)",
      race_year < 2010 ~ "2003-2009 (10-8-6 pts system)",
      TRUE ~ "2010+ (25 pts win)"
    ),
    max_points_era = case_when(
      race_year < 1961 ~ 8,
      race_year < 1991 ~ 9,
      race_year < 2003 ~ 10,
      race_year < 2010 ~ 10,
      TRUE ~ 25
    ),
    scaled_points = (points / max_points_era) * 25
  ) %>%
  select(driver_name, race_year, constructorId, raceId, positionOrder,
         points, scaled_points, dnf, everything())


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
  filter(positionOrder==1) %>%
  left_join(races_0 %>% select(raceId, year), by = "raceId") %>%
  left_join(drivers_clean, by = "driverId")

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
driver_career %>%
  filter(driver_name == "Lewis Hamilton")

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
  left_join(races_0 %>% select(raceId, year), by = "raceId")

results_ns <- results_ns %>%
  mutate(
    year = coalesce(year.x, year.y)  
  ) %>%
  select(-year.x, -year.y)                 

# Check number of GP races in 2023 == should be 22 
results_ns %>%
  filter(year == 2023) %>%
  summarise(total_races = n_distinct(raceId))
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
# First race of each season
season_start <- races_0 %>%
  group_by(year) %>%
  summarise(
    first_race_date = min(as.Date(date)),
    .groups = 'drop'
  )

age_season <- results_ns %>%
  left_join(drivers_0 %>% select(driverId, dob), by = "driverId") %>%
  # Rename the year from races_0 to avoid overwriting
  left_join(races_0 %>% select(raceId, race_date = date, race_year = year), by = "raceId") %>%
  left_join(season_start, by = c("year" = "year")) %>%
  mutate(
    dob = as.Date(dob),
    first_race_date = as.Date(first_race_date),
    age = as.numeric(difftime(first_race_date, dob, units = "days")) / 365.25
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
race_winners <- race_winners %>%
  mutate(
    year = coalesce(year.x, year.y)  
  ) %>%
  select(-year.x, -year.y) 

chaos <- race_winners %>%
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




#              -----  UP TO THIS POINT RUNS SMOOTHLY  -----




#===============================================================================

#MIXED EFFECTS MODEL ===========================================================

model_data <- results_scaled %>%
  select(driver_name, constructorId, race_year, scaled_points) %>%
  filter(!is.na(scaled_points)) %>%
  mutate(
    driver_name = factor(driver_name),
    constructorId = factor(constructorId),
    year = factor(race_year)   # treat year as random effect
  )

library(dplyr)

model_data <- results_scaled %>%
  select(driver_name, constructorId, race_year, scaled_points) %>%
  filter(!is.na(scaled_points)) %>%
  # Count seasons per driver and keep only those with >=3
  group_by(driver_name) %>%
  mutate(seasons_competed = n_distinct(race_year)) %>%
  ungroup() %>%
  filter(seasons_competed >= 3) %>%
  mutate(
    driver_name = factor(driver_name),
    constructorId = factor(constructorId),
    year = factor(race_year)  # treat year as fixed effect
  ) %>%
  select(-seasons_competed)  # optional: remove helper column



#===========================================
#most standard basic approach
#response variable; scaled_points
#fixed effects; 1
#random effects; (1|driver_name), (1|constructorId)  == allows each driver/team to have their own deviation from the overall mean
mem_base <- lmer(scaled_points ~ 1 + (1 | driver_name) + (1 | constructorId),data = model_data)

summary(mem_base)
#residual variance is very higher 

driver_skill1 <- ranef(mem_base)$driver_name %>%
  rownames_to_column("driver_name") %>%
  rename(skill = `(Intercept)`)

team_strength1 <- ranef(mem_base)$constructorId %>%
  rownames_to_column("constructorId") %>%
  rename(strength = `(Intercept)`)

head(driver_skill1[order(-driver_skill1$skill), ])
head(team_strength1[order(-team_strength1$strength), ])

#Extract random effects ===================
driver_effects1 <- ranef(mem_base)$driver_name %>%       
  rownames_to_column("driver_name") %>%
  rename(driver_skill = `(Intercept)`) %>%
  arrange(desc(driver_skill1))

constructor_effects1 <- ranef(mem_base)$constructorId %>%
  rownames_to_column("constructorId") %>%
  mutate(constructorId = as.integer(constructorId)) %>%
  rename(team_strength = `(Intercept)`) %>%
  left_join(
    constructors_0 %>% 
      mutate(constructorId = as.integer(constructorId)) %>% 
      select(constructorId, name),
    by = "constructorId"
  ) %>%
  arrange(desc(team_strength1))

#===============================================================================

#How can this be improved? adding year slopes ==================================

mem_year <- lmer(scaled_points ~ factor(year) + (1 | driver_name) + (1 | constructorId), data = model_data)
summary(mem_year)
#variance is higher but other results are better

#=========================================
# checking if it improves the model
#without year
mem_base <- lmer(scaled_points ~ 1 + (1 | driver_name) + (1 | constructorId), data = model_data, REML = TRUE)

#year as fixed effect
mem_year <- lmer(scaled_points ~ factor(year) + (1 | driver_name) + (1 | constructorId), data = model_data, REML = TRUE)

# Compare marginal and conditional R^2
r2_base <- r2_nakagawa(mem_base)
r2_year <- r2_nakagawa(mem_year)
r2_summary <- tibble(
  model = c("mem_base", "mem_year"),
  marginal_R2 = c(r2_base$R2_marginal, r2_year$R2_marginal),
  conditional_R2 = c(r2_base$R2_conditional, r2_year$R2_conditional)
)
print(r2_summary)
#1.9% of the variance explained by adding year as a fixed effect explains
#19.1% of the variance in points is explained by driver and constructor random effects alone.
#23% of variance is explained when including year as a fixed effect in addition to random effects.

#Random effect variances
var_base <- as.data.frame(VarCorr(mem_base)) %>% select(grp, vcov, sdcor)
var_year <- as.data.frame(VarCorr(mem_year)) %>% select(grp, vcov, sdcor)

print(var_base)
print(var_year)

#===============



driver_skill2 <- ranef(mem_year)$driver_name %>%
  rownames_to_column("driver_name") %>%
  rename(skill = `(Intercept)`)

team_strength2 <- ranef(mem_year)$constructorId %>%
  rownames_to_column("constructorId") %>%
  rename(strength = `(Intercept)`)

head(driver_skill2[order(-driver_skill2$skill), ])
head(team_strength2[order(-team_strength2$strength), ])

#Extract random effects ===================
driver_effects2 <- ranef(mem_year)$driver_name %>%       
  rownames_to_column("driver_name") %>%
  rename(driver_skill2 = `(Intercept)`) %>%
  arrange(desc(driver_skill2))

constructor_effects2 <- ranef(mem_year)$constructorId %>%
  rownames_to_column("constructorId") %>%
  mutate(constructorId = as.integer(constructorId)) %>%
  rename(team_strength2 = `(Intercept)`) %>%
  left_join(
    constructors_0 %>% 
      mutate(constructorId = as.integer(constructorId)) %>% 
      select(constructorId, name),
    by = "constructorId"
  ) %>%
  arrange(desc(team_strength2))


#Visualising strongest drivers, teams ==========================================

top_drivers <- driver_effects2 %>% slice_max(driver_skill2, n = 10)  # Top 10
ggplot(top_drivers, aes(x = reorder(driver_name, driver_skill2), y = driver_skill2)) +
  geom_col(fill = "firebrick3") +
  coord_flip() +
  labs(
    title = "Top 10 Drivers by Estimated Skill (Random Effect)",
    x = "Driver",
    y = "Estimated points above average (controlling for team)"
  ) +
  theme_minimal()

top_teams <- constructor_effects2 %>% slice_max(team_strength2, n = 10)  # Top 10
ggplot(top_teams, aes(x = reorder(name, team_strength2), y = team_strength2)) +
  geom_col(fill = "blue4") +
  coord_flip() +
  labs(
    title = "Top 10 Teams by Estimated Strength (Random Effect)",
    x = "Constructor",
    y = "Estimated points improvement for drivers"
  ) +
  theme_minimal()

#===============================================================================




#===============================================================================

driver_season <- results_scaled %>%
  group_by(driver_name, race_year, constructorId) %>%
  summarise(
    total_scaled_points = sum(scaled_points, na.rm = TRUE),
    races_entered = n_distinct(raceId),
    wins = sum(positionOrder == 1, na.rm = TRUE),
    podiums = sum(positionOrder <= 3, na.rm = TRUE),
    dnfs = sum(dnf, na.rm = TRUE),     
    .groups = "drop"
  )


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
  rename(year = race_year) %>%
  group_by(year) %>%
  summarise(
    total_scaled_points = sum(total_scaled_points, na.rm = TRUE),
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
  mutate(points_per_race = total_scaled_points / races_in_season)

#Build ARIMA Model on Points per Race
ferrari_ts <- ts(ferrari_season$points_per_race, start = min(ferrari_season$year), frequency = 1)
ferrari_arima <- auto.arima(ferrari_ts, seasonal = FALSE, stepwise = FALSE, approximation =  FALSE)
summary(ferrari_arima)
checkresiduals(ferrari_arima)


#Forecast Next Season
ferrari_forecast <- forecast(ferrari_arima, h = 3)
print(ferrari_forecast)


#Create forecast dataframe 
ferrari_forecast_df <- data.frame(
  year = max(ferrari_season$year) + seq_len(length(ferrari_forecast$mean)),
  points_per_race = as.numeric(ferrari_forecast$mean),
  lower = as.numeric(ferrari_forecast$lower[, 2]),
  upper = as.numeric(ferrari_forecast$upper[, 2])
)

#Visualisation of line plot
#Define regulation eras 
reg_eras <- data.frame(
  era = c("Safety/Aero Reforms", "V10/V8 Era", "Hybrid Era", "Ground Effect Era"),
  start = c(1994, 2000, 2014, 2022),
  end   = c(2000, 2014, 2022, 2025),
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
            color = "black", size = 0.8) +
  geom_point(data = ferrari_season, aes(x = year, y = points_per_race),
             color = "black", size = 2) +
  # Forecast line & ribbon
  geom_line(data = ferrari_forecast_df,
            aes(x = year, y = points_per_race),
            color = "red", linetype = "dotdash", size = 1) +
  geom_ribbon(data = ferrari_forecast_df,
              aes(x = year, ymin = lower, ymax = upper),
              fill = "white", alpha = 0.5) +
  # Scales, labels, theme
  scale_fill_manual(values = reg_eras$fill, name = "Regulation Era") +
  labs(
    title = "Ferrari Performance - Points per Race (by season)",
    subtitle = "Shaded backgrounds indicate major regulation eras",
    x = "Year",
    y = "Points per Race"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face="bold")
  )

#===============================================================================










#===============================================================================

#LEWIS HAMILTON

ham_season <- results_scaled %>%
  filter(driver_name == "Lewis Hamilton") %>%
  group_by(race_year) %>%
  summarise(
    total_scaled_points = sum(scaled_points, na.rm = TRUE),
    races_in_season = n_distinct(raceId),
    points_per_race = if_else(races_in_season > 0, total_scaled_points / races_in_season, NA_real_),
    .groups = "drop"
  ) %>%
  arrange(race_year)

year_seq <- seq(min(ham_season$race_year, na.rm=TRUE), max(ham_season$race_year, na.rm=TRUE))
ham_season <- tibble(race_year = year_seq) %>%
  left_join(ham_season, by = "race_year") %>%
  mutate(
    total_scaled_points = replace_na(total_scaled_points, 0),
    races_in_season = replace_na(races_in_season, 0),
    points_per_race = if_else(races_in_season == 0, NA_real_, points_per_race)
  )

#PLOTTING HISTORICAL DATA
#plot with LOESS smoothing
ggplot(ham_season, aes(x = race_year)) +
  geom_col(aes(y = points_per_race), fill = "grey", alpha = 0.6) +
  geom_smooth(aes(y = points_per_race), method = "loess", span = 0.35, se = TRUE, color = "black") +
  labs(title = "Lewis Hamilton - points per race by season (and LOESS trend)",
       x = "Year", y = "Points per race") +
  theme_minimal()



#==== GAM ===========  !!! not relevant for presentation or updated ============
# GAM smoother (can capture more complex curvature)
gam_mod <- mgcv::gam(points_per_race ~ s(race_year, k = 10), data = ham_season)
ham_season$gam_fit <- predict(gam_mod, newdata = ham_season)

ggplot(ham_season, aes(x = race_year)) +
  geom_col(aes(y = points_per_race), fill = "steelblue", alpha = 0.6) +
  geom_line(aes(y = gam_fit), color = "darkred", size = 1.1) +
  labs(title = "Lewis Hamilton - GAM smooth of Points per race by season",
       x = "Year", y = "Points per race") +
  theme_minimal()

#plot together
ggplot(ham_season, aes(x = race_year)) +
  geom_col(aes(y = total_scaled_points), fill = "darkgrey", alpha = 0.5) +
  # LOESS smoother
  geom_smooth(aes(y = total_scaled_points), method = "loess", span = 0.35, se = TRUE, color = "blue", linetype = "dashed") +
  # GAM smoother
  geom_line(aes(y = gam_fit), color = "darkred", size = 1.1) +
  labs(
    title = "Lewis Hamilton — Career Trend: LOESS vs GAM",
    subtitle = "Blue dashed = LOESS; Dark red = GAM",
    x = "Year",
    y = "Total Scaled Points"
  ) +
  theme_minimal()
#===^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^=================



#PREDICTING
#Lower RMSE => better model. High RMSE => predictions are far from actual values
#Lower MAE => better model. 

start_year <- min(ham_season$race_year)
end_year   <- max(ham_season$race_year)

ham_ts <- ts(ham_season$points_per_race, start = start_year, frequency = 1)
autoplot(ham_ts) + ggtitle("Lewis Hamilton - Points per Race per Season")
summary(ham_ts)

h <- 3

#=============== auto-ARIMA ====================================================

ham_arima <- auto.arima(ham_ts, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
summary(ham_arima)

checkresiduals(ham_arima)
#ljungbox p = 0.67 => Positive and <1 => points-per-race is correlated with the previous season, but not perfectly.

f_arima <- forecast(ham_arima, h = h, level = c(80, 95))
f_arima #for point forecast values

autoplot(f_arima) +
  labs(title = "Lewis Hamilton - ARIMA Forecast - points per race",
       x = "Year", y = "Points per race") +
  theme_minimal()

resid_var <- var(residuals(ham_arima))
sqrt(resid_var) 

sim <- replicate(1000, as.numeric(simulate(ham_arima, nsim = h, future = TRUE)))
empirical_CI <- apply(sim, 1, quantile, probs = c(0.025, 0.975))
empirical_CI


hist_data <- data.frame(
  year = time(ham_ts),
  points = as.numeric(ham_ts))

f_arima_df <- data.frame(
  year = time(f_arima$mean),
  forecast = as.numeric(f_arima$mean),
  lo80 = f_arima$lower[,1],
  hi80 = f_arima$upper[,1],
  lo95 = f_arima$lower[,2],
  hi95 = f_arima$upper[,2])

#points as bar chart
ggplot() +
  geom_col(data = hist_data, aes(x = year, y = points), fill = "grey", alpha = 0.6) +
  geom_line(data = f_arima_df, aes(x = year, y = forecast), color = "red", size = 1.2) +
  geom_ribbon(data = f_arima_df, aes(x = year, ymin = lo95, ymax = hi95), alpha = 0.2, fill = "red") +
  geom_line(data = ham_season, aes(x = race_year, y = gam_fit), color = "black", size = 1.1) +
  labs(title = "Lewis Hamilton — Points per Race Forecast and Trend",
       x = "Year", y = "Points per Race") +
  theme_minimal()



#======================TRIALLING DIFF MODELS ===================================

#=========== log of Arima ===========
ham_ts_log <- log(ham_ts + 1)  # add 1 to avoid log(0)
ham_arima_log <- auto.arima(ham_ts_log)
f_log <- forecast(ham_arima_log, h = h)
f_log$mean  <- exp(f_log$mean) - 1
f_log$lower <- exp(f_log$lower) - 1
f_log$upper <- exp(f_log$upper) - 1
summary(ham_arima_log) #MAE 0.206  RMSE 0.256
autoplot(f_log) +
  ggtitle("Log-transformed ARIMA Forecast - Lewis Hamilton (points per race)") +
  ylab("Points per race") +
  xlab("Season")
#=====================================
#============adding drift=============
ham_arimaDrift <- auto.arima(ham_ts, stepwise = FALSE, approximation = FALSE, allowdrift = TRUE)
f_arimaDrift <- forecast(ham_arimaDrift, h = 3) #no drift present
summary(f_arimaDrift)  #exact same
autoplot(f_arimaDrift) +
  ggtitle("Lewis Hamilton - ARIMA Forecast (with Drift)") +
  ylab("Total Normalised Points") +
  theme_minimal()

#============= ETS ===================
ham_ets <- ets(ham_ts)
summary(ham_ets)       #MAE 2.605  RMSE 3.84
checkresiduals(ham_ets)
f_ets   <- forecast(ham_ets, h = h, level = c(80, 95)) #giving flat forecast
autoplot(f_ets) + 
  labs(title = "ETS Forecast - Lewis Hamilton (points per race)",
       x = "year", y = "points per race") +
  theme_minimal()

#ETS(AAN) -- worse
ham_ets_AAN <- ets(ham_ts, model = "AAN")
summary(ham_ets_AAN)

#ETS(AAN) damped -- worse
ham_etsAAN_damped <- ets(ham_ts, model = "AAN", damped = TRUE)
summary(ham_etsAAN_damped)

#log only gives the same values

#ETS damped ==========
ham_etsDamped <- ets(ham_ts, model = "AAN", damped = TRUE)
f_ets <- forecast(ham_ets, h = 3)
summary(ham_etsDamped)
autoplot(f_ets) +
  ggtitle("Lewis Hamilton — Damped Trend ETS Forecast") +
  ylab("Total Normalised Points") +
  theme_minimal()
#===============================================================================

# GAM smoother for visual trend - points per race
gam_mod <- gam(points_per_race ~ s(race_year, k = 10), data = ham_season)
ham_season$gam_fit <- predict(gam_mod, newdata = ham_season)

#===============================================================================












#===============================================================================
#===============================================================================

final_races_wdc <- races_0 %>%
  group_by(year) %>%
  summarise(final_race_id = raceId[which.max(round)], .groups = "drop")


top3_wdc <- driver_standings_0 %>%
  inner_join(final_races_wdc, by = c("raceId" = "final_race_id")) %>%
  filter(year >= 1990 & year <= 2024,
         position <= 3) %>%
  left_join(
    drivers_0 %>%
      mutate(driver_name = paste(forename, surname)) %>%
      select(driverId, driver_name, nationality),
    by = "driverId"
  ) %>%
  select(year, position, driverId, driver_name, nationality, points, wins) %>%
  arrange(desc(year), position)


print(top3_wdc)


driver_yearly_scaled <- results_scaled %>%
  filter(year >= 1990, year <= 2024) %>%
  group_by(driverId, driver_name, year) %>%
  summarise(total_scaled_points = sum(scaled_points, na.rm = TRUE), .groups = "drop")

top3_unique <- top3_wdc %>%
  filter(position <= 3, year >= 1990, year <= 2024) %>%
  filter(driver_name != "Lewis Hamilton") %>%
  distinct(driverId, driver_name)

retired_2024_names <- c(
  "Daniel Ricciardo",
  "Logan Sargeant",
  "Valtteri Bottas",
  "Zhou Guanyu",
  "Kevin Magnussen"
)

# Map names (both orders) to driverIds present in drivers_0
# so we can robustly match regardless of "forename surname" vs "surname forename"
retired_2024_ids <- drivers_0 %>%
  mutate(
    driver_name = paste(forename, surname),
    driver_name_rev = paste(surname, forename)
  ) %>%
  filter(driver_name %in% retired_2024_names | driver_name_rev %in% retired_2024_names) %>%
  distinct(driverId) %>%
  pull(driverId)

# Last active year from results
driver_last_year <- results_scaled %>%
  distinct(driverId, driver_name, year) %>%
  group_by(driverId, driver_name) %>%
  summarise(last_active_year = max(year, na.rm = TRUE), .groups = "drop")

# Build pool of retired drivers among the unique top-3 list (Hamilton excluded already)
retired_driver_pool <- top3_unique %>%
  left_join(driver_last_year, by = c("driverId", "driver_name")) %>%
  mutate(
    retired_by_2024 = (last_active_year < 2024) |
      (last_active_year == 2024 & (driverId %in% retired_2024_ids))
  ) %>%
  filter(retired_by_2024) %>%
  select(driverId, driver_name)


split_into_stints <- function(years_vec) {
  yrs <- sort(unique(years_vec))
  if (length(yrs) == 0) return(tibble(stint_id = integer(), year = integer()))
  # new stint when gap > 1
  stint_id <- cumsum(c(1, diff(yrs) > 1))
  tibble(year = yrs, stint_id = stint_id)
}

driver_stints <- results_scaled %>%
  filter(driverId %in% retired_driver_pool$driverId) %>%
  distinct(driverId, driver_name, year) %>%
  filter(year >= 1990, year <= 2024) %>%
  arrange(driverId, year) %>%
  group_by(driverId, driver_name) %>%
  group_modify(~ split_into_stints(.x$year)) %>%
  ungroup()

print(driver_stints) #Seems to be working fine for drivers like Kimi who left in 2010


driver_stints <- driver_stints %>%
  group_by(driverId, driver_name, stint_id) %>%
  summarise(
    start_year = min(year), end_year = max(year),
    years = list(sort(year)),
    .groups = "drop"
  ) %>%
  arrange(driver_name, start_year) %>%
  group_by(driverId, driver_name) %>%
  mutate(stint_num = row_number(),
         stint_label = paste0(driver_name, "_", stint_num)) %>%
  ungroup()

#======================Superfluous? or Am i stupid? ====================================
safe_auto_arima <- function(ts_series) {
  # Safely fit ARIMA; return NULL if not enough data / errors
  if (length(ts_series) < 2) return(NULL)
  tryCatch(
    {
      auto.arima(ts_series, stepwise = FALSE, approximation = FALSE)
    },
    error = function(e) NULL
  )
}

stint_models <- driver_stints %>%
  mutate(
    data = pmap(list(driverId, years),
                function(did, yrs) {
                  # pull yearly scaled points for this driver and these active years
                  df <- driver_yearly_scaled %>%
                    filter(driverId == did, year %in% unlist(yrs)) %>%
                    arrange(year)
                  # ensure we include years with 0 total_scaled_points (if they raced but scored 0)
                  # build a complete frame over the stint's active years
                  yr_complete <- tibble(year = unlist(yrs))
                  df <- yr_complete %>%
                    left_join(df, by = "year") %>%
                    mutate(
                      driverId = ifelse(is.na(driverId), did, driverId),
                      driver_name = driver_stints$driver_name[match(did, driver_stints$driverId)][1],
                      total_scaled_points = replace_na(total_scaled_points, 0)
                    ) %>%
                    arrange(year)
                  df
                }),
    ts = map(data, ~ {
      if (nrow(.x) < 2) return(NULL)
      ts(.x$total_scaled_points, start = min(.x$year), end = max(.x$year), frequency = 1)
    }),
    fit = map(ts, safe_auto_arima),
    fitted_vals = map2(fit, data, ~ {
      if (is.null(.x)) return(rep(NA_real_, nrow(.y)))
      fv <- as.numeric(fitted(.x))
      # align length (sometimes fitted is shorter for ARIMA with differencing)
      if (length(fv) < nrow(.y)) {
        c(rep(NA_real_, nrow(.y) - length(fv)), fv)
      } else fv
    })
  )


invisible(
  pwalk(
    list(stint_models$data,
         stint_models$fitted_vals,
         driver_stints$stint_label,
         driver_stints$start_year,
         driver_stints$end_year),
    function(df, fv, lbl, y0, y1) {
      if (nrow(df) < 2) {
        message(lbl, " (", y0, "-", y1, "): <2 points, skipping ARIMA plot.")
        return(invisible(NULL))
      }
      p <- ggplot(df, aes(x = year, y = total_scaled_points)) +
        geom_line(linewidth = 0.9) +
        geom_point(size = 2) +
        geom_line(aes(y = fv), linetype = "dashed") +
        labs(
          title = paste0("ARIMA — ", lbl, " [", y0, "–", y1, "]"),
          x = "Year", y = "Scaled points (per season)",
          subtitle = "Solid: actual | Dashed: ARIMA fitted (no forecast)"
        ) +
        theme_minimal()
      print(p)
    }
  )
)

agg_yearly <- driver_yearly_scaled %>%
  filter(driverId %in% retired_driver_pool$driverId) %>%
  group_by(year) %>%
  summarise(mean_scaled_points = mean(total_scaled_points, na.rm = TRUE), .groups = "drop") %>%
  arrange(year)

if (nrow(agg_yearly) >= 2) {
  agg_ts <- ts(agg_yearly$mean_scaled_points,
               start = min(agg_yearly$year),
               end   = max(agg_yearly$year),
               frequency = 1)
  agg_fit <- safe_auto_arima(agg_ts)
  agg_fitted <- if (!is.null(agg_fit)) {
    fv <- as.numeric(fitted(agg_fit))
    if (length(fv) < nrow(agg_yearly)) {
      c(rep(NA_real_, nrow(agg_yearly) - length(fv)), fv)
    } else fv
  } else rep(NA_real_, nrow(agg_yearly))
  
  p_agg <- ggplot(agg_yearly, aes(x = year, y = mean_scaled_points)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_line(aes(y = agg_fitted), linetype = "dashed") +
    labs(
      title = "Aggregate ARIMA — Retired Top-3 Drivers (1990–2024, excl. Hamilton)",
      x = "Year", y = "Mean scaled points (per driver per season)",
      subtitle = "Solid: actual mean | Dashed: ARIMA fitted (no forecast)"
    ) +
    theme_minimal()
  print(p_agg)
} else {
  message("Aggregate series has <2 points; skipping aggregate ARIMA.")
}
#====================================================================================


# ── helper
split_into_stints <- function(years_vec) {
  yrs <- sort(unique(years_vec))
  if (length(yrs) == 0) return(tibble(year = integer(), stint_id = integer()))
  tibble(year = yrs, stint_id = cumsum(c(1, diff(yrs) > 1)))
}

safe_auto_arima <- function(x) {
  if (length(x) < 2) return(NULL)
  tryCatch(auto.arima(x, stepwise = TRUE, approximation = FALSE),
           error = function(e) NULL)
}

# ── 1) yearly scaled points per driver (active years only, 1990–2024) ─────────
driver_yearly_scaled <- results_scaled %>%
  filter(year >= 1990, year <= 2024) %>%
  group_by(driverId, driver_name, year) %>%
  summarise(total_scaled_points = sum(scaled_points, na.rm = TRUE), .groups = "drop")

# ── 2) robust WDC list from final race standings (coerce position -> integer) ─
final_races <- races_0 %>%
  group_by(year) %>%
  summarise(final_race_id = raceId[which.max(round)], .groups = "drop")

wdc_by_year <- driver_standings_0 %>%
  semi_join(final_races, by = c("raceId" = "final_race_id")) %>%
  mutate(position = suppressWarnings(as.integer(position))) %>%
  filter(position == 1) %>%
  left_join(final_races, by = c("raceId" = "final_race_id")) %>%  # bring year in
  left_join(
    drivers_0 %>% mutate(driver_name = paste(forename, surname)) %>%
      select(driverId, driver_name),
    by = "driverId"
  ) %>%
  select(year, driverId, driver_name)

wdc_unique <- wdc_by_year %>% distinct(driverId, driver_name)

# ── 3) keep only WDCs retired BEFORE 2024; exclude Hamilton ───────────────────
driver_last_year <- results_scaled %>%
  distinct(driverId, driver_name, year) %>%
  group_by(driverId, driver_name) %>%
  summarise(last_active_year = max(year, na.rm = TRUE), .groups = "drop")

wdc_retired <- wdc_unique %>%
  left_join(driver_last_year, by = c("driverId","driver_name")) %>%
  filter(!is.na(last_active_year), last_active_year < 2024) %>%
  # must have at least one season in 1990–2024 window
  semi_join(driver_yearly_scaled %>% distinct(driverId), by = "driverId") %>%
  filter(driver_name != "Lewis Hamilton")

# sanity checks: no active champs & no non-WDCs
stopifnot(!any(wdc_retired$driver_name %in% c("Max Verstappen","Fernando Alonso")))
stopifnot(all(wdc_retired$driverId %in% wdc_unique$driverId))

message("Retired WDCs (excl. Hamilton): ",
        paste(sort(wdc_retired$driver_name), collapse = ", "))

# ── 4) split each retired WDC's active years into stints (consecutive years) ──
driver_stints <- results_scaled %>%
  filter(driverId %in% wdc_retired$driverId, year >= 1990, year <= 2024) %>%
  distinct(driverId, driver_name, year) %>%
  arrange(driverId, year) %>%
  group_by(driverId, driver_name) %>%
  group_modify(~ split_into_stints(.x$year)) %>%
  ungroup() %>%
  group_by(driverId, driver_name, stint_id) %>%
  summarise(
    start_year = min(year), end_year = max(year),
    years = list(sort(year)),
    .groups = "drop"
  ) %>%
  arrange(driver_name, start_year) %>%
  group_by(driverId, driver_name) %>%
  mutate(stint_num = row_number(),
         stint_label = paste0(driver_name, "_", stint_num)) %>%
  ungroup()

# Guard: only WDCs present
stopifnot(all(driver_stints$driverId %in% wdc_retired$driverId))

message("Total stints to model: ", nrow(driver_stints))

# ── 5) per-stint series (active years), ARIMA fit (in-sample only) ────────────
stint_models <- driver_stints %>%
  mutate(
    data = pmap(list(driverId, years),
                function(did, yrs) {
                  yrs <- sort(unlist(yrs))
                  base <- tibble(year = yrs)
                  pts  <- driver_yearly_scaled %>%
                    filter(driverId == did, year %in% yrs) %>%
                    select(year, total_scaled_points)
                  base %>%
                    left_join(pts, by = "year") %>%
                    mutate(total_scaled_points = tidyr::replace_na(total_scaled_points, 0)) %>%
                    arrange(year)
                }),
    ts = map(data, ~ if (nrow(.x) >= 2)
      ts(.x$total_scaled_points, start = min(.x$year), end = max(.x$year), frequency = 1) else NULL),
    fit = map(ts, safe_auto_arima),
    fitted_vals = map2(fit, data, ~ {
      if (is.null(.x)) return(rep(NA_real_, nrow(.y)))
      fv <- as.numeric(fitted(.x))
      if (length(fv) < nrow(.y)) c(rep(NA_real_, nrow(.y) - length(fv)), fv) else fv
    })
  )

# ── 6) clearer small-multiples: bigger text, 4 columns, uncluttered axes ──────
plot_df <- stint_models %>%
  select(driver_name, stint_label, start_year, end_year, data, fitted_vals) %>%
  mutate(
    actual = purrr::map(data, ~ dplyr::transmute(.x, year, value = total_scaled_points, series = "Actual")),
    fitted = purrr::map2(data, fitted_vals, ~ dplyr::transmute(.x, year, value = .y, series = "ARIMA fitted"))
  ) %>%
  mutate(df = purrr::map2(actual, fitted, ~ dplyr::bind_rows(.x, .y))) %>%
  select(-data, -fitted_vals, -actual, -fitted) %>%
  tidyr::unnest(df) %>%
  dplyr::filter(series != "ARIMA fitted" | !is.na(value))

# facet labels like "Michael Schumacher — stint 2 (2010–2012)"
stint_labs <- driver_stints %>%
  dplyr::transmute(
    stint_label,
    n_years  = purrr::map_int(years, length),
    is_short = n_years <= 3L,
    facet_lab = paste0(driver_name, " — stint ", stint_num, " (", start_year, "–", end_year, ")")
  )

plot_df <- plot_df %>%
  dplyr::left_join(stint_labs, by = "stint_label")

# helpers to pick endpoints for short stints
short_endpoints <- plot_df %>%
  dplyr::filter(is_short, series == "Actual") %>%
  dplyr::group_by(stint_label) %>%
  dplyr::filter(year %in% range(year)) %>%
  dplyr::ungroup()

# ── plot: thicker for short stints, hide ARIMA on short ones, free x & y ──────
p <- ggplot() +
  # long stints: actual
  geom_line(data = dplyr::filter(plot_df, !is_short, series == "Actual"),
            aes(year, value), linewidth = 1) +
  geom_point(data = dplyr::filter(plot_df, !is_short, series == "Actual"),
             aes(year, value), size = 2) +
  # long stints: ARIMA fitted (dashed)
  geom_line(data = dplyr::filter(plot_df, !is_short, series == "ARIMA fitted"),
            aes(year, value), linetype = "dashed") +
  
  # short stints: actual (thicker / bigger)
  geom_line(data = dplyr::filter(plot_df, is_short, series == "Actual"),
            aes(year, value), linewidth = 1.4) +
  geom_point(data = dplyr::filter(plot_df, is_short, series == "Actual"),
             aes(year, value), size = 2.8) +
  # short stints: label endpoints with the year
  geom_text(data = short_endpoints,
            aes(year, value, label = year),
            vjust = -0.6, size = 3) +
  
  facet_wrap(~ facet_lab, scales = "free", ncol = 3) +
  scale_x_continuous(breaks = scales::pretty_breaks(4)) +
  labs(
    title = "Career Trajectories — Retired World Champions (1990–2024, excl. Hamilton)",
    subtitle = "Solid = actual scaled points per season; Dashed = ARIMA in-sample fit (no forecast)\nShort stints are emphasized and omit ARIMA fit",
    x = "Year", y = "Scaled points (per season)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p)

# ── 7) Export at max clarity ──────────────────────────────────────────────────
# Choose reasonable canvas size based on number of facets
n_facets <- dplyr::n_distinct(plot_df$facet_lab)
ncol <- 3
nrow <- ceiling(n_facets / ncol)
# inches: ~4in per col, ~3.5in per row (tweak as you like)
W <- 4 * ncol
H <- 3.5 * nrow

# (A) Vector PDF (crisp at any zoom)
ggplot2::ggsave("wdc_stints.pdf", plot = p, width = W, height = H, device = cairo_pdf)

# (B) Vector SVG (great for web/Illustrator) — requires svglite
if (requireNamespace("svglite", quietly = TRUE)) {
  svglite::svglite("wdc_stints.svg", width = W, height = H); print(p); dev.off()
}

# (C) High-DPI PNG (publication raster) — use ragg if available
if (requireNamespace("ragg", quietly = TRUE)) {
  ragg::agg_png("wdc_stints_600dpi.png", width = round(W*300), height = round(H*300), res = 600)
  print(p); dev.off()
} else {
  ggplot2::ggsave("wdc_stints_600dpi.png", plot = p, width = W, height = H, dpi = 600)
}


