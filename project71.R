#libraries to load
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(scales)
library(ggfortify)
library(Matrix)
library(lme4)
library(forecast)
library(lmerTest)
library(ggplot2)
library(performance)

#dataset available at:
# https://www.kaggle.com/datasets/rohanrao/formula-1-world-championship-1950-2020

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
#SECTION 2: EXPLORATORY DATA ANALYSIS ==========================================
#===============================================================================

#checking for null values
lapply(datasets, function(df) {
  sapply(df, function(x)
    sum(is.na(x) | x == "" | x == "\\N" | x == "###" | x == "null")
  )
})

#Checking unique values
length(unique(circuits_0$circuitId))  #77
length(unique(drivers_0$driverId))  #861
length(unique(constructors_0$constructorId))  #212

#some summary stats
summary(results_0)

#Results =======================================================================

#Sprint weekends
common_race_ids <- intersect(results_0$raceId, sprint_results_0$raceId)
length(common_race_ids)  # number of Sprint race weekends

results_ns <- results_0 %>%          #results with no sprint races
  left_join(
    races_0 %>% 
      dplyr::select(raceId, year),
    by = "raceId"
  )

#Confirming all Grand Prix still in the dataset
length(unique(sprint_results_0$raceId))  #18
length(unique(results_0$raceId))  #1125
length(unique(results_ns$raceId)) #1125

#taking minimal result info
results <- results_ns %>%
  select(raceId, driverId, constructorId, positionOrder, points) %>% 
  filter(!is.na(positionOrder))  #NA rows where no points scored

results_clean <- results_ns %>%      
  mutate(
    positionOrder = as.numeric(positionOrder),
    points = as.numeric(points),
  )

if(nrow(dupes) > 0) {
  message("Warning: duplicate driver-race rows found. We'll keep the first occurrence per (raceId, driverId).")
  results_clean <- results_clean %>%
    arrange(raceId, driverId) %>%
    distinct(raceId, driverId, .keep_all = TRUE)
}

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

#===============================================================================

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

#Scaling points to match current system ========================================

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

print(era_points)
results_scaled <- results_scaled %>%
  left_join(era_points, by = "era")

results_scaled %>%
  select(driver_name, year, era, points, scaled_points, max_points_for_win) %>%
  head(10)

era_summary <- results_scaled %>%
  group_by(era) %>%
  summarise(max_points = max(points, na.rm = TRUE),
            .groups = "drop")

#regulation changes ============================================================

eras <- data.frame(
  era = c("1.5L Era", "Turbo Era", "Safety/Aero Era", "V10/V8 Era", 
          "Aero Overhaul", "Hybrid Era", "Ground Effect Era"),
  start = c(1961, 1984, 1994, 2005, 2009, 2014, 2022),
  end   = c(1983, 1993, 2004, 2008, 2013, 2021, 2025)
)

eras <- eras %>%
  mutate(start = start - 0.5,
         end   = end + 0.5)
print(eras)

#drivers =======================================================================

drivers_clean <- drivers_0 %>%
  mutate(driver_name = paste(forename, surname)) %>%
  select(driverId, driver_name)

race_winners <- results_ns %>%
  filter(positionOrder==1) %>%
  left_join(races_0 %>% select(raceId, year), by = "raceId") %>%
  left_join(drivers_clean, by = "driverId")

drivers <- drivers_0 %>%
  select(driverId, forename, surname) %>%  
  mutate(driver_name = paste(forename, surname)) %>%
  select(driverId, driver_name)

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

write_csv(dr_summary, "driver_resuslts_summary.csv")

head(dr_summary)

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

driver_career %>%     #to inspect output
  filter(driver_name == "Lewis Hamilton")

career_atl1 <- results_scaled %>%    #drivers with at least 1 win 
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

results_ns <- results_ns %>%    #remove duplicate year column
  mutate(
    year = coalesce(year.x, year.y)  
  ) %>%
  select(-year.x, -year.y)                 

results_ns %>%                      #inspecting output == should be 22
  filter(year == 2023) %>%
  summarise(total_races = n_distinct(raceId))

#races and circuits ============================================================

races_0 %>%       
  count(year) %>%
  ggplot(aes(x = year, y = n)) +
  theme_minimal()  +
  geom_bar(stat = "identity", fill = "firebrick3", lwd = 4) +
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

#Exploring the data ============================================================

#drivers by total wins (no sprints) ============================================
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

#teams by total wins (no sprints) ==============================================
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

#Average age by season =========================================================

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

#Chaos index - unique race winner ==============================================
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
# SECTION 3: DRIVER vs TEAM CONTRIBUTION:
#===============================================================================

model_data <- results_scaled %>%
  select(driver_name, constructorId, race_year, scaled_points) %>%
  filter(!is.na(scaled_points)) %>%
  mutate(
    driver_name = factor(driver_name),
    constructorId = factor(constructorId),
    year = factor(race_year)   # treat year as random effect
  )

model_data <- results_scaled %>%
  select(driver_name, constructorId, race_year, scaled_points) %>%
  filter(!is.na(scaled_points)) %>%
  group_by(driver_name) %>%
  mutate(seasons_competed = n_distinct(race_year)) %>%
  ungroup() %>%
  filter(seasons_competed >= 3) %>%    
  mutate(
    driver_name = factor(driver_name),
    constructorId = factor(constructorId),
    year = factor(race_year)  # treat year as fixed effect
  ) %>%
  select(-seasons_competed) 

#Model 1 -- baseline model
mem_base <- lmer(scaled_points ~ 1 + (1 | driver_name) + (1 | constructorId),data = model_data)
summary(mem_base)
mem_base <- lmer(scaled_points ~ 1 + (1 | driver_name) + (1 | constructorId), data = model_data, REML = TRUE)

#Model 2 -- with year
mem_year <- lmer(scaled_points ~ factor(year) + (1 | driver_name) + (1 | constructorId), data = model_data)
summary(mem_year)
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
r2_summary

#Random effect variances
var_base <- as.data.frame(VarCorr(mem_base)) %>% select(grp, vcov, sdcor)
var_year <- as.data.frame(VarCorr(mem_year)) %>% select(grp, vcov, sdcor)

print(var_base)
print(var_year)

#1 => base model
driver_skill1 <- ranef(mem_base)$driver_name %>%
  rownames_to_column("driver_name") %>%
  rename(skill = `(Intercept)`)

team_strength1 <- ranef(mem_base)$constructorId %>%
  rownames_to_column("constructorId") %>%
  rename(strength = `(Intercept)`)

head(driver_skill1[order(-driver_skill1$skill), ])
head(team_strength1[order(-team_strength1$strength), ])

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

#2 => model with year
driver_skill2 <- ranef(mem_year)$driver_name %>%
  rownames_to_column("driver_name") %>%
  rename(skill = `(Intercept)`)

team_strength2 <- ranef(mem_year)$constructorId %>%
  rownames_to_column("constructorId") %>%
  rename(strength = `(Intercept)`)

head(driver_skill2[order(-driver_skill2$skill), ])
head(team_strength2[order(-team_strength2$strength), ])

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

#Visualising strongest drivers and teams =======================================

top_drivers <- driver_effects2 %>% slice_max(driver_skill2, n = 10)  # Top 10
ggplot(top_drivers, aes(x = reorder(driver_name, driver_skill2), y = driver_skill2)) +
  geom_col(fill = "firebrick3") +
  coord_flip() +
  labs(
    title = "Top 10 Drivers by Estimated Skill",
    x = "Driver",
    y = "Estimated points above average (controlling for team)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x  = element_text(face = "bold", size = 12),
    axis.text.y  = element_text(face = "bold", size = 12)
  )

top_teams <- constructor_effects2 %>% slice_max(team_strength2, n = 10)
ggplot(top_teams, aes(x = reorder(name, team_strength2), y = team_strength2)) +
  geom_col(fill = "blue4") +
  coord_flip() +
  labs(
    title = "Top 10 Teams by Estimated Strength",
    x = "Constructor",
    y = "Estimated points above average (controlling for driver)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x  = element_text(face = "bold", size = 12),
    axis.text.y  = element_text(face = "bold", size = 12)
  )

#===============================================================================
#SECTION 4: LEWIS HAMILTON
#===============================================================================

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

#plot historical data with LOESS smoothing 
ggplot(ham_season, aes(x = race_year)) +
  geom_col(aes(y = points_per_race), fill = "grey", alpha = 0.6) +
  geom_smooth(aes(y = points_per_race), method = "loess", span = 0.35, se = TRUE, color = "black") +
  labs(title = "Lewis Hamilton - points per race by season (and LOESS trend)",
       x = "Year", y = "Points per race") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),       # bold title
    axis.title.x = element_text(face = "bold", size = 14),     # bold x-axis title
    axis.title.y = element_text(face = "bold", size = 14) )

start_year <- min(ham_season$race_year)
end_year   <- max(ham_season$race_year)

ham_ts <- ts(ham_season$points_per_race, start = start_year, frequency = 1)
autoplot(ham_ts) + ggtitle("Lewis Hamilton - Points per Race per Season")
summary(ham_ts)

h <- 3

acf(ham_ts, main = "ACF - Hamilton Points per Race", xlab = "Lag", ylab = "Autocorrelation", font.main = 3, font.lab = 2)
pacf(ham_ts, main = "PACF - Hamilton Points per Race")

#auto-ARIMA ====================================================================

ham_arima <- auto.arima(ham_ts, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
summary(ham_arima)

h_arima <- forecast(ham_arima, h = h, level = c(80, 95))
h_arima   

autoplot(h_arima) +
  labs(title = "Lewis Hamilton - ARIMA Forecast - points per race",
       x = "Year", y = "Points per race") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16),       # bold title
    axis.title.x = element_text(face = "bold", size = 12),     # bold x-axis title
    axis.title.y = element_text(face = "bold", size = 12)      # bold y-axis title
  )

checkresiduals(ham_arima)

resid_var <- var(residuals(ham_arima))
sqrt(resid_var) 

sim <- replicate(1000, as.numeric(simulate(ham_arima, nsim = h, future = TRUE)))
empirical_CI <- apply(sim, 1, quantile, probs = c(0.025, 0.975))
empirical_CI

#trialing other models ========================================================

#log of Arima ===========
ham_ts_log <- log(ham_ts + 1)  # add 1 to avoid log(0)
ham_arima_log <- auto.arima(ham_ts_log)
h_log <- forecast(ham_arima_log, h = h)
h_log$mean  <- exp(h_log$mean) - 1
h_log$lower <- exp(h_log$lower) - 1
h_log$upper <- exp(h_log$upper) - 1
summary(ham_arima_log) 
autoplot(h_log) +
  ggtitle("Log-transformed ARIMA Forecast - Lewis Hamilton (points per race)") +
  ylab("Points per race") +
  xlab("Season")

#adding drift=============
ham_arimaDrift <- auto.arima(ham_ts, stepwise = FALSE, approximation = FALSE, allowdrift = TRUE)
h_arimaDrift <- forecast(ham_arimaDrift, h = 3) #no drift present
summary(h_arimaDrift)  #exact same
autoplot(h_arimaDrift) +
  ggtitle("Lewis Hamilton - ARIMA Forecast (with Drift)") +
  ylab("Total Normalised Points") +
  theme_minimal()

#ETS ===================
ham_ets <- ets(ham_ts)
summary(ham_ets)       #MAE 2.605  RMSE 3.84
checkresiduals(ham_ets)
h_ets   <- forecast(ham_ets, h = h, level = c(80, 95)) #giving flat forecast
autoplot(h_ets) + 
  labs(title = "ETS Forecast - Lewis Hamilton (points per race)",
       x = "year", y = "points per race") +
  theme_minimal()

#ETS(AAN) -- worse
ham_ets_AAN <- ets(ham_ts, model = "AAN")
summary(ham_ets_AAN)

#ETS(AAN) damped -- worse
ham_etsAAN_damped <- ets(ham_ts, model = "AAN", damped = TRUE)
summary(ham_etsAAN_damped)

#ETS damped ==========
ham_etsDamped <- ets(ham_ts, model = "AAN", damped = TRUE)
h_ets <- forecast(ham_ets, h = 3)
summary(ham_etsDamped)
autoplot(h_ets) +
  ggtitle("Lewis Hamilton — Damped Trend ETS Forecast") +
  ylab("Total Normalised Points") +
  theme_minimal()

#WDC career curves =============================================================

driver_yearly_avg <- results_scaled %>%
  filter(year >= 1990, year <= 2024) %>%
  group_by(driverId, driver_name, year) %>%
  summarise(
    avg_scaled_points = mean(scaled_points, na.rm = TRUE),  # <-- average per race
    .groups = "drop"
  )

name_map <- drivers_0 %>%
  mutate(driver_name = paste(forename, surname),
         label_name  = paste0(substr(forename, 1, 1), ". ", surname)) %>%
  select(driverId, driver_name, label_name)

exclude_drivers <- c("Alain Prost", "Nelson Piquet")

wdc_retired_lbl <- wdc_retired %>%
  filter(!driver_name %in% exclude_drivers) %>%
  left_join(name_map, by = c("driverId","driver_name"))

wdc_retired_yearly <- driver_yearly_avg %>%
  filter(driverId %in% wdc_retired_lbl$driverId) %>%
  left_join(name_map, by = c("driverId", "driver_name")) %>%
  arrange(label_name, year)

traj <- ggplot(wdc_retired_yearly, aes(x = year, y = avg_scaled_points, group = label_name)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2.5) +
  facet_wrap(~ label_name, scales = "free", ncol = 5) +
  scale_x_continuous(breaks = scales::pretty_breaks(4)) +
  labs(
    title = "Career Trajectories — Retired World Champions (1990–2024, excl. Hamilton)",
    subtitle = "Average scaled points per race",
    x = "Year", y = "Avg. scaled points per race"
  ) +
  theme_minimal(base_size = 16) +
  theme(strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank())

print(traj)

#Contrasting teammates =========================================================

wdc_roster <- wdc_retired_lbl %>%
  distinct(driverId, driver_name, label_name) %>%
  arrange(label_name) %>%
  bind_rows(
    drivers_0 %>%
      mutate(driver_name = paste(forename, surname),
             label_name = paste0(substr(forename, 1, 1), ". ", surname)) %>%
      filter(driver_name == "Valtteri Bottas") %>%
      select(driverId, driver_name, label_name)
  ) %>%
  distinct(driverId, .keep_all = TRUE) %>%
  arrange(label_name)

h <- 3

for (i in seq_len(nrow(wdc_roster))) {
  did <- wdc_roster$driverId[i]
  lbl <- wdc_roster$label_name[i]
  
  drv_season <- results_scaled %>%
    filter(driverId == did) %>%
    group_by(year) %>%
    summarise(
      total_scaled_points = sum(scaled_points, na.rm = TRUE),
      races_in_season = n_distinct(raceId),
      points_per_race = if_else(races_in_season > 0,
                                total_scaled_points / races_in_season,
                                NA_real_),
      .groups = "drop"
    ) %>%
    arrange(year)
  
  start_year <- min(drv_season$year, na.rm = TRUE)
  drv_ts <- ts(drv_season$points_per_race, start = start_year, frequency = 1)
  
  print(
    autoplot(drv_ts) +
      ggtitle(paste0(lbl, " — Scaled Points per Race per Season")) +
      xlab("Year") + ylab("Points per race") +
      theme_minimal()
  )
  
  print(summary(drv_ts))
  
  drv_arima <- auto.arima(drv_ts, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
  print(summary(drv_arima))
  
  checkresiduals(drv_arima)
  
  drv_fc <- forecast(drv_arima, h = h, level = c(80, 95))
  
  print(
    autoplot(drv_fc) +
      labs(
        title = paste0(lbl, " - ARIMA Forecast (Points per Race)"),
        x = "Year", y = "Points per race"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14),   # bold and larger title
        axis.title.x = element_text(size = 12), # bold x-axis label
        axis.title.y = element_text(size = 12), # bold y-axis label
        axis.text.x = element_text(face = "bold", size = 12),  # bold x-axis tick labels
        axis.text.y = element_text(face = "bold", size = 12)   # bold y-axis tick labels
      )
  )
}

#===============================================================================
#SECTION 5: FERRARI vs MERCEDES
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

constructors_0 <- constructors_0 %>%
  mutate(name = str_trim(tolower(name)))

#Ferrari =======================================================================
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

acf(ferrari_ts)

ferrari_forecast <- forecast(ferrari_arima, h = 3)
print(ferrari_forecast)

ferrari_forecast_df <- data.frame(
  year = max(ferrari_season$year) + seq_len(length(ferrari_forecast$mean)),
  points_per_race = as.numeric(ferrari_forecast$mean),
  lower = as.numeric(ferrari_forecast$lower[, 2]),
  upper = as.numeric(ferrari_forecast$upper[, 2])
)

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
  geom_rect(
    data = reg_eras,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = era),
    alpha = 0.5  # richer opacity
  ) +
  geom_line(data = ferrari_season, aes(x = year, y = points_per_race),
            color = "black", size = 0.8) +
  geom_point(data = ferrari_season, aes(x = year, y = points_per_race),
             color = "black", size = 2) +
  geom_line(data = ferrari_forecast_df,
            aes(x = year, y = points_per_race),
            color = "red", linetype = "dotdash", size = 1) +
  geom_ribbon(data = ferrari_forecast_df,
              aes(x = year, ymin = lower, ymax = upper),
              fill = "white", alpha = 0.5) +
  scale_fill_manual(values = reg_eras$fill, name = "Regulation Era") +
  labs(
    title = "Ferrari Performance - Points per Race for both cars combined (by season)",
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

#Mercedes ======================================================================
mercedes_id <- constructors_0 %>%
  filter(name == "mercedes") %>%
  pull(constructorId)

#Aggregate season-level performance (Mercedes)
mercedes_season <- driver_season %>%
  filter(constructorId %in% mercedes_id) %>%
  rename(year = race_year) %>%
  group_by(year) %>%
  summarise(
    total_scaled_points = sum(total_scaled_points, na.rm = TRUE),
    wins              = sum(wins, na.rm = TRUE),
    dnfs              = sum(dnfs, na.rm = TRUE),
    races_entered     = sum(races_entered, na.rm = TRUE),
    .groups           = "drop"
  ) %>%
  filter(year > 1990) %>%
  arrange(year)

mercedes_season <- mercedes_season %>%
  left_join(season_lengths, by = "year") %>%
  mutate(points_per_race = total_scaled_points / races_in_season)

mercedes_ts <- ts(mercedes_season$points_per_race, start = min(mercedes_season$year), frequency = 1)
mercedes_arima <- auto.arima(mercedes_ts, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
summary(mercedes_arima)
checkresiduals(mercedes_arima)

acf(mercedes_ts)

mercedes_forecast <- forecast(mercedes_arima, h = 3)
print(mercedes_forecast)

mercedes_forecast_df <- data.frame(
  year = max(mercedes_season$year) + seq_len(length(mercedes_forecast$mean)),
  points_per_race = as.numeric(mercedes_forecast$mean),
  lower = as.numeric(mercedes_forecast$lower[, 2]),  
  upper = as.numeric(mercedes_forecast$upper[, 2])   
)
min_year <- min(mercedes_season$year)
max_year <- max(mercedes_forecast_df$year)

reg_eras$start[1] <- min_year
reg_eras$end[nrow(reg_eras)] <- max_year

ggplot() +
  geom_rect(
    data = reg_eras,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = era),
    alpha = 0.5
  ) +
  geom_line(
    data = mercedes_season,
    aes(x = year, y = points_per_race),
    color = "black",
    size  = 0.8
  ) +
  geom_point(
    data = mercedes_season,
    aes(x = year, y = points_per_race),
    color = "black",
    size  = 2
  ) +
  geom_line(
    data = mercedes_forecast_df,
    aes(x = year, y = points_per_race),
    color    = "blue",
    linetype = "dotdash",
    size     = 1
  ) +
  geom_ribbon(
    data = mercedes_forecast_df,
    aes(x = year, ymin = lower, ymax = upper),
    fill  = "white",
    alpha = 0.5
  ) +
  scale_fill_manual(values = reg_eras$fill, name = "Regulation Era") +
  labs(
    title    = "Mercedes Performance - Points per Race (by season)",
    subtitle = "Shaded backgrounds indicate major regulation eras",
    x        = "Year",
    y        = "Points per Race"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position   = "right",
    panel.grid.minor  = element_blank(),
    plot.title        = element_text(face = "bold")
  )

#===============================================================================
#===============================================================================
