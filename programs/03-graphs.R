# This is a script
# to plot some graphs
# of suicide data in
# in the US

# date: July 26th, 2022

# open data

state_year <- read_csv(file.path(datasets,"state_deaths_by_year.csv")) |> 
  mutate(merge_year = case_when(year < 1970                ~ 1960,
                                year < 1980 & year >= 1970 ~ 1970,
                                year < 1990 & year >= 1980 ~ 1980,
                                year < 2000 & year >= 1990 ~ 1990,
                                year >= 2000               ~ year),
         `total suicide` = case_when(year < 1972 | year > 1972 ~ `total suicide`,
                                     year == 1972 ~ `total suicide` * 2),
         `total deaths` = case_when(year < 1972 | year > 1972 ~ `total deaths`,
                                     year == 1972 ~ `total deaths` * 2),
         `total suicide by firearm` = case_when(year < 1972 | year > 1972 ~ `total suicide by firearm`,
                                    year == 1972 ~ `total suicide by firearm` * 2),
         state_name = fips(stateoc, to = 'Name'))
us_year    <- read_csv(file.path(datasets,"us_deaths_by_year.csv")) |> 
  mutate(merge_year = case_when(year < 1970                ~ 1960,
                                year < 1980 & year >= 1970 ~ 1970,
                                year < 1990 & year >= 1980 ~ 1980,
                                year < 2000 & year >= 1990 ~ 1990,
                                year >= 2000               ~ year),
         `total suicide` = case_when(year < 1972 | year > 1972 ~ `total suicide`,
                                     year == 1972 ~ `total suicide` * 2),
         `total deaths` = case_when(year < 1972 | year > 1972 ~ `total deaths`,
                                    year == 1972 ~ `total deaths` * 2),
         `total suicide by firearm` = case_when(year < 1972 | year > 1972 ~ `total suicide by firearm`,
                                                year == 1972 ~ `total suicide by firearm` * 2))



population <- read_csv(file.path(datasets,"Population.csv")) |> 
  rename(
         "stateoc" = "STATEFIP") |> 
  mutate(merge_year = YEAR)

# Calculate US Populations 
us_population <- read_csv(file.path(datasets,"US_Population.csv"))

# merge population
# and suicide data

state_year <- left_join(
  state_year,
  population,
  na_matches = "never",
  by = c("merge_year", "stateoc")
)

us_year <- left_join(
  us_year,
  us_population,
  na_matches = "never",
  by = c("merge_year")
)

# calculate suicides
# per 100,000
# for per state
# per year data

state_year <- state_year |> 
  mutate(suicide_per_hund_th = `total suicide`/Population * 100000,
         suicide_fire_per_hund_th = `total suicide by firearm`/Population * 100000)

us_year <- us_year |> 
  mutate(suicide_per_hund_th = `total suicide`/Population * 100000,
         suicide_fire_per_hund_th = `total suicide by firearm`/Population * 100000)

# plot graph

## suicide by-state-by-year

### by firearms
P56 = unname(createPalette(56,  c("#ff0000", "#00ff00", "#0000ff")))
p1 <- ggplot(state_year, aes(year, suicide_fire_per_hund_th)) +
  geom_point(aes(color = factor(state_name))) +
  stat_summary(geom = "line", aes(color = factor(state_name))) +
  labs(x = "Year", y = "Suicide by Firearms per 100,000: by State") +
  ggtitle("Suicide by Firearms") + 
  scale_color_manual(values = P56)
  # scale_colour_viridis_d(option = "plasma")
  # scale_colour_viridis_d()
  # scale_colour_viridis_d(option = "inferno")
# +
#   theme(legend.position = "none")
p1
ggsave(paste0(figures_wd,"/suicide_firearm_byyearbystate.png"), width = 10, height = 4, units = "in")
ggsave(paste0(thesis_plots,"/suicide_firearm_byyearbystate.png"), width = 10, height = 4, units = "in")

### total suicides
p2 <- ggplot(state_year, aes(year, suicide_per_hund_th)) +
  geom_point(aes(color = factor(state_name))) +
  stat_summary(geom = "line", aes(color = state_name)) +
  labs(x = "Year", y = "Suicide per 100,000: by State") +
  ggtitle("Suicides") +
  scale_color_manual(values = P56)
p2
ggsave(paste0(figures_wd,"/suicide_byyearbystate.png"), width = 10, height = 4, units = "in")
ggsave(paste0(thesis_plots,"/suicide_byyearbystate.png"), width = 10, height = 4, units = "in")

## suicides in the US

### firearms
p3 <- ggplot(us_year, aes(year, `total suicide by firearm`)) +
  geom_point() +
  stat_summary(geom = "line") +
  labs(x = "Year", y = "Suicide by Firearms") +
  ggtitle("Suicide by Firearms in the US") +
  theme(legend.position = "none")
p3
ggsave(paste0(figures_wd,"/suicide_firearm_byyear.png"), width = 10, height = 4, units = "in")
ggsave(paste0(thesis_plots,"/suicide_firearm_byyear.png"), width = 10, height = 4, units = "in")

### total suicides
p4 <- ggplot(us_year, aes(year, `total suicide`)) +
  geom_point() +
  stat_summary(geom = "line") +
  labs(x = "Year", y = "Suicides") +
  ggtitle("Suicides  in the US") +
  theme(legend.position = "none")
p4
ggsave(paste0(figures_wd,"/suicide_byyear.png"), width = 10, height = 4, units = "in")
ggsave(paste0(thesis_plots,"/suicide_byyear.png"), width = 10, height = 4, units = "in")


### firearms per 100,000
p5 <- ggplot(us_year, aes(year, suicide_fire_per_hund_th)) +
  geom_point() +
  stat_summary(geom = "line") +
  labs(x = "Year", y = "Suicide by Firearms per 100,000") +
  ggtitle("Suicide by Firearms in the US") +
  theme(legend.position = "none")
p5
ggsave(paste0(figures_wd,"/suicide_firearm_byyear_popcont.png"), width = 10, height = 4, units = "in")
ggsave(paste0(thesis_plots,"/suicide_firearm_byyear_popcont.png"), width = 10, height = 4, units = "in")

### total suicides
p6 <- ggplot(us_year, aes(year, suicide_per_hund_th)) +
  geom_point() +
  stat_summary(geom = "line") +
  labs(x = "Year", y = "Suicides per 100,000") +
  ggtitle("Suicides  in the US") +
  theme(legend.position = "none")
p6
ggsave(paste0(figures_wd,"/suicide_byyear_popcont.png"), width = 10, height = 4, units = "in")
ggsave(paste0(thesis_plots,"/suicide_byyear_popcont.png"), width = 10, height = 4, units = "in")

