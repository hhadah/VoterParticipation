# This a script 
# clean voting data

# Date: Aug 15th, 2022

### Open CPS
CPS <- read_csv(file.path(raw_wd, "cps_00046.csv"))

# Clean Data
CPS <- CPS %>% 
  mutate(
  Female = case_when(SEX == 1 ~ 0,
                     SEX == 2 ~ 1),
  Hispanic = case_when(HISPAN == 0 ~ 0,
                       HISPAN > 0 & HISPAN <= 4 ~ 1),
  Race = case_when(RACE == 1 ~ "White",
                   RACE == 2 ~ "Black",
                   RACE > 2 ~ "Other"),
  Age_group = case_when(AGE < 24 & AGE >= 18 ~ "18-24",
                        AGE < 34 & AGE >= 24 ~ "25-34",
                        AGE < 44 & AGE >= 34 ~ "35-44",
                        AGE < 55 & AGE >= 44 ~ "45-54",
                        AGE < 64 & AGE >= 55 ~ "55-64",
                        AGE >= 64 ~ "64+"),
  Voted = ifelse(VOTED == 2, 1, 0)
)

# calculate voting averages
# by age group by year

# Summary stats
library(survey)
library(srvyr)

options(survey.lonely.psu = "adjust")
options(na.action="na.pass")

CPS_wt <- subset(CPS, YEAR >= 1976) %>%
  filter(!is.na(VOSUPPWT)) %>% 
  as_survey(weights = VOSUPPWT)

CPS_voting <- CPS_wt |> 
  group_by(YEAR, Age_group) |> 
  summarise(Voted = survey_mean(Voted, na.rm = T))

CPS_voting <- CPS |> 
  group_by(YEAR, Age_group) |> 
  summarise(Voted = weighted.mean(Voted, VOSUPPWT, na.rm = T))

# graph

p1 <- ggplot(CPS_voting, aes(YEAR, Voted)) +
  geom_point(aes(color = factor(Age_group))) +
  stat_summary(geom = "line", aes(color = factor(Age_group))) +
  labs(x = "Year", y = "Share of Electorate",
       caption = "Data source: The Current Population Survey (CPS) Voter Supplement.") +
  ggtitle("In Your Face!") +
  scale_color_manual(values = cbbPalette,
                     # labels = c("New England", "M. Atlantic", "E. N. Central",
                     #            "W. N. Central", "S. Atlantic", "E. S. Central",
                     #            "W. S. Central", "Mountain", "Pacific"),
                     name = "Age group")
  # scale_x_continuous(limits = c(1977,1996), breaks = c(1977, 1982, 1985, 1988, 1990, 1992, 1994, 1996)) +
  # scale_y_continuous(limits = c(-0.65,0.65), breaks = c(-0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6)) +
  # theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
p1
ggsave(paste0(figures_wd,"/Voter_part.png"), width = 10, height = 4, units = "in")

# Save data
write_csv(CPS, file.path(datasets,"VotingData.csv"))
