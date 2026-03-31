##### DS340 Project #####

library(tidyverse)
library(data.table)
library(viridis)
library(sf)
library(maps)

### Load data
activity <- read.csv("data/atusact_2024.dat")
respondent <- read.csv("data/atusresp_2024.dat")
roster <- read.csv("data/atusrost_2024.dat")
who <- read.csv("data/atuswho_2024.dat")
cps <- read.csv("data/atuscps_2024.dat")

activity0324 <- read.csv("data/atusact_0324.dat")
respondent0324 <- read.csv("data/atusresp_0324.dat")
roster0324 <- read.csv("data/atusrost_0324.dat")
who0324 <- read.csv("data/atuswho_0324.dat")
cps0324 <- read.csv("data/atuscps_0324.dat")

#######################
#####DATA CLEANING#####
#######################
#### 2024 Data
### Single Parent Identification and Process Roster File

process_roster <- function(roster_df){
  roster_df %>%
    group_by(TUCASEID) %>%
    mutate(
      has_child = any(TERRP == 22 & TEAGE < 18),         # Own children under 18
      has_partner = any(TERRP %in% c(20,21))            # Spouse or unmarried partner
    ) %>%
    ungroup() %>%
    filter(TERRP %in% c(18,19)) %>%                     # Keep respondents
    mutate(single_parent = has_child & !has_partner)
}

roster24_new <- process_roster(roster)
roster0324_new <- process_roster(roster0324)

### Process Activity Files
process_activity <- function(activity_df, trcode_var = "TRCODE"){
  childcare <- activity_df %>%
    filter(str_starts(as.character(.data[[trcode_var]]), "301") |
             str_starts(as.character(.data[[trcode_var]]), "302") |
             str_starts(as.character(.data[[trcode_var]]), "303"))
  
  work <- activity_df %>%
    filter(str_starts(as.character(.data[[trcode_var]]), "5"))
  
  list(childcare = childcare, work = work)
}

act2024_new <- process_activity(activity, trcode_var = "TRCODE")

act0324_new <- process_activity(activity0324, trcode_var = "TRCODEP")

respondent0324_new <- respondent0324 %>%
  select(TUCASEID, TUYEAR, everything())

merge_respondent <- function(childcare_df, work_df, roster_df, respondent_df){
  
  # Merge childcare with roster info first
  merged <- childcare_df %>%
    left_join(roster_df %>% select(TUCASEID, single_parent, TESEX), by = "TUCASEID") %>%
    left_join(respondent_df %>% select(TUCASEID, TUYEAR, everything()), by = "TUCASEID") %>%
    group_by(TUCASEID, TUYEAR, single_parent, TESEX) %>%  # include TESEX
    summarise(total_childcare = sum(TUACTDUR, na.rm = TRUE), .groups = "drop")
  
  # Summarize work time
  work_summary <- work_df %>%
    group_by(TUCASEID) %>%
    summarise(work_time = sum(TUACTDUR, na.rm = TRUE), .groups = "drop")
  
  # Merge work and recode gender
  merged <- merged %>%
    left_join(work_summary, by = "TUCASEID") %>%
    mutate(Gender = recode(TESEX, `1` = "Male", `2` = "Female"))
  
  return(merged)
}

parents2024 <- merge_respondent(act2024_new$childcare, act2024_new$work, roster24_new, respondent)
parents0324 <- merge_respondent(act0324_new$childcare, act0324_new$work, roster0324_new, respondent0324_new)

demog24 <- cps %>%
  select(TUCASEID, PEEDUCA, GESTFIPS) %>%
  rename(
    EducationAttain = PEEDUCA,
    StateFIPS = GESTFIPS
  ) %>%
  mutate(EducationAttain = case_when(
    EducationAttain < 35 ~ 0,
    EducationAttain >= 35 & EducationAttain <= 39 ~ 1,
    EducationAttain > 39 ~ 2
  ))


demog0324 <- cps0324 %>%
  select(TUCASEID, PEEDUCA, GESTFIPS) %>%
  rename(
    EducationAttain = PEEDUCA,
    StateFIPS = GESTFIPS
  ) %>%
  mutate(EducationAttain = case_when(
    EducationAttain < 35 ~ 0,
    EducationAttain >= 35 & EducationAttain <= 39 ~ 1,
    EducationAttain > 39 ~ 2
  ))

# Merge into parents0324
parents2024 <- parents2024 %>%
  left_join(demog24, by = c("TUCASEID"))
parents0324 <- parents0324 %>%
  left_join(demog0324, by = c("TUCASEID"))



# impute missing data using mean imputation
parents2024 <- parents2024 %>%
  group_by(single_parent, Gender) %>%
  mutate(total_childcare = ifelse(is.na(total_childcare),
                                  mean(total_childcare, na.rm = TRUE),
                                  total_childcare)) %>%
  ungroup()

parents0324 <- parents0324 %>%
  group_by(TUYEAR, single_parent, Gender) %>%
  mutate(total_childcare = ifelse(is.na(total_childcare),
                                  mean(total_childcare, na.rm = TRUE),
                                  total_childcare)) %>%
  ungroup()

### Summarize childcare

# 2024
childcare_summary_2024 <- parents2024 %>%
  group_by(single_parent) %>%
  summarise(
    avg_childcare = mean(total_childcare, na.rm = TRUE),
    median_childcare = median(total_childcare, na.rm = TRUE),
    n = n()
  )

childcare_summary_2024

# Multi-year trend 2003-2024
childcare_trend <- parents0324 %>%
  group_by(TUYEAR, single_parent) %>%
  summarise(
    avg_childcare = mean(total_childcare, na.rm = TRUE),
    median_childcare = median(total_childcare, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

childcare_trend



########################
#####VISUALIZATIONS#####
########################
## Table 1. Sample Characteristics 2024
table1 <- parents2024 %>%
  group_by(single_parent) %>%
  summarise(
    n = n(),
    pct_male = mean(Gender == "Male")*100,
    pct_female = mean(Gender == "Female")*100,
    pct_edu_below_hs = mean(EducationAttain == 0, na.rm = TRUE)*100,
    pct_edu_hs = mean(EducationAttain == 1, na.rm = TRUE)*100,
    pct_edu_above_hs = mean(EducationAttain == 2, na.rm = TRUE)*100,
    avg_work_time = mean(work_time, na.rm = TRUE)
  )
table1

## Table 2. Child Care Summary 2024
table2 <- parents2024 %>%
  group_by(single_parent) %>%
  summarise(
    mean_childcare = mean(total_childcare, na.rm = TRUE),
    median_childcare = median(total_childcare, na.rm = TRUE),
    n = n()
  )
table2

## Figure 1. Child Care Time By Household Type and Gender Boxplot 
ggplot(parents2024, aes(x = single_parent, y = total_childcare, fill = Gender)) +
  geom_boxplot() +
  labs(
    title = "Figure 1. Childcare Time by Household Type and Gender 2024",
    x = "Household Type",
    y = "Total Childcare (minutes per day)"
  ) +
  theme_classic()

## Figure 2. Child Care Time By Household Type and Education Attainment
ggplot(parents2024, aes(x = single_parent, y = total_childcare, fill = factor(EducationAttain))) +
  geom_boxplot() +
  labs(
    title = "Figure 2. Childcare Time by Household Type and Education Attainment 2024",
    x = "Household Type",
    y = "Total Childcare (minutes per day)",
    fill = "Education Level"
  ) +
  theme_classic()



## Figure 4. Trend of Average Childcare Time (2003–2024)
childcare_trend <- parents0324 %>%
  group_by(TUYEAR, single_parent) %>%
  summarise(
    avg_childcare = mean(total_childcare, na.rm = TRUE),
    median_childcare = median(total_childcare, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

ggplot(childcare_trend, aes(x = TUYEAR, y = avg_childcare, color = factor(single_parent))) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c("FALSE" = "blue", "TRUE" = "red"),
    labels = c("Multi-parent", "Single-parent"),
    name = "Household Type"
  ) +
  labs(
    title = "Figure 4. Average Daily Childcare Time (2003–2024)",
    x = "Year",
    y = "Average Childcare Time (minutes)",
    caption = "Data: ATUS 2003-2024"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold")
  )
