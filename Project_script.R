##### DS340 Project #####

library(tidyverse)
library(data.table)

### Load data
activity <- read.csv("data/atusact_2024.dat")
respondent <- read.csv("data/atusresp_2024.dat")
roster <- read.csv("data/atusrost_2024.dat")
who <- read.csv("data/atuswho_2024.dat")
cps <- read.csv("data/atuscps_2024.dat")


### Single Parent Identification

# For each household, flag if the household has their own children
# TERRP == 22
roster <- roster %>%
  group_by(TUCASEID) %>%
  mutate(
    has_child = any(TERRP == 22 & TEAGE < 18),
    has_partner = any(TERRP %in% c(20,21))  # spouse or unmarried partner
  ) %>%
  ungroup()

# Identify single parent: has own children AND no partner
# Keep respondent rows (TERRP 18 and 19)
roster <- roster %>%
  filter(TERRP %in% c(18,19)) %>%
  mutate(single_parent = has_child & !has_partner)

table(roster$single_parent) #311 single parents identified

### Childcare activities
childcare <- activity %>%
  filter(str_starts(as.character(TRCODE), "301") |
           str_starts(as.character(TRCODE), "302") |
           str_starts(as.character(TRCODE), "303"))

### Work activities
work <- activity %>%
  filter(str_starts(as.character(TRCODE), "5"))
           
### Merge respondent info with childcare
resp_all <- roster %>%
  left_join(respondent, by = "TUCASEID")

parents <- childcare %>%
  left_join(resp_all, by = "TUCASEID") %>%
  # total childcare time per respondent
  group_by(TUCASEID) %>%
  mutate(total_childcare = sum(TUACTDUR, na.rm = TRUE)) %>%
  ungroup() 

work_summary <- work %>%
  group_by(TUCASEID) %>%
  summarise(work_time = sum(TUACTDUR, na.rm = TRUE), .groups = "drop")

parents <- parents %>%
  left_join(work_summary, by = "TUCASEID") %>%
  mutate(
    Gender = recode(TESEX, `1` = "Male", `2` = "Female")
  )

### Average childcare by single vs multi-parent
childcare_summary <- parents %>%
  group_by(single_parent) %>%
  summarise(avg_childcare = mean(TUACTDUR24, na.rm = TRUE),
            median_childcare = median(TUACTDUR24, na.rm = TRUE),
            n = n())

childcare_summary

### Visualizations

ggplot(childcare_summary, aes(x = single_parent, y = avg_childcare, fill = single_parent)) +
  geom_col() +
  geom_text(aes(label = round(avg_childcare,1)), vjust = -0.5) +
  scale_x_discrete(labels = c("FALSE" = "Multi-Parent", "TRUE" = "Single Parent")) +
  scale_fill_manual(values = c("FALSE" = "turquoise", "TRUE" = "gold")) +
  labs(x = "Household Type",
       y = "Average Minutes of Childcare",
       title = "Average Daily Childcare Time: Single vs Multi-Parent") +
  theme_classic() +
  theme(legend.position = "none")


ggplot(parents, aes(x = Gender, y = total_childcare, fill = single_parent)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("FALSE" = "turquoise", "TRUE" = "gold"),
                    labels = c("Multi-Parent", "Single Parent")) +
  labs(x = "Gender",
       y = "Total Childcare (minutes)",
       fill = "Household Type",
       title = "Childcare Time by Gender and Household Type") +
  theme_classic()
