##### DS340 Project #####

library(tidyverse)
library(data.table)
library(viridis)
library(sf)
library(maps)
library(usdm)
library(randomForest)
library(knitr)
library(gridExtra)
library(grid)

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
      has_child = any(TERRP == 22 & TEAGE < 18),
      has_partner = any(TERRP %in% c(20,21)),
      num_children = sum(TERRP == 22 & TEAGE < 18)   
    ) %>%
    ungroup() %>%
    filter(TERRP %in% c(18,19)) %>%
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
  
  merged <- childcare_df %>%
    left_join(roster_df %>% 
                select(TUCASEID, single_parent, TESEX, num_children), 
              by = "TUCASEID") %>%
    left_join(respondent_df %>% select(TUCASEID, TUYEAR, everything()), 
              by = "TUCASEID") %>%
    group_by(TUCASEID, TUYEAR, single_parent, TESEX, num_children) %>%  
    summarise(total_childcare = sum(TUACTDUR, na.rm = TRUE), .groups = "drop")
  
  work_summary <- work_df %>%
    group_by(TUCASEID) %>%
    summarise(work_time = sum(TUACTDUR, na.rm = TRUE), .groups = "drop")
  
  merged <- merged %>%
    left_join(work_summary, by = "TUCASEID") %>%
    mutate(Gender = recode(TESEX, `1` = "Male", `2` = "Female"))
  
  return(merged)
}

parents2024 <- merge_respondent(act2024_new$childcare, act2024_new$work, roster24_new, respondent)
parents0324 <- merge_respondent(act0324_new$childcare, act0324_new$work, roster0324_new, respondent0324_new)

parents2024 <- parents2024 %>%
  mutate(
    num_children = ifelse(num_children == 0 | is.na(num_children), NA, num_children),
    childcare_per_child = total_childcare / num_children
  )

parents0324 <- parents0324 %>%
  mutate(
    num_children = ifelse(num_children == 0 | is.na(num_children), NA, num_children),
    childcare_per_child = total_childcare / num_children
  )

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



# replace missing data with 0
parents2024 <- parents2024 %>%
  mutate(
    total_childcare = ifelse(is.na(total_childcare), 0, total_childcare),
    childcare_per_child = ifelse(is.na(childcare_per_child), 0, childcare_per_child),
    work_time = ifelse(is.na(work_time), 0, work_time)
  )

parents0324 <- parents0324 %>%
  mutate(
    total_childcare = ifelse(is.na(total_childcare), 0, total_childcare),
    childcare_per_child = ifelse(is.na(childcare_per_child), 0, childcare_per_child),
    work_time = ifelse(is.na(work_time), 0, work_time)
  )

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

#per child
childcare_per_child_summary_2024 <- parents2024 %>%
  group_by(single_parent) %>%
  summarise(
    avg_childcare = mean(childcare_per_child, na.rm = TRUE),
    median_childcare = median(childcare_per_child, na.rm = TRUE),
    n = n()
  )
childcare_per_child_summary_2024

# Multi-year trend 2003-2024
childcare_trend <- parents0324 %>%
  group_by(TUYEAR, single_parent) %>%
  summarise(
    avg_childcare = mean(childcare_per_child, na.rm = TRUE),
    median_childcare = median(childcare_per_child, na.rm = TRUE),
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
ggplot(parents2024, aes(x = Gender, y = childcare_per_child, fill = single_parent)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 250)) +
  scale_fill_manual(values = c("gold", "lightblue")) +
  labs(
    title = "Figure 1. Childcare Time by Household Type and Gender 2024\n",
    x = "Gender",
    y = "Total Childcare (minutes per day)",
    fill = "Household Type"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 17, face = "bold")
  )
ggsave("figure1.png", width = 8, height = 6)

## Figure 2. Child Care Time By Household Type and Education Attainment
ggplot(parents2024, aes(x = factor(EducationAttain), y = childcare_per_child, fill = single_parent)) +
  geom_boxplot() +
  scale_fill_manual(values = c("gold", "lightblue")) +
  coord_cartesian(ylim = c(0, 300)) +
  labs(
    title = "Figure 2. Childcare Time by Household Type and \nEducation Attainment 2024\n",
    x = "Education Attainment",
    y = "Total Childcare (minutes per day)",
    fill = "Household Type"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 17, face = "bold")
  )
ggsave("figure2.png", width = 8, height = 6)


## Figure 3.1
state_summary <- parents2024 %>%
  group_by(StateFIPS, single_parent) %>%
  summarise(
    avg_childcare = mean(childcare_per_child, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(single_parent)

us_states <- map_data("state")

fips_lookup <- as.data.frame(state.fips) %>%
  mutate(region = gsub(":.*", "", polyname)) %>%
  distinct(fips, region)

state_summary_map <- state_summary %>%
  left_join(fips_lookup, by = c("StateFIPS" = "fips")) %>%
  left_join(us_states, by = "region")

ggplot(state_summary_map, aes(x = long, y = lat, group = group, fill = avg_childcare)) +
  geom_polygon(color = "white") +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Avg Childcare\n(min/day/child)") +
  labs(
    title = "Figure 3.1. Average Childcare Time by State (Single Parents) 2024",
    x = "", y = ""
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()
  )
ggsave("figure3.1.png", width = 8, height = 6)
# 3.2
state_summary2 <- parents2024 %>%
  group_by(StateFIPS, single_parent) %>%
  summarise(
    avg_childcare = mean(childcare_per_child, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!single_parent)

state_summary_map2 <- state_summary2 %>%
  left_join(fips_lookup, by = c("StateFIPS" = "fips")) %>%
  left_join(us_states, by = "region")

ggplot(state_summary_map2, aes(x = long, y = lat, group = group, fill = avg_childcare)) +
  geom_polygon(color = "white") +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Avg Childcare\n(min/day/child)") +
  labs(
    title = "Figure 3.2. Average Childcare Time by State (Multi-Parents) 2024",
    x = "", y = ""
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()
  )
ggsave("figure3.2.png", width = 8, height = 6)



## Figure 4. Trend of Average Childcare Time (2003–2024)
childcare_trend <- parents0324 %>%
  group_by(TUYEAR, single_parent) %>%
  summarise(
    avg_childcare = mean(childcare_per_child, na.rm = TRUE),
    median_childcare = median(childcare_per_child, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

ggplot(childcare_trend, aes(x = TUYEAR, y = avg_childcare, color = factor(single_parent))) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  coord_cartesian(ylim = c(0, 80)) +
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
  theme_classic(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold")
  )
ggsave("figure4.png", width = 10, height = 4)

######################
#######MODELING#######
######################
# Convert categorical variables to factors
parents2024 <- parents2024 %>%
  select(-TUCASEID,-TUYEAR) %>%
  mutate(
    single_parent = factor(single_parent, labels = c("Multi-parent", "Single-parent")),
    Gender = factor(Gender),
    EducationAttain = factor(EducationAttain, labels = c("Below_HS", "HS", "Above_HS")),
    Region = factor(StateFIPS)
  ) %>%
  select(-StateFIPS)
  

#VIF step test for multicollinearity

xvar <- model.matrix(childcare_per_child ~ ., 
                     data = parents2024)[,-1]
vifstep(x = xvar, th = 5) # TESEX identified with collinearity problem

### Linear Regression Model ###

#test/train split
set.seed(340)
train_index <- sample(seq_len(nrow(parents2024)), size = 0.8 * nrow(parents2024))
train_data <- parents2024[train_index, ]
test_data <- parents2024[-train_index, ]

model1 <- lm(childcare_per_child ~ single_parent + work_time + EducationAttain, data = train_data)
summary(model1)

coef_df <- as.data.frame(coef(summary(model1)))
ci <- confint(model1)

coef_table <- data.frame(
  Variable = rownames(coef_df),
  Estimate = coef_df[, "Estimate"],
  Lower_95_CI = ci[,1],
  Upper_95_CI = ci[,2],
  row.names = NULL
)

# Rename variables to readable labels
coef_table$Variable <- recode(coef_table$Variable,
                              "(Intercept)" = "Intercept",
                              "single_parentSingle-parent" = "Single Parent (vs Multi-parent)",
                              "work_time" = "Work Time (minutes)",
                              "EducationAttainHS" = "High School (vs Below HS)",
                              "EducationAttainAbove_HS" = "Above High School (vs Below HS)"
)

# Round nicely
coef_table[, -1] <- round(coef_table[, -1], 3)

coef_table
png("model1_coefficients.png", width = 1400, height = 400, res = 200)

grid.table(coef_table)

dev.off()

#k-fold CV
k <- 5
folds <- cut(seq(1, nrow(parents2024)), breaks = k, labels = FALSE)
cv_results <- numeric(k)

for(i in 1:k){
  test_fold <- which(folds == i)
  train_fold <- setdiff(seq_len(nrow(parents2024)), test_fold)
  
  train_cv <- parents2024[train_fold, ]
  test_cv <- parents2024[test_fold, ]
  
  fit <- lm(childcare_per_child ~ single_parent + work_time + EducationAttain, data = train_cv)
  preds <- predict(fit, newdata = test_cv)
  
  cv_results[i] <- mean((test_cv$childcare_per_child - preds)^2)  # MSE
}

mean(cv_results)

# Residual plot
# plot(model1, which=1)  # Residuals vs Fitted
plot(model1, which=2)  # QQ plot

# Predicted childcare by household type
test_data$pred_lm <- predict(model1, newdata = test_data)

# Residuals
ggplot(test_data, aes(x = pred_lm, y = childcare_per_child - pred_lm)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype="dashed", color="red") +
  labs(title="Linear Regression Residuals\n", x="Predicted", y="Residuals") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 17, face = "bold")
  )
ggsave("resid1.png", width = 8, height = 6)



### Random Forest Model ###

# Train Random Forest on 2024 train data
set.seed(340)
rf_model <- randomForest(
  childcare_per_child ~ single_parent + work_time + Gender + EducationAttain + Region,
  data = train_data,
  ntree = 500,
  importance = TRUE
)

# Predictions on test set
test_data$pred_rf <- predict(rf_model, newdata = test_data)

#k-fold CV
k <- 5
folds <- cut(seq(1, nrow(parents2024)), breaks = k, labels = FALSE)
rf_cv_mse <- numeric(k)

for(i in 1:k){
  test_fold <- which(folds == i)
  train_fold <- setdiff(seq_len(nrow(parents2024)), test_fold)
  
  train_cv <- parents2024[train_fold, ]
  test_cv <- parents2024[test_fold, ]
  
  rf_cv <- randomForest(childcare_per_child ~ single_parent + work_time + Gender + EducationAttain + Region,
                        data = train_cv, ntree = 300)
  
  preds <- predict(rf_cv, newdata = test_cv)
  rf_cv_mse[i] <- mean((test_cv$childcare_per_child - preds)^2)
}

mean(rf_cv_mse)

# Residuals
ggplot(test_data, aes(x = pred_rf, y = childcare_per_child - pred_rf)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype="dashed", color="red") +
  labs(title="Random Forest Residuals\n", x="Predicted", y="Residuals") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 17, face = "bold")
  )
ggsave("resid2.png", width = 8, height = 6)


# Feature importance plot
png("varimp.png", width = 1500, height = 1200, res = 200)
varImpPlot(rf_model,main="")
title(main = "Feature Importance Plot", cex.main = 1.5)
dev.off()

importance_df <- as.data.frame(importance(rf_model))
importance_df$Variable <- rownames(importance_df)
rownames(importance_df)[1] <- "Parental Status"
rownames(importance_df)[2] <- "Work Time (minutes)"
rownames(importance_df)[3] <- "Gender"
rownames(importance_df)[4] <- "Education Attainment"
rownames(importance_df)[5] <- "Region"


ggplot(importance_df, aes(x = reorder(Variable, IncMSE), y = IncMSE)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Figure 4. Feature Importance Plot\n",
    x = "Variable",
    y = "% Increase in MSE"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold")
  )

ggsave("varimp_bar.png", width = 8, height = 6)


# Linear Regression RMSE
lm_rmse <- sqrt(mean((test_data$childcare_per_child - test_data$pred_lm)^2))
lm_rmse
# Random Forest RMSE
rf_rmse <- sqrt(mean((test_data$childcare_per_child - test_data$pred_rf)^2))
rf_rmse
