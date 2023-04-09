library(FactoMineR)
library(factoextra)
library(httpgd)
library(dplyr)
library(tidyr)

library(ggcorrplot)
library(corrplot)
library(ggrepel)
library(devtools)

df <- read.csv("./Datasets/cleaned_db.csv")
df <- df[, -1]

num_cols <- sapply(df, is.numeric)
df_num <- df[, num_cols]
df_cat <- df[, !num_cols]

df_num_scaled <- as.data.frame(scale(df_num))
df_scaled <- bind_cols(df_num_scaled, df_cat)

variables <- c("X.1000_Damage_to_Any_One_Person.s_Property",
             "Crash_Severity",
             "Person_Injury_Severity",
             "injuries_report", # Cualitative consecuences 4
             "Crash_Death_Count",
             "Crash_Not_Injured_Count",
             "Person_Total_Injury_Count", # Cuantitative consecuences 3
             "At_Intersection_Flag",
             "Construction_Zone_Flag",
             "Day_of_Week",
             "Light_Condition",
             "Manner_of_Collision",
             "Number_of_Entering_Roads",
             "Physical_Feature_1",
             "Road_Class",
             "Roadway_Type",
             "Traffic_Control_Type",
             "Weather_Condition", # Cualitative causes 11
             "Crash_Month",
             "Crash_Time",
             "Median_Width",
             "Speed_Limit", # Cuantitative Causes 4
             "Driver_License_Class",
             "Person_Ethnicity",
             "Person_Gender",
             "Person_Helmet", # Cualitative Person 4
             "Person_Age") # Cuantitative Person 1

df_scaled <- df_scaled[, variables]

res.MFA <- MFA(df_scaled, group = c(4, 3, 11, 4, 4, 1), type = c("n", "c", "n", "c", "n", "c"),
               name.group = c("Cuali_Cons", "Cuanti_Cons", "Cuali_Caus", "Cuanti_Caus", "Cuali_Pers", "Cuani_Pers"))

summary(res.MFA)

res.MFA$eig # We take 5 dimensions, applying the criteria of picking dimensions with eigenvalues > 1


eigen <- res.MFA$eig[1:10, 1]
eigens <- data.frame(Dimension = 1:length(eigen), Eigens = eigen)

ggplot(eigens, aes(x = Dimension, y = Eigens, fill = Dimension)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = round(Eigens, 2)), hjust = +0.5, vjust=-0.4, size = 10) +
  scale_fill_gradient(high = "#F2C9CC", low = "#8B0000") +
  labs(x = "Number of Dimensions", y = "Eigenvalues") +
  ggtitle("Eigensvalues of the 10 first dimensions") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
        axis.title = element_text(size = 25, face = "bold"),
        axis.text = element_text(size = 25),
        legend.position = "none")


inertia <- res.MFA$eig[1:5, 3]
inertias <- data.frame(Dimension = 1:length(inertia), Inertia = inertia)

ggplot(inertias, aes(x = Dimension, y = Inertia, fill = Dimension)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = sprintf("%.1f%%", Inertia)), hjust = +0.5, vjust=-0.4, size = 10) +
  scale_fill_gradient(low = "#F2C9CC", high = "#8B0000") +
  labs(x = "Number of Dimensions", y = "Cumulative Inertia (%)") +
  ggtitle("Cumulative inertia of the 5 first dimensions") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
        axis.title = element_text(size = 25, face = "bold"),
        axis.text = element_text(size = 25),
        legend.position = "none")


plot(res)
plot(res, invisible = "quali")

fviz_mfa(res.MFA, axes = 1:2)
fviz_mfa(res, axes = 3:4)
