#For Insects
library(readxl)
library(ggplot2)
library(dplyr)
library(lme4)
library(car)
library(MuMIn)
library(ggeffects)

# Read the data
Insects <- read_excel("C:/Users/PC/Documents/Sustainable Development/Thesis/Results/Part two/LMM.xlsx", sheet = "Insects")

# Calculate the difference between past and present island areas
Insects$Area_Difference <- Insects$PA - Insects$CA

# Calculate the percentage change in area
Insects$Area_Percent_Change <- (Insects$Area_Difference / Insects$PA) * 100

# Calculate the proportion of endangered endemics, replacing NA with 0
Insects$Prop_Endangered <- ifelse(is.na(Insects$AssEE / Insects$AssE), 0, Insects$AssEE / Insects$AssE)

# Scale the Area_Percent_Change variable
Insects$Area_Percent_Change_scaled <- scale(Insects$Area_Percent_Change)

# Prepare data for analysis (excluding zeros)
Insects_analysis <- Insects[Insects$Prop_Endangered > 0, ]

# Fit the GLMM (using only non-zero values for analysis)
glmm_model <- glmer(cbind(AssEE, AssE - AssEE) ~ Area_Percent_Change_scaled + CA + (1|Archipelago), 
                    family = binomial, data = Insects_analysis)


# Generate predictions from the GLMM
pred_glmm <- ggpredict(glmm_model, terms = c("Area_Percent_Change_scaled [all]"))

# Create the plot
plot <- ggplot(Insects, aes(x = Area_Percent_Change, y = Prop_Endangered, color = Archipelago)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_line(data = pred_glmm, aes(x = x * sd(Insects_analysis$Area_Percent_Change) + mean(Insects_analysis$Area_Percent_Change), y = predicted), 
            color = "black", size = 1) +
  geom_ribbon(data = pred_glmm, aes(x = x * sd(Insects_analysis$Area_Percent_Change) + mean(Insects_analysis$Area_Percent_Change), 
                                    y = predicted, ymin = conf.low, ymax = conf.high), 
              alpha = 0.2, inherit.aes = FALSE) +
  geom_text(aes(label = Abbreviation), hjust = -0.5, vjust = 1.5, size = 3) +
  labs(title = "Proportion of Endangered Endemic Insects vs Percent Change in Island Area",
       x = "Percent Change in Island Area",
       y = "Proportion of Endangered Endemic Insects") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1))

# Print the plot
print(plot)
# Print the summary of the model
print(summary(glmm_model))

# Check for overdispersion
overdisp <- sum(residuals(glmm_model, type = "pearson")^2) / df.residual(glmm_model)
print(paste("Overdispersion ratio:", overdisp))

# If overdispersion is present (ratio > 1.5), consider using a quasi-binomial family
if(overdisp > 1.5) {
  print("Overdispersion detected. Consider using a quasi-binomial model.")
}

# Calculate R-squared (Nakagawa & Schielzeth's R-squared for GLMM)
r2 <- r.squaredGLMM(glmm_model)
print(paste("Marginal R-squared:", r2[1]))
print(paste("Conditional R-squared:", r2[2]))

# Calculate and print the correlation (excluding zero values)
correlation <- cor.test(Insects_analysis$Area_Percent_Change, Insects_analysis$Prop_Endangered)
print(correlation)

#Plants
library(readxl)
library(ggplot2)
library(dplyr)
library(lme4)
library(car)
library(MuMIn)
library(ggeffects)

# Read the data
Plants <- read_excel("C:/Users/PC/Documents/Sustainable Development/Thesis/Results/Part two/LMM.xlsx", sheet = "Plants")

# Calculate the difference between past and present island areas
Plants$Area_Difference <- Plants$PA - Plants$CA

# Calculate the percentage change in area
Plants$Area_Percent_Change <- (Plants$Area_Difference / Plants$PA) * 100

# Calculate the proportion of endangered endemics, replacing NA with 0
Plants$Prop_Endangered <- ifelse(is.na(Plants$AssEE / Plants$AssE), 0, Plants$AssEE / Plants$AssE)

# Scale the Area_Percent_Change variable
Plants$Area_Percent_Change_scaled <- scale(Plants$Area_Percent_Change)

# Prepare data for analysis (excluding zeros)
Plants_analysis <- Plants[Plants$Prop_Endangered > 0, ]

# Fit the GLMM (using only non-zero values for analysis)
glmm_model <- glmer(cbind(AssEE, AssE - AssEE) ~ Area_Percent_Change_scaled + CA + (1|Archipelago), 
                    family = binomial, data = Plants_analysis)

# Generate predictions from the GLMM
pred_glmm <- ggpredict(glmm_model, terms = c("Area_Percent_Change_scaled [all]"))

# Create the plot
plot <- ggplot(Plants, aes(x = Area_Percent_Change, y = Prop_Endangered, color = Archipelago)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_line(data = pred_glmm, aes(x = x * sd(Plants_analysis$Area_Percent_Change) + mean(Plants_analysis$Area_Percent_Change), y = predicted), 
            color = "black", size = 1) +
  geom_ribbon(data = pred_glmm, aes(x = x * sd(Plants_analysis$Area_Percent_Change) + mean(Plants_analysis$Area_Percent_Change), 
                                    y = predicted, ymin = conf.low, ymax = conf.high), 
              alpha = 0.2, inherit.aes = FALSE) +
  geom_text(aes(label = Abbreviation), hjust = -0.5, vjust = 1.5, size = 3) +
  labs(title = "Proportion of Endangered Endemic Plants vs Percent Change in Island Area ",
       x = "Percent Change in Island Area ",
       y = "Proportion of Endangered Endemic Plants") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1))

# Print the plot
print(plot)

# Print the summary of the model
print(summary(glmm_model))

# Check for overdispersion
overdisp <- sum(residuals(glmm_model, type = "pearson")^2) / df.residual(glmm_model)
print(paste("Overdispersion ratio:", overdisp))

# Calculate R-squared (Nakagawa & Schielzeth's R-squared for GLMM)
r2 <- r.squaredGLMM(glmm_model)
print(paste("Marginal R-squared:", r2[1]))
print(paste("Conditional R-squared:", r2[2]))

# Calculate and print the correlation (excluding zero values)
correlation <- cor.test(Plants_analysis$Area_Percent_Change, Plants_analysis$Prop_Endangered)
print(correlation)

#Birds
library(readxl)
library(ggplot2)
library(dplyr)
library(lme4)
library(car)
library(MuMIn)
library(ggeffects)

# Read the data
Birds <- read_excel("C:/Users/PC/Documents/Sustainable Development/Thesis/Results/Part two/LMM.xlsx", sheet = "Birds")

# Calculate the difference between past and present island areas
Birds$Area_Difference <- Birds$PA - Birds$CA

# Calculate the percentage change in area
Birds$Area_Percent_Change <- (Birds$Area_Difference / Birds$PA) * 100

# Calculate the proportion of endangered endemics, replacing NA with 0
Birds$Prop_Endangered <- ifelse(is.na(Birds$AssEE / Birds$AssE), 0, Birds$AssEE / Birds$AssE)

# Scale the Area_Percent_Change variable
Birds$Area_Percent_Change_scaled <- scale(Birds$Area_Percent_Change)

# Prepare data for analysis (excluding zeros)
Birds_analysis <- Birds[Birds$Prop_Endangered > 0, ]

# Fit the GLMM (using only non-zero values for analysis)
glmm_model <- glmer(cbind(AssEE, AssE - AssEE) ~ Area_Percent_Change_scaled + CA + (1|Archipelago), 
                    family = binomial, data = Birds_analysis)

# Generate predictions from the GLMM
pred_glmm <- ggpredict(glmm_model, terms = c("Area_Percent_Change_scaled [all]"))

# Create the plot
plot <- ggplot(Birds, aes(x = Area_Percent_Change, y = Prop_Endangered, color = Archipelago)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_line(data = pred_glmm, aes(x = x * sd(Birds_analysis$Area_Percent_Change) + mean(Birds_analysis$Area_Percent_Change), y = predicted), 
            color = "black", size = 1) +
  geom_ribbon(data = pred_glmm, aes(x = x * sd(Birds_analysis$Area_Percent_Change) + mean(Birds_analysis$Area_Percent_Change), 
                                    y = predicted, ymin = conf.low, ymax = conf.high), 
              alpha = 0.2, inherit.aes = FALSE) +
  geom_text(aes(label = Abbreviation), hjust = -0.5, vjust = 1.5, size = 3) +
  labs(title = "Proportion of Endangered Endemic Birds vs Percent Change in Island Area",
       x = "Percent Change in Island Area",
       y = "Proportion of Endangered Endemic Birds") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1))

# Print the plot
print(plot)

# Print the summary of the model
print(summary(glmm_model))

# Check for overdispersion
overdisp <- sum(residuals(glmm_model, type = "pearson")^2) / df.residual(glmm_model)
print(paste("Overdispersion ratio:", overdisp))

# Calculate R-squared (Nakagawa & Schielzeth's R-squared for GLMM)
r2 <- r.squaredGLMM(glmm_model)
print(paste("Marginal R-squared:", r2[1]))
print(paste("Conditional R-squared:", r2[2]))

# Calculate and print the correlation (excluding zero values)
correlation <- cor.test(Birds_analysis$Area_Percent_Change, Birds_analysis$Prop_Endangered)
print(correlation)