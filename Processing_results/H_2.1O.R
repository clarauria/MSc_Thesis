library(readxl)
library(lme4)
library(ggplot2)
library(DHARMa)
library(effects)
library(MASS)
library(ggeffects)

# Load the data for Plants
LMM <- read_excel("C:/Users/PC/Documents/Sustainable Development/Thesis/Results/Part two/LMM.xlsx", sheet = "Plants")

# Calculate area difference and percentage change for Plants
LMM$Area_Diff <- LMM$PA - LMM$CA
LMM$Percent_Change <- (LMM$Area_Diff / LMM$PA) * 100

# Scale variables for Plants
LMM$CA_scaled <- scale(LMM$CA)  # Scaling CA
LMM$Percent_Change_scaled <- scale(LMM$Percent_Change)

# Fit Negative Binomial GLMM for Plants including CA_scaled
model_nb_plants <- glmer.nb(EO ~ CA_scaled + Percent_Change_scaled + (1|Archipelago), data = LMM)

# Print summary of the model for Plants
print(summary(model_nb_plants))

# Generate predictions from the Negative Binomial GLMM for Plants
pred_nb_plants <- ggpredict(model_nb_plants, terms = c("CA_scaled [all]", "Percent_Change_scaled [all]"))

# Create the plot for Plants
LMM$row_number <- 1:nrow(LMM)  # Add row numbers to the dataframe
ggplot(LMM, aes(x = Percent_Change, y = EO, color = Archipelago)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_line(data = pred_nb_plants, 
            aes(x = x * sd(LMM$Percent_Change) + mean(LMM$Percent_Change), y = predicted), 
            color = "black", linewidth = 1) +  # Changed size to linewidth
  geom_ribbon(data = pred_nb_plants, 
              aes(x = x * sd(LMM$Percent_Change) + mean(LMM$Percent_Change), 
                  y = predicted, ymin = conf.low, ymax = conf.high), 
              alpha = 0.2, inherit.aes = FALSE) +
  geom_text(aes(label = row_number), hjust = 1.5, vjust = 1.5, size = 3) +
  labs(title = "Number of Single-Island Endemics (Plants) vs Percentage Area Change",
       x = "Percentage Area Change",
       y = "Number of Single-Island Endemics (Plants)") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Load the data for Insects
Insects <- read_excel("C:/Users/PC/Documents/Sustainable Development/Thesis/Results/Part two/LMM.xlsx", sheet = "Insects")

# Calculate area difference and percentage change for Insects
Insects$Area_Diff <- Insects$PA - Insects$CA
Insects$Percent_Change <- (Insects$Area_Diff / Insects$PA) * 100

# Scale variables for Insects
Insects$CA_scaled <- scale(Insects$CA)  # Scaling CA
Insects$Percent_Change_scaled <- scale(Insects$Percent_Change)

# Convert to numeric if necessary
Insects$Percent_Change_scaled <- as.numeric(Insects$Percent_Change_scaled)

# Fit Negative Binomial GLMM for Insects including CA_scaled
model_nb_insects <- glmer.nb(EO ~ CA_scaled + Percent_Change_scaled + (1|Archipelago), data = Insects)

# Print summary of the model for Insects
print(summary(model_nb_insects))

# Generate predictions from the Negative Binomial GLMM for Insects
pred_nb_insects <- ggpredict(model_nb_insects, terms = c("CA_scaled [all]", "Percent_Change_scaled [all]"))

# Create the plot for Insects
Insects$row_number <- 1:nrow(Insects)  # Add row numbers to the dataframe
ggplot(Insects, aes(x = Percent_Change, y = EO, color = Archipelago)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_line(data = pred_nb_insects, 
            aes(x = x * sd(Insects$Percent_Change) + mean(Insects$Percent_Change), 
                y = predicted), 
            color = "black", linewidth = 1) +  # Changed size to linewidth
  geom_ribbon(data = pred_nb_insects, 
              aes(x = x * sd(Insects$Percent_Change) + mean(Insects$Percent_Change), 
                  y = predicted, ymin = conf.low, ymax = conf.high), 
              alpha = 0.2, inherit.aes = FALSE) +
  geom_text(aes(label = row_number), hjust = 1.5, vjust = 1.5, size = 3) +
  labs(title = "Number of Single-Island Endemics (Insects) vs Percentage Area Change",
       x = "Percentage Area Change",
       y = "Number of Single-Island Endemics (Insects)") +
  theme_minimal() +
  theme(legend.position = "bottom")