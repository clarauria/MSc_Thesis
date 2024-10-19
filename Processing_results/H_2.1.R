
#In order to know which approach would be more adequate for the first hypothesis, I will
# test the 2.1 hypothesis fitting an lmm and a glmm ( poisson and negative binomial) and compare them

library(lme4)
library(lmerTest)
library(car)
library(ggplot2)
library(DHARMa)
library(effects)
library(MASS)
library(readxl)

Plants <- read_excel("C:/Users/PC/Documents/Sustainable Development/Thesis/Results/Part two/LMM.xlsx", sheet = "Plants")

# Scale variables
Plants$CA_scaled <- scale(Plants$CA)
Plants$PercChange_scaled <- scale(Plants$PercChange)

# Check distribution of E
hist(Plants$E, breaks = 20, main = "Distribution of E", xlab = "Number of Endemics")

# Print summary statistics
summary(Plants$E)
var(Plants$E)

# Fit LMM
model_lmm <- lmer(E ~ CA_scaled + PercChange_scaled + (1|Archipelago), data = Plants)

# Fit Poisson GLMM
model_glmm <- glmer(E ~ CA_scaled + PercChange_scaled + (1|Archipelago), family = poisson, data = Plants)

# Fit Negative Binomial GLMM
model_nb <- glmer.nb(E ~ CA_scaled + PercChange_scaled + (1|Archipelago), data = Plants)

# Compare models
AIC(model_lmm, model_glmm, model_nb)
BIC(model_lmm, model_glmm, model_nb)

# Print summaries
summary(model_lmm)
summary(model_glmm)
summary(model_nb)

# Plotting
library(ggeffects)

# Generate predictions from the Negative Binomial GLMM
pred_nb <- ggpredict(model_nb, terms = c("PercChange_scaled [all]", "CA_scaled [mean]"))

# Create the plot for Vascular Plants
ggplot(Plants, aes(x = PercChange, y = E, color = Archipelago)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_line(data = pred_nb, aes(x = x * sd(Plants$PercChange) + mean(Plants$PercChange), y = predicted), 
            color = "black", size = 1) +
  geom_ribbon(data = pred_nb, aes(x = x * sd(Plants$PercChange) + mean(Plants$PercChange), 
                                  y = predicted, ymin = conf.low, ymax = conf.high), 
              alpha = 0.2, inherit.aes = FALSE) +
  geom_text(aes(label = Abbreviation), hjust = 1.5, vjust = 1.5, size = 3) +
  labs(title = "Number of Endemic Vascular Plants vs Percentage Area Change",
       x = "Percentage Area Change",
       y = "Number of Endemic Vascular Plants") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("endemics_vs_percent_change_nb_model.png", width = 10, height = 6)

#The glmm with the negative binomial distribution fits better the data,so it is the one used.

#Now the same for Insects
library(readxl)
Insects <- read_excel("C:/Users/PC/Documents/Sustainable Development/Thesis/Results/Part two/LMM.xlsx", sheet = "Insects")

# Scale variables
Insects$CA_scaled <- scale(Insects$CA)
Insects$PercChange_scaled <- scale(Insects$PercChange)

# Check distribution of E 
hist(Insects$E, breaks = 20, main = "Distribution of Endemic Insects", xlab = "Number of Endemic Insects")

# Print summary statistics
summary(Insects$E)
var(Insects$E)

# Fit LMM
model_lmm <- lmer(E ~ CA_scaled + PercChange_scaled + (1|Archipelago), data = Insects)

# Fit Poisson GLMM
model_glmm <- glmer(E ~ CA_scaled + PercChange_scaled + (1|Archipelago), family = poisson, data = Insects)

# Fit Negative Binomial GLMM
model_nb <- glmer.nb(E ~ CA_scaled + PercChange_scaled + (1|Archipelago), data = Insects)

# Compare models
AIC(model_lmm, model_glmm, model_nb)
BIC(model_lmm, model_glmm, model_nb)

# Print summaries
summary(model_lmm)
summary(model_glmm)
summary(model_nb)

# Generate predictions from the best model 
library(ggeffects)
pred_nb <- ggpredict(model_nb, terms = c("PercChange_scaled [all]", "CA_scaled [mean]"))

# Create the plot
ggplot(Insects, aes(x = PercChange, y = E, color = Archipelago)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_line(data = pred_nb, aes(x = x * sd(Insects$PercChange) + mean(Insects$PercChange), y = predicted), 
            color = "black", size = 1) +
  geom_ribbon(data = pred_nb, aes(x = x * sd(Insects$PercChange) + mean(Insects$PercChange), 
                                  y = predicted, ymin = conf.low, ymax = conf.high), 
              alpha = 0.2, inherit.aes = FALSE) +
  geom_text(aes(label = Abbreviation), hjust = 1.5, vjust = 1.5, size = 3) +
  labs(title = "Number of Endemic Insects vs Percentage Area Change",
       x = "Percentage Area Change",
       y = "Number of Endemic Insects") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("endemic_insects_vs_percent_change_nb_model.png", width = 10, height = 6)

#The glmm with the negative binomial distribution fits better the data, so it is the one used.


#Now for Birds
library(readxl)
Birds <- read_excel("C:/Users/PC/Documents/Sustainable Development/Thesis/Results/Part two/LMM.xlsx", sheet = "Birds")

# Scale variables
Birds$CA_scaled <- scale(Birds$CA)
Birds$PercChange_scaled <- scale(Birds$PercChange)

# Check distribution of E (endemic birds)
hist(Birds$E, breaks = 20, main = "Distribution of Endemic Birds", xlab = "Number of Endemic Birds")

# Print summary statistics
summary(Birds$E)
var(Birds$E)

# Fit LMM
model_lmm <- lmer(E ~ CA_scaled + PercChange_scaled + (1|Archipelago), data = Birds)

# Fit Poisson GLMM
model_glmm <- glmer(E ~ CA_scaled + PercChange_scaled + (1|Archipelago), family = poisson, data = Birds)

# Fit Negative Binomial GLMM
model_nb <- glmer.nb(E ~ CA_scaled + PercChange_scaled + (1|Archipelago), data = Birds)

# Compare models
AIC(model_lmm, model_glmm, model_nb)
BIC(model_lmm, model_glmm, model_nb)

# Print summaries
summary(model_lmm)
summary(model_glmm)
summary(model_nb)

# Generate predictions from the best model 
library(ggeffects)
pred_lmm <- ggpredict(model_lmm, terms = c("PercChange_scaled [all]", "CA_scaled [mean]"))

# Create the plot
ggplot(Birds, aes(x = PercChange, y = E, color = Archipelago)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_line(data = pred_lmm, aes(x = x * sd(Birds$PercChange) + mean(Birds$PercChange), y = predicted), 
            color = "black", size = 1) +
  geom_ribbon(data = pred_lmm, aes(x = x * sd(Birds$PercChange) + mean(Birds$PercChange), 
                                  y = predicted, ymin = conf.low, ymax = conf.high), 
              alpha = 0.2, inherit.aes = FALSE) +
  geom_text(aes(label = Abbreviation), hjust = 1.5, vjust = 1.5, size = 3) +
  labs(title = "Number of Endemic Birds vs Percentage Area Change",
       x = "Percentage Area Change",
       y = "Number of Endemic Birds") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("endemic_birds_vs_percent_change_nb_model.png", width = 10, height = 6)

#The lmm was the one that fits better the data. 

#Which variables have an impact on the number of endemics

Plants <- read_excel("C:/Users/PC/Documents/Sustainable Development/Thesis/Results/Part two/LMM.xlsx", sheet = "Plants")
Insects <- read_excel("C:/Users/PC/Documents/Sustainable Development/Thesis/Results/Part two/LMM.xlsx", sheet = "Insects")

library(lme4)
library(MuMIn)
library(ggplot2)
library(car)  # For vif function

# Function to check for collinearity
check_collinearity <- function(model) {
  if (length(fixef(model)) < 3) {  # Intercept + at least 2 predictors
    return(TRUE)  # Models with fewer than 2 predictors are considered non-collinear
  }
  tryCatch({
    vif_values <- vif(model)
    return(all(vif_values < 5))  # Using 5 as a threshold
  }, error = function(e) {
    return(TRUE)  # If VIF calculation fails, assume non-collinear
  })
}

# Function to run model selection
run_model_selection <- function(data) {
  # Ensure all variables are scaled
  data$CA_scaled <- scale(data$CA)
  data$PA_scaled <- scale(data$PA)
  data$N_scaled <- scale(data$N)
  data$NHL_scaled <- scale(data$NHL)
  data$HHL_scaled <- scale(data$HHL)
  data$FNHL_scaled <- scale(data$FNHL)
  data$PercChange_scaled <- scale(data$PercChange) 
  
  # Check for any missing values
  print(colSums(is.na(data[, c("CA_scaled", "PA_scaled", "N_scaled", "NHL_scaled", "HHL_scaled", "FNHL_scaled", "E", "PercChange_scaled")])))
  
  # Create the full model
  full_model <- glmer.nb(E ~ CA_scaled + PA_scaled + N_scaled + NHL_scaled + HHL_scaled + FNHL_scaled + PercChange_scaled + (1|Archipelago), 
                         data = data, na.action = na.fail)
  
  # Generate all possible combinations of variables
  options(na.action = "na.fail")  # This ensures dredge will fail if there are any NA values
  model_combinations <- dredge(full_model, rank = "AICc")
  
  # Filter out models with collinear variables
  non_collinear_models <- model_combinations[sapply(1:nrow(model_combinations), function(i) {
    model <- get.models(model_combinations, i)[[1]]
    check_collinearity(model)
  }), ]
  
  # Get the best model
  best_model <- get.models(non_collinear_models, 1)[[1]]
  
  # Calculate variable importance
  var_importance <- sw(non_collinear_models)
  
  return(list(model_combinations = non_collinear_models,
              best_model = best_model,
              importance = var_importance))
}

# Run model selection for Vascular Plants and Insects
vascular_plants_models <- run_model_selection(Plants)
insect_models <- run_model_selection(Insects)

# Function to plot variable importance
plot_importance <- function(importance, title) {
  importance_df <- data.frame(
    Variable = names(importance),
    Importance = as.numeric(importance)
  )
  importance_df <- importance_df[order(-importance_df$Importance),]
  
  ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    coord_flip() +
    theme_minimal() +
    labs(title = title, x = "Variables", y = "Importance")
}

# Plot variable importance for each group
vascular_plants_plot <- plot_importance(vascular_plants_models$importance, "Variable Importance - Vascular Plants")
insect_plot <- plot_importance(insect_models$importance, "Variable Importance - Insects")

# Print plots
print(vascular_plants_plot)
print(insect_plot)

# Print summaries
print("Vascular Plants - Best Model:")
print(summary(vascular_plants_models$best_model))
print("Variable Importance:")
print(vascular_plants_models$importance)

print("Insects - Best Model:")
print(summary(insect_models$best_model))
print("Variable Importance:")
print(insect_models$importance)

# Print top models
print("Top models for Vascular Plants:")
print(vascular_plants_models$model_combinations)


print("Top models for Insects:")
print(insect_models$model_combinations)


#Variable importance for Birds
Birds <- read_excel("C:/Users/PC/Documents/Sustainable Development/Thesis/Results/Part two/LMM.xlsx", sheet = "Birds")
library(lme4)
library(MuMIn)
library(ggplot2)
library(car)  # For vif function

# Function to check for collinearity
check_collinearity <- function(model) {
  if (length(fixef(model)) < 3) {  # Intercept + at least 2 predictors
    return(TRUE)  # Models with fewer than 2 predictors are considered non-collinear
  }
  tryCatch({
    vif_values <- vif(model)
    return(all(vif_values < 5))  # Using 5 as a threshold
  }, error = function(e) {
    return(TRUE)  # If VIF calculation fails, assume non-collinear
  })
}

# Function to run model selection for birds (using LMM)
run_model_selection_birds <- function(data) {
  # Ensure all variables are scaled
  data$CA_scaled <- scale(data$CA)
  data$PA_scaled <- scale(data$PA)
  data$N_scaled <- scale(data$N)
  data$NHL_scaled <- scale(data$NHL)
  data$HHL_scaled <- scale(data$HHL)
  data$FNHL_scaled <- scale(data$FNHL)
  data$PercChange_scaled <- scale(data$PercChange)
  # Check for any missing values
  print(colSums(is.na(data[, c("CA_scaled", "PA_scaled", "N_scaled", "NHL_scaled", "HHL_scaled", "FNHL_scaled", "E","PercChange_scaled")])))
  
  # Create the full model (LMM for birds)
  full_model <- lmer(E ~ CA_scaled + PA_scaled + N_scaled + NHL_scaled + HHL_scaled + FNHL_scaled + PercChange_scaled + (1|Archipelago), 
                     data = data, na.action = na.fail)
  
  # Generate all possible combinations of variables
  options(na.action = "na.fail")  # This ensures dredge will fail if there are any NA values
  model_combinations <- dredge(full_model, rank = "AICc")
  
  # Filter out models with collinear variables
  non_collinear_models <- model_combinations[sapply(1:nrow(model_combinations), function(i) {
    model <- get.models(model_combinations, i)[[1]]
    check_collinearity(model)
  }), ]
  
  # Get the best model
  best_model <- get.models(non_collinear_models, 1)[[1]]
  
  # Calculate variable importance
  var_importance <- sw(non_collinear_models)
  
  return(list(model_combinations = non_collinear_models,
              best_model = best_model,
              importance = var_importance))
}

# Run model selection for Birds
bird_models <- run_model_selection_birds(Birds)

# Function to plot variable importance
plot_importance <- function(importance, title) {
  importance_df <- data.frame(
    Variable = names(importance),
    Importance = as.numeric(importance)
  )
  importance_df <- importance_df[order(-importance_df$Importance),]
  
  ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    coord_flip() +
    theme_minimal() +
    labs(title = title, x = "Variables", y = "Importance")
}

# Plot variable importance for birds
bird_plot <- plot_importance(bird_models$importance, "Variable Importance - Birds")

# Print plot
print(bird_plot)

# Print summary
print("Birds - Best Model:")
print(summary(bird_models$best_model))
print("Variable Importance:")
print(bird_models$importance)

# Print top models
print("Top models for Birds:")
print(bird_models$model_combinations)

