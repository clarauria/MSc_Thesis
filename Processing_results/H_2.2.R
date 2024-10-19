# Hypothesis 2.2
library(readxl)
library(lme4)
library(car)
library(ggplot2)
library(DHARMa)
library(effects)
library(MASS)
library(ggeffects)

# Step 1: Reload the updated Excel file
Plants <- read_excel("C:/Users/PC/Documents/Sustainable Development/Thesis/Results/Part two/LMM.xlsx", sheet = "Plants")  

# Verify that the new columns are present
print(names(Plants))

# If AssE and AssEE are present, proceed with the analysis

# Calculate proportion of endangered endemics and proportion of native cover
Plants$Prop_Endangered <- Plants$AssEE / Plants$AssE
Plants$Prop_Native_Cover <- Plants$N / Plants$CA

# Scale variables
Plants$CA_scaled <- scale(Plants$CA)
Plants$Prop_Native_Cover_scaled <- scale(Plants$Prop_Native_Cover)

# Check distribution of Prop_Endangered
hist(Plants$Prop_Endangered, breaks = 20, main = "Distribution of Proportion of Endangered Endemics", xlab = "Proportion")

# Print summary statistics
summary(Plants$Prop_Endangered)
var(Plants$Prop_Endangered)

# Fit Binomial GLMM
model_binomial <- glmer(cbind(AssEE, AssE - AssEE) ~ CA_scaled + Prop_Native_Cover_scaled + (1|Archipelago), 
                        family = binomial, data = Plants)

# Print summary
summary(model_binomial)

# Check for overdispersion
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq / rdf
  pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
  c(chisq = Pearson.chisq, ratio = prat, rdf = rdf, p = pval)
}
overdisp_fun(model_binomial)

# Plotting
# Generate predictions from the Binomial GLMM
pred_binomial <- ggpredict(model_binomial, terms = c("Prop_Native_Cover_scaled [all]", "CA_scaled [mean]"))

# Create the plot
ggplot(Plants, aes(x = Prop_Native_Cover, y = Prop_Endangered, color = Archipelago)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_line(data = pred_binomial, aes(x = x * sd(Plants$Prop_Native_Cover) + mean(Plants$Prop_Native_Cover), y = predicted), 
            color = "black", linewidth = 1) +
  geom_ribbon(data = pred_binomial, aes(x = x * sd(Plants$Prop_Native_Cover) + mean(Plants$Prop_Native_Cover), 
                                        y = predicted, ymin = conf.low, ymax = conf.high), 
              alpha = 0.2, inherit.aes = FALSE) +
  geom_text(aes(label = Abbreviation), hjust = 1.5, vjust = 1.5, size = 3) +
  labs(title = "Proportion of Endangered Endemic Vascular Plants vs Proportion of Native Cover",
       x = "Proportion of Native Cover",
       y = "Proportion of Endangered Endemic Vascular Plants") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("prop_endangered_vs_prop_native_cover_binomial_model.png", width = 10, height = 6)


#Same for Insects

library(readxl)
Insects <- read_excel("C:/Users/PC/Documents/Sustainable Development/Thesis/Results/Part two/LMM.xlsx", sheet = "Insects")

# Calculate proportion of endangered endemics and proportion of native cover
Insects$Prop_Endangered <- ifelse(Insects$AssE > 0, Insects$AssEE / Insects$AssE, 0)
Insects$Prop_Native_Cover <- Insects$N / Insects$CA

# Scale variables
Insects$CA_scaled <- scale(Insects$CA)
Insects$Prop_Native_Cover_scaled <- scale(Insects$Prop_Native_Cover)

# Print summary to check the data
summary(Insects[c("AssEE", "AssE", "Prop_Endangered", "N", "CA", "Prop_Native_Cover")])

# Fit Binomial GLMM
# We'll use only the islands with AssE > 0
Insects_subset <- Insects[Insects$AssE > 0, ]
model_binomial <- glmer(cbind(AssEE, AssE - AssEE) ~ CA_scaled + Prop_Native_Cover_scaled + (1|Archipelago), 
                        family = binomial, data = Insects_subset)

# Print summary
summary(model_binomial)

# Check for overdispersion
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq / rdf
  pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
  c(chisq = Pearson.chisq, ratio = prat, rdf = rdf, p = pval)
}
overdisp_fun(model_binomial)

# Plotting
library(ggplot2)
library(ggeffects)

# Generate predictions from the Binomial GLMM
pred_binomial <- ggpredict(model_binomial, terms = c("Prop_Native_Cover_scaled [all]", "CA_scaled [mean]"))

# Create the plot
ggplot(Insects, aes(x = Prop_Native_Cover, y = Prop_Endangered, color = Archipelago)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_line(data = pred_binomial, aes(x = x * sd(Insects_subset$Prop_Native_Cover) + mean(Insects_subset$Prop_Native_Cover), y = predicted), 
            color = "black", linewidth = 1) +
  geom_ribbon(data = pred_binomial, aes(x = x * sd(Insects_subset$Prop_Native_Cover) + mean(Insects_subset$Prop_Native_Cover), 
                                        y = predicted, ymin = conf.low, ymax = conf.high), 
              alpha = 0.2, inherit.aes = FALSE) +
  geom_text(aes(label = Abbreviation), hjust = -0.5, vjust = 1.5, size = 3) +
  labs(title = "Proportion of Endangered Endemic Insects vs Proportion of Native Cover",
       x = "Proportion of Native Cover",
       y = "Proportion of Endangered Endemic Insects") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, by = 0.1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1))

ggsave("prop_endangered_vs_prop_native_cover_binomial_model_insects_corrected.png", width = 10, height = 6)


#without zeros
# Remove zeros and NA values
Insects_nonzero <- Insects[Insects$AssE > 0 & !is.na(Insects$Prop_Endangered), ]

# Recalculate scaled variables
Insects_nonzero$CA_scaled <- scale(Insects_nonzero$CA)
Insects_nonzero$Prop_Native_Cover_scaled <- scale(Insects_nonzero$Prop_Native_Cover)

# Fit the model
model_nonzero <- glmer(cbind(AssEE, AssE - AssEE) ~ CA_scaled + Prop_Native_Cover_scaled + (1|Archipelago), 
                       family = binomial, data = Insects_nonzero)

# Print summary
summary(model_nonzero)

# Check for overdispersion
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq / rdf
  pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
  c(chisq = Pearson.chisq, ratio = prat, rdf = rdf, p = pval)
}
overdisp_fun(model_nonzero)


# Plot
library(ggplot2)
library(ggeffects)

pred_nonzero <- ggpredict(model_nonzero, terms = c("Prop_Native_Cover_scaled [all]", "CA_scaled [mean]"))

ggplot(Insects_nonzero, aes(x = Prop_Native_Cover, y = Prop_Endangered, color = Archipelago)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_line(data = pred_nonzero, aes(x = x * sd(Insects_nonzero$Prop_Native_Cover) + mean(Insects_nonzero$Prop_Native_Cover), y = predicted), 
            color = "black", size = 1) +
  geom_ribbon(data = pred_nonzero, aes(x = x * sd(Insects_nonzero$Prop_Native_Cover) + mean(Insects_nonzero$Prop_Native_Cover), 
                                       y = predicted, ymin = conf.low, ymax = conf.high), 
              alpha = 0.2, inherit.aes = FALSE) +
  labs(title = "Proportion of Endangered Endemic Insects vs Proportion of Native Cover (Non-zero)",
       x = "Proportion of Native Cover",
       y = "Proportion of Endangered Endemic Insects") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1))

ggsave("prop_endangered_vs_prop_native_cover_binomial_model_insects_nonzero.png", width = 10, height = 6)

#Now for birds

# Calculate proportion of endangered endemics and proportion of native cover
Birds$Prop_Endangered <- ifelse(Birds$AssE > 0, Birds$AssEE / Birds$AssE, 0)
Birds$Prop_Native_Cover <- Birds$N / Birds$CA

# Keep all rows, including those with zero Prop_Endangered
Birds_subset <- Birds[!is.na(Birds$Prop_Endangered), ]  # Removed the condition for AssE > 0

# Scale the predictors
Birds_subset$CA_scaled <- scale(Birds_subset$CA)
Birds_subset$Prop_Native_Cover_scaled <- scale(Birds_subset$Prop_Native_Cover)

# Fit the binomial GLMM
model_birds <- glmer(cbind(AssEE, AssE - AssEE) ~ CA_scaled + Prop_Native_Cover_scaled + (1|Archipelago), 
                     family = binomial, data = Birds_subset)

# Print summary
summary(model_birds)

# Check for overdispersion
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq / rdf
  pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
  c(chisq = Pearson.chisq, ratio = prat, rdf = rdf, p = pval)
}
overdisp_fun(model_birds)

# Generate predictions for plotting
pred_birds <- ggpredict(model_birds, terms = c("Prop_Native_Cover_scaled [all]", "CA_scaled [mean]"))

# Create the plot
ggplot(Birds, aes(x = Prop_Native_Cover, y = Prop_Endangered, color = Archipelago)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_text(aes(label = Abbreviation), vjust = -1, size = 3, check_overlap = TRUE) +  # Add labels
  geom_line(data = pred_birds, aes(x = x * sd(Birds$Prop_Native_Cover) + mean(Birds$Prop_Native_Cover), 
                                     y = predicted), color = "black", linewidth = 1) +  # Add the predicted line
  geom_ribbon(data = pred_birds, aes(x = x * sd(Birds$Prop_Native_Cover) + mean(Birds$Prop_Native_Cover), 
                                       y = predicted, ymin = conf.low, ymax = conf.high), 
              alpha = 0.2, inherit.aes = FALSE) +  # Add confidence interval
  labs(title = "Proportion of Endangered Endemic Birds vs Proportion of Native Cover",
       x = "Proportion of Native Cover",
       y = "Proportion of Endangered Endemic Birds") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, by = 0.1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1))



# Print some additional information about the data
cat("Number of observations:", nrow(Birds_subset), "\n")
cat("Number of archipelagos:", length(unique(Birds_subset$Archipelago)), "\n")
cat("Range of Prop_Native_Cover:", range(Birds_subset$Prop_Native_Cover), "\n")
cat("Range of Prop_Endangered:", range(Birds_subset$Prop_Endangered), "\n")

#Importance of all variables
library(readxl)
library(lme4)
library(MuMIn)
library(ggplot2)
library(car)


#Which variables have an impact on the number of endangered endemics

Plants <- read_excel("C:/Users/PC/Documents/Sustainable Development/Thesis/Results/Part two/LMM.xlsx", sheet = "Plants")
Insects <- read_excel("C:/Users/PC/Documents/Sustainable Development/Thesis/Results/Part two/LMM.xlsx", sheet = "Insects")

library(lme4)
library(MuMIn)
library(ggplot2)
library(car)  # For vif function

# Function to check for collinearity
check_collinearity <- function(model) {
  if (length(fixef(model)) < 3) {  # Intercept + at least 2 predictors
    print("statement true")
    return(TRUE)  # Models with fewer than 2 predictors are considered non-collinear
  }
  tryCatch({
    vif_values <- vif(model)
    return(all(vif_values < 5))  # Using 5 as a threshold, adjust if needed
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
  full_model <- glmer(PropEnd ~ CA_scaled + PA_scaled + N_scaled + NHL_scaled + HHL_scaled + FNHL_scaled + PercChange_scaled + (1|Archipelago), 
                         family = binomial, data = data, na.action = na.fail)
  
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
    return(all(vif_values < 5))  # Using 5 as a threshold, adjust if needed
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
