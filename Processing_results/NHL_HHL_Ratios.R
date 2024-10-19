library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggrepel)
NHL_HHL_ratios <- read_excel("C:/Users/PC/Documents/Sustainable Development/Thesis/Results/Part one/NHL_HHL/NHL_HHL_Ratios.xlsx", sheet = 1)

# Create a dot plot grouped by archipelagos
plot <- ggplot(NHL_HHL_ratios, aes(x = Archipelago, y = `HumHL/BaseHL`, color = Archipelago)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), size = 3, alpha = 0.7) +
  geom_text_repel(aes(label = Island), 
                  size = 3, 
                  box.padding = 0.1, 
                  point.padding = 0.2, 
                  force = 1,
                  max.overlaps = Inf,
                  nudge_y = -0.05,  # Adjust vertical position
                  nudge_x = 0.1,    # Adjust this value to move labels horizontally
                  segment.color = NA) +  # Remove connecting lines
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none"
  ) +
  labs(x = "Archipelago",
       y = "HumHL/BaseHL Ratio") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 650))  

# Save the plot 
ggsave(file.path(getwd(), "HHL_NHL_ratios_by_archipelago.png"), plot = plot, width = 10, height = 6)

# Print a message to confirm the plot has been saved
cat("Plot saved as HHL_NHL_ratios_by_archipelago.png\n")



#NHL and HHL Cleveland dot plot
create_cleveland_plot <- function(islands_to_exclude = c("Isabela"), filename = "BaseHL_HumHL_Cleveland_dotplot.png") {
   # Read the Excel file
   NHL_HHL_data <- read_excel("C:/Users/PC/Documents/Sustainable Development/Thesis/Results/Part one/NHL_HHL/NHL_HHL_Ratios.xlsx", sheet = 1)

   # Modify the Archipelago and remove specified islands
   NHL_HHL_data <- NHL_HHL_data %>%
     filter(!Island %in% islands_to_exclude)

   # Reshape the data for Cleveland dot plot
   NHL_HHL_long <- NHL_HHL_data %>%
     select(Archipelago, Island, BaseHL, HumHL) %>%
     pivot_longer(cols = c(BaseHL, HumHL), names_to = "Type", values_to = "Value")

   # Create the Cleveland dot plot
   plot <- ggplot(NHL_HHL_long, aes(x = Value, y = reorder(Island, Value))) +
     geom_line(aes(group = Island), color = "gray") +
     geom_point(aes(color = Type), size = 4) +
     facet_grid(Archipelago ~ ., scales = "free_y", space = "free_y") +
     scale_color_manual(values = c("BaseHL" = "darkgreen", "HumHL" = "gold")) +
     theme_minimal() +
     theme(
       panel.background = element_rect(fill = "white", color = NA),
       plot.background = element_rect(fill = "white", color = NA),
       legend.position = "top",
       legend.text = element_text(size = 12),
       strip.text.y = element_text(angle = 0, size = 14),
       axis.text.y = element_text(size = 14)
     ) +
     labs(x = "Rate (km2/year)", y = "Island", color = "Type")

   # Save the plot 
   ggsave(file.path("C:/Users/PC/Documents/Sustainable Development/Thesis/Results/Part one/NHL_HHL", filename), plot = plot, width = 12, height = 10)

   # Check if the plot was saved successfully
   if (file.exists(file.path(getwd(), filename))) {
       cat("Plot saved as", filename, "\n")
   } else {
       cat("Failed to save the plot as", filename, "\n")
   }
}

# Call the function to create the Cleveland dot plot with the original range
create_cleveland_plot(islands_to_exclude = c("Isabela"), filename = "BaseHL_HumHL_Cleveland_dotplot.png")  

# Call the function to create the Cleveland dot plot with a smaller range
create_cleveland_plot(islands_to_exclude = c("Isabela", "Hawaii", "La Réunion", "Mauritius", "Oahu"), filename = "BaseHL_HumHL_Cleveland_dotplot_smaller_range.png")
  
