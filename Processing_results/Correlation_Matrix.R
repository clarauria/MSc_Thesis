install.packages("corrplot")
library(readxl)
library(corrplot)

# Read the Excel file
data <- read_excel("C:/Users/PC/Documents/Sustainable Development/Thesis/Results/Part two/LMM.xlsx")  

# Select specific variables for correlation
selected_variables <- data[, c("CA", "PA", "N", "NHL", "HHL", "FNHL", "PercChange")]  

# Define new labels for the variables
new_labels <- c("CurrentA", "PastA", "Native", "BaseHL", "HumHL", "FastBaseHL", "PercChange")

# Create the correlation matrix
correlation_matrix <- cor(selected_variables, use = "complete.obs")

# Set the row and column names for the correlation matrix to the new labels
rownames(correlation_matrix) <- new_labels  # Set row names
colnames(correlation_matrix) <- new_labels  # Set column names

# Plot the correlation matrix using corrplot with circles and add correlation coefficients
corrplot(correlation_matrix, method = "circle", 
         addCoef.col = "black",  
         tl.col = "black",    
         tl.srt = 45,         
         tl.cex = 1,  # Adjust text size for labels
         col = colorRampPalette(c("royalblue4", "white", "orangered4"))(200))  # Ensure this is correctly placed
