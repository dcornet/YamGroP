# ----------------------------------------------------------------------------------
# R Script for Image Classification using Random Forest
# ----------------------------------------------------------------------------------

# --------------------------- 1. Load Required Libraries ---------------------------
# Define a vector of necessary packages
packages <- c("tidyverse", "imager", "colorscience", "crayon", "randomForest", "pROC")

# Function to install and load packages if they are not already installed
install_if_necessary <- function(pack) {
  if (!require(pack, character.only = TRUE)) {  # Check if the package is installed
    install.packages(pack)                       # Install the package if not installed
    require(pack, character.only = TRUE)         # Load the package after installation
  }
}

# Apply the installation/loading function to all packages listed in 'packages'
lapply(packages, install_if_necessary)

# ------------------- 2. Data Collection for Classification -------------------------
# Initialize an empty dataframe to store classification results
results <- data.frame()

# Set the number of images to process
num_images <- 2

# Set the number of pixels to sample from each class (Frame and Background) per image
pixels_per_class <- 5

# Clear the R console for a clean start
cat("\014")

# ------------------- 3. Loop Through Images and Collect Pixel Data -------------------
for (i in 1:num_images) {
  
  # Prompt the user to select an image file and load it
  cat(crayon::red(paste0("\nSelect image ", i, " of ", num_images, "\n")))
  image <- load.image(file.choose()) # Opens a file dialog for image selection
  
  # Resize the image to 600x400 pixels for consistency
  image <- resize(image, 600, 400)
  
  # Set plot margins to zero for better visualization
  par(mar = c(0, 0, 0, 0))
  
  # Display the loaded image
  plot(image)
  
  # ------------------- 3a. Collect Frame Pixels -------------------------------------
  # Prompt the user to select pixels corresponding to the 'Frame' class
  cat(crayon::green(paste0("\nPlease select ",  pixels_per_class, 
                           " pixels corresponding to the blue frame\n")))
  
  # Use 'locator' to capture the coordinates of selected pixels
  Frame <- as.data.frame(lapply(locator(pixels_per_class), round, 0))
  
  # ------------------- 3b. Collect Background Pixels --------------------------------
  # Prompt the user to select pixels corresponding to the 'Background' class
  cat(crayon::green(paste0("\nPlease select ",  pixels_per_class, 
                           " pixels corresponding to the background\n")))
  
  # Use 'locator' to capture the coordinates of selected pixels
  Background <- as.data.frame(lapply(locator(pixels_per_class), round, 0))
  
  # ------------------- 3c. Assign Class Labels -------------------------------------
  # Assign the class label 'Frame' to the selected frame pixels
  Frame$Class <- "Frame"
  
  # Assign the class label 'Background' to the selected background pixels
  Background$Class <- "Background"
  
  # ------------------- 3d. Combine Frame and Background Data ------------------------
  # Combine the Frame and Background data into a single dataframe
  df <- bind_rows(Frame, Background)
  
  # ------------------- 3e. Extract Color Channels and Coordinates -------------------
  # Extract the Red, Green, and Blue color channels from the image at the selected pixel coordinates
  # and bind them with the class labels and coordinates
  df <- cbind(df$Class, 
              diag(image[df$x, df$y, 1, 1]), # Red channel
              diag(image[df$x, df$y, 1, 2]), # Green channel
              diag(image[df$x, df$y, 1, 3]), # Blue channel
              df$x, df$y)
  
  # ------------------- 3f. Append to the Results Dataframe --------------------------
  # Append the current image's pixel data to the overall results dataframe
  results <- bind_rows(as.data.frame(results), as.data.frame(df))
}

# ------------------- 4. Compute Color Space Values for Classification Data ------------
# Rename the columns of the results dataframe for clarity
colnames(results) <- c("class", "R", "G", "B", "x", "y")

# Convert all columns except 'class' to character type
results[,-1] <- lapply(results[,-1], as.character)

# Convert all columns except 'class' to numeric type
results[,-1] <- lapply(results[,-1], as.numeric)

# ------------------- 5. Convert RGB to Other Color Spaces -----------------------------
# Convert RGB values to HSV (Hue, Saturation, Value) color space
hsv_data <- as.data.frame(RGB2HSV(cbind(results$R*255, results$G*255, results$B*255)))

# Convert RGB values to XYZ color space
XYZ_data <- as.data.frame(RGB2XYZ(cbind(results$R, results$G, results$B)))
colnames(XYZ_data)<-c("X", "Y", "Z")

# Convert XYZ values to Lab color space
Lab_data <- as.data.frame(XYZ2Lab(cbind(XYZ_data$X, XYZ_data$Y, XYZ_data$Z)))

# Combine all color space data with the original results dataframe
color_data <- cbind(results, hsv_data, XYZ_data, Lab_data)

# Create additional color-based features for classification
color_data$r.g <- color_data$R / (ifelse(color_data$G == 0, 0.001, color_data$G)) # Ratio of Red to Green
color_data$g2rb <- (2 * color_data$G) - color_data$R - color_data$B  # Custom feature

# Convert the 'class' column to a factor for classification
color_data$class <- as.factor(color_data$class)
color_data<-dplyr::select(color_data, -x, -y)

# ------------------- 6. Train a Random Forest Classifier -------------------------------
# Train a Random Forest model to classify pixels as 'Frame' or 'Background'
model <- randomForest(
  class ~ .,                        # Predict 'class' using all other variables
  data = color_data,                # Training data
  importance = TRUE,                # Calculate variable importance
  proximity = TRUE,                 # Calculate proximity between cases
  mtry = 5,                         # Number of variables randomly sampled as candidates at each split
  ntree = 450,                      # Number of trees in the forest
  nodesize = 7                      # Minimum size of terminal nodes
)


# ------------------- 7. Visualize trained model ----------------------------------------
# Print a summary of the trained Random Forest model
print(model)

# Plot the error rate of the Random Forest model as trees are added
png(height=6,  width=10, res=300, filename= "./out/FrameClassifier_OOBbyTreeNb.png",
    units = "in",  type = "cairo",  family = "Garamond")
plot(model, main = "Error Rate by Number of Trees")
legend("topright", legend = c("Overall OOB Error", "Class 1 Error", "Class 2 Error"), 
       col = c("black", "red", "green"), lty = 1, cex = 0.8)
dev.off()

# Display the importance of each variable in the model
importance(model)

png(height=10,  width=8, res=300, filename= "./out/FrameClassifier_VariableImportance.png",
    units = "in",  type = "cairo",  family = "Garamond")
varImpPlot(model, main="Variable importance")
dev.off()

# Display confusion matrix
print(model$confusion)
conf_matrix <- as.data.frame(as.table(model$confusion))
colnames(conf_matrix) <- c("Actual", "Predicted", "Frequency")
total_correct <- sum(diag(model$confusion))
total_predictions <- sum(model$confusion)
accuracy <- total_correct / total_predictions

png(height=6,  width=8, res=300, filename= "./out/FrameClassifier_ConfusionMatrix.png",
    units = "in",  type = "cairo",  family = "Garamond")
ggplot(data = conf_matrix, aes(x = Predicted, y = Actual, fill = Frequency)) +
  geom_tile(color="grey50") +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Confusion Matrix Heatmap", x = "Predicted Class", y = "Actual Class",
       subtitle = paste("Accuracy:", round(accuracy * 100, 2), "%")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()


# Display ROC curve (binary classification)
png(height=6,  width=6, res=300, filename= "./out/FrameClassifier_ROC.png",
    units = "in",  type = "cairo",  family = "Garamond")
roc_curve <- roc(response = model$y, predictor = model$votes[,2])
plot(roc_curve, main = "ROC Curve")
dev.off()


# ------------------- 8. Save the Trained Model -----------------------------------------
# Save the trained Random Forest model as an RDS file for future use
saveRDS(model, "./out/FramePixelClassifier.rds")

