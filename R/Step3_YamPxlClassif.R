# Loading Necessary Libraries ------------------------------------------------
packages <- c("tidyverse", "imager","crayon", "colorscience", "randomForest")

# Function to install and load packages if necessary
install_if_necessary <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name)
    require(package_name, character.only = TRUE)
  }
}

# Apply the function to all packages in parallel
future.apply::future_lapply(packages, install_if_necessary)

# Data Collection for Classification -----------------------------------------
classification_data <- data.frame()
number_of_images <- 2  # Number of images to build the classification model
pixels_per_category <- 10  # Number of pixels to sample from each class in each image

# Loop through the number of images
for (image_index in 1:number_of_images) {
  cat(crayon::red(paste0("\nSelect image ", image_index, " of ", number_of_images, "\n")))
  selected_image <- load.image(file.choose())  # Choose the image file
  par(mar = c(0, 0, 0, 0))
  plot(selected_image)
  
  # Collect points for Yam and Background classes
  cat(crayon::green(paste0("\nPlease select ", pixels_per_category, " pixels corresponding to the yam cover\n")))
  yam_points <- as.data.frame(lapply(locator(pixels_per_category), round, 0))
  cat(crayon::green(paste0("\nPlease select ", pixels_per_category, " pixels corresponding to the background\n")))
  background_points <- as.data.frame(lapply(locator(pixels_per_category), round, 0))
  yam_points$class <- "Y"
  background_points$class <- "B"
  combined_points <- bind_rows(yam_points, background_points)
  
  # Extract color channels and coordinates
  combined_points <- cbind(combined_points$class, 
              diag(selected_image[combined_points$x, combined_points$y, 1, 1]),  # Red channel
              diag(selected_image[combined_points$x, combined_points$y, 1, 2]),  # Green channel
              diag(selected_image[combined_points$x, combined_points$y, 1, 3]),  # Blue channel
              combined_points$x, combined_points$y)
  
  # Bind the results to the final data frame
  classification_data <- bind_rows(classification_data, as.data.frame(combined_points))
}

# Convert data types and transform color spaces ------------------------------
colnames(classification_data) <- c("class", "red", "green", "blue", "x_coordinate", "y_coordinate")
classification_data[,-1] <- lapply(classification_data[,-1], as.character)
classification_data[,-1] <- lapply(classification_data[,-1], as.numeric)

# Convert RGB to HSV, XYZ, and Lab color spaces, and add additional features
# Converting RGB to different color spaces (HSV, XYZ, Lab) provides multiple ways of representing color information, which can help the model capture different aspects of color variation. HSV represents hue, saturation, and value, which can be useful for distinguishing colors more intuitively. XYZ and Lab are designed to approximate human vision and can help improve model performance by providing perceptually uniform color representations.
data_matrix <- cbind(classification_data, as.data.frame(RGB2HSV(cbind(classification_data$red * 255, classification_data$green * 255, classification_data$blue * 255))))
data_matrix <- cbind(data_matrix, as.data.frame(RGB2XYZ(cbind(data_matrix$red, data_matrix$green, data_matrix$blue))))
data_matrix <- cbind(data_matrix, as.data.frame(XYZ2Lab(cbind(data_matrix$V1, data_matrix$V2, data_matrix$V3))))
data_matrix <- dplyr::select(data_matrix, class, red:blue, H:b)
data_matrix$red_green_ratio <- data_matrix$red / data_matrix$green
data_matrix$green_2red_blue <- (2 * data_matrix$green) - data_matrix$red - data_matrix$blue
data_matrix$class <- as.factor(data_matrix$class)

# Train a Random Forest Classifier and Save the Model ------------------------
# Use cross-validation to find the optimal values for ntree and mtry to decrease training time while maintaining accuracy
yam_pixel_classifier <- randomForest(class ~ ., data = data_matrix, importance = TRUE, proximity = TRUE, 
                               mtry = 5, ntree = 300, nodesize = 3)


# ------------------- Visualize trained model ----------------------------------------
# Print a summary of the trained Random Forest model
print(yam_pixel_classifier)

# Plot the error rate of the Random Forest model as trees are added
png(height=6,  width=10, res=300, filename= "./out/YamClassifier_OOBbyTreeNb.png",
    units = "in",  type = "cairo",  family = "Garamond")
plot(yam_pixel_classifier, main = "Error Rate by Number of Trees")
legend("topright", legend = c("Overall OOB Error", "Class 1 Error", "Class 2 Error"), 
       col = c("black", "red", "green"), lty = 1, cex = 0.8)
dev.off()

# Display the importance of each variable in the model
importance(yam_pixel_classifier)

png(height=10,  width=8, res=300, filename= "./out/YamClassifier_VariableImportance.png",
    units = "in",  type = "cairo",  family = "Garamond")
varImpPlot(yam_pixel_classifier, main="Variable importance")
dev.off()

# Display confusion matrix
print(yam_pixel_classifier$confusion)
conf_matrix <- as.data.frame(as.table(yam_pixel_classifier$confusion))
colnames(conf_matrix) <- c("Actual", "Predicted", "Frequency")
total_correct <- sum(diag(yam_pixel_classifier$confusion))
total_predictions <- sum(yam_pixel_classifier$confusion)
accuracy <- total_correct / total_predictions

png(height=6,  width=8, res=300, filename= "./out/YamClassifier_ConfusionMatrix.png",
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
png(height=6,  width=6, res=300, filename= "./out/YamClassifier_ROC.png",
    units = "in",  type = "cairo",  family = "Garamond")
roc_curve <- roc(response = yam_pixel_classifier$y, predictor = yam_pixel_classifier$votes[,2])
plot(roc_curve, main = "ROC Curve")
dev.off()





saveRDS(yam_pixel_classifier, "./out/YamPixelClassifier.rds")
