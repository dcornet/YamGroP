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
print(yam_pixel_classifier)
plot(yam_pixel_classifier)
importance(yam_pixel_classifier)
saveRDS(yam_pixel_classifier, "./out/YamPixelClassifier.rds")
