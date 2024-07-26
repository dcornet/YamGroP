# Loading Necessary Libraries ------------------------------------------------
packages <- c("tidyverse", "imager","crayon", "colorscience", "randomForest")

# Function to install and load packages if necessary
install_if_necessary <- function(pack) {
  if (!require(pack, character.only = TRUE)) {
    install.packages(pack)
    require(pack, character.only = TRUE)
  }
}

# Apply the function to all packages
lapply(packages, install_if_necessary)

# Data Collection for Classification -----------------------------------------
results <- data.frame()
num_images <- 1  # Number of images to build the classification model
pixels_per_class <- 2  # Number of pixels to sample from each class in each image

# Loop through the number of images
for (i in 1:num_images) {
  cat(crayon::red(paste0("\nSelect image ", i, " of ", num_images, "\n")))
  image <- load.image(file.choose())  # Choose the image file
  par(mar = c(0, 0, 0, 0))
  plot(image)
  
  # Collect points for Yam and Other classes
  cat(crayon::green(paste0("\nPlease select ", pixels_per_class, " pixels corresponding to the yam cover\n")))
  Yam <- as.data.frame(lapply(locator(pixels_per_class), round, 0))
  cat(crayon::green(paste0("\nPlease select ", pixels_per_class, " pixels corresponding to the background\n")))
  Other <- as.data.frame(lapply(locator(pixels_per_class), round, 0))
  Yam$Class <- "Y"
  Other$Class <- "O"
  df <- bind_rows(Yam, Other)
  
  # Extract color channels and coordinates
  df <- cbind(df$Class, 
              diag(image[df$x, df$y, 1, 1]),  # Red channel
              diag(image[df$x, df$y, 1, 2]),  # Green channel
              diag(image[df$x, df$y, 1, 3]),  # Blue channel
              df$x, df$y)
  
  # Bind the results to the final data frame
  results <- bind_rows(results, as.data.frame(df))
}

# Convert data types and transform color spaces ------------------------------
colnames(results) <- c("class", "R", "G", "B", "x", "y")
results[,-1] <- lapply(results[,-1], as.character)
results[,-1] <- lapply(results[,-1], as.numeric)

# Convert RGB to HSV, XYZ, and Lab color spaces, and add additional features
data_matrix <- cbind(results, as.data.frame(RGB2HSV(cbind(results$R * 255, results$G * 255, results$B * 255))))
data_matrix <- cbind(data_matrix, as.data.frame(RGB2XYZ(cbind(data_matrix$R, data_matrix$G, data_matrix$B))))
data_matrix <- cbind(data_matrix, as.data.frame(XYZ2Lab(cbind(data_matrix$V1, data_matrix$V2, data_matrix$V3))))
data_matrix <- dplyr::select(data_matrix, class, R:B, H:b)
data_matrix$r.g <- data_matrix$R / data_matrix$G
data_matrix$g2rb <- (2 * data_matrix$G) - data_matrix$R - data_matrix$B
data_matrix$class <- as.factor(data_matrix$class)

# Train a Random Forest Classifier and Save the Model ------------------------
yam_classifier <- randomForest(class ~ ., data = data_matrix, importance = TRUE, proximity = TRUE, 
                               mtry = 5, ntree = 300, nodesize = 3)
print(yam_classifier)
plot(yam_classifier)
importance(yam_classifier)
saveRDS(yam_classifier, "./out/YamPixelClassifier.rds")
