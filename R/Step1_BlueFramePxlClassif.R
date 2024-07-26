# Loading the necessary libraries -------------------------------------------
packages <- c("tidyverse", "imager", "colorscience", "crayon", "randomForest")

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
num_images <- 10 # Number of images to use for building the classification model
pixels_per_class <- 20 # Number of pixels to sample from each class in each image
cat("\014")  # Clear console

for (i in 1:num_images) {
  cat(crayon::red(paste0("\nSelect image ", i, " of ", num_images, "\n")))
  image <- load.image(file.choose()) # Select the image file
  image <- resize(image, 600, 400)
  par(mar = c(0, 0, 0, 0))
  plot(image)
  
  # Collect points for the Frame and Background classes
  cat(crayon::green(paste0("\nPlease select ",  pixels_per_class, 
                           " pixels corresponding to the blue frame\n")))
  Frame <- as.data.frame(lapply(locator(pixels_per_class), round, 0))
  cat(crayon::green(paste0("\nPlease select ",  pixels_per_class, 
                           " pixels corresponding to the background\n")))
  Background <- as.data.frame(lapply(locator(pixels_per_class), round, 0))
  
  # Assign class labels
  Frame$Class <- "Frame"
  Background$Class <- "Background"
  
  # Combine data into one dataframe
  df <- bind_rows(Frame, Background)
  
  # Extract color channels and coordinates
  df <- cbind(df$Class, 
              diag(image[df$x, df$y, 1, 1]), # Red channel
              diag(image[df$x, df$y, 1, 2]), # Green channel
              diag(image[df$x, df$y, 1, 3]), # Blue channel
              df$x, df$y)
  
  # Append to the results dataframe
  results <- bind_rows(as.data.frame(results), as.data.frame(df))
}

# Computing Color Space Values for Classification Data ----------------------
colnames(results) <- c("class", "R", "G", "B", "x", "y")
results[,-1] <- lapply(results[,-1], as.character)
results[,-1] <- lapply(results[,-1], as.numeric)

# Convert RGB to other color spaces
hsv_data<-as.data.frame(RGB2HSV(cbind(results$R*255, results$G*255, results$B*255)))
XYZ_data<-as.data.frame(RGB2XYZ(cbind(results$R, results$G, results$B)))
Lab_data<-as.data.frame(XYZ2Lab(cbind(XYZ_data$V1, XYZ_data$V2, XYZ_data$V2)))
color_data <- cbind(results, hsv_data, XYZ_data, Lab_data)
color_data$r.g <- color_data$R / color_data$G
color_data$g2rb <- (2 * color_data$G) - color_data$R - color_data$B
color_data$class <- as.factor(color_data$class)

# Training a Random Forest Classifier and Saving Results -------------------
model <- randomForest(class ~ ., data = color_data, importance = TRUE, 
                      proximity = TRUE, mtry = 5, ntree = 450, nodesize = 7)
print(model)
plot(model)
importance(model)
saveRDS(model, "./out/FramePixelClassifier.rds")
