# Load Necessary Libraries ---------------------------------------------------
# Define the list of required packages
packages <- c("tidyverse", "imager", "colorscience", "randomForest", "doParallel", "cowplot")

# Function to install and load packages if necessary
install_if_necessary <- function(pack) {
  if (!require(pack, character.only = TRUE)) {
    install.packages(pack)  # Install the package if not already installed
    require(pack, character.only = TRUE)  # Load the package
  }
}

# Apply the function to all packages in parallel to ensure they are installed and loaded
future.apply::future_lapply(packages, install_if_necessary)

# Load Pre-trained Model -----------------------------------------------------
# Load the pre-trained Random Forest classifier for yam cover detection
yam_classifier <- readRDS("./out/YamPixelClassifier.rds")

# Batch Segmentation of Images -----------------------------------------------
# Get the list of image files for processing from the specified directory
image_files <- list.files(path = "./out/CroppedFrame/", pattern = '.png', ignore.case = TRUE)

# Register parallel backend to use multiple cores for faster processing
num_cores <- parallel::detectCores() - 5  # Use available cores minus one for system stability
registerDoParallel(cluster <- makeCluster(num_cores))

# Parallel processing of images to segment and classify yam cover
results <- foreach(i = 1:length(image_files), .verbose = FALSE,
                   .packages = c('imager', 'tidyverse', 'colorscience', 'randomForest', 'ggplot2', 'cowplot')) %dopar% {
                     file_name <- image_files[i]
                     
                     # Load the image from the specified path
                     image <- load.image(paste0("./out/CroppedFrame/", file_name))
                     image_dim <- dim(image)
                     
                     # Convert the image to a matrix with RGB channels
                     image_data <- channels(image) %>% as.data.frame() %>% spread(im, value)
                     colnames(image_data) <- c("X_Coordinate", "Y_Coordinate", "red", "green", "blue")
                     image_data$x <- rep(1:image_dim[1], each = image_dim[2])
                     image_data$y <- rep(image_dim[2]:1, image_dim[1])
                     
                     # Transform RGB values to HSV, XYZ, and Lab color spaces for additional features
                     image_data <- cbind(image_data, as.data.frame(RGB2HSV(cbind(image_data$red * 255, image_data$green * 255, image_data$blue * 255))))
                     image_data <- cbind(image_data, as.data.frame(RGB2XYZ(cbind(image_data$red, image_data$green, image_data$blue))))
                     image_data <- cbind(image_data, as.data.frame(XYZ2Lab(cbind(image_data$V1, image_data$V2, image_data$V3))))
                     
                     # Calculate additional color features
                     image_data$red_green_ratio <- image_data$red / (ifelse(image_data$green == 0, .Machine$double.eps, image_data$green))  # Red to Green ratio
                     image_data$green_2red_blue <- (2 * image_data$green) - image_data$red - image_data$blue  # Green minus Red and Blue
                     
                     # Predict the class (Yam or Background) for all pixels using the pre-trained model in batch
                     image_data$Class <- predict(yam_classifier, newdata = image_data[, -c(1:2)], type = "response")
                     
                     # Calculate the percentage of pixels classified as yam cover in the image
                     yam_cover_percentage <- nrow(subset(image_data, Class == "Y")) / nrow(image_data) * 100
                     
                     # Visualize Original and Segmented Images and Save -------------------------------
                     # Segment the image using the pre-trained classifier
                     segmented_image_data <- image_data %>%
                       select(X_Coordinate, Y_Coordinate, red, green, blue, Class) %>%
                       mutate(Color = ifelse(Class == "Y", "green", "black"))
                     
                     # Plot original image
                     original_plot <- ggplot() +
                       geom_raster(data = as.data.frame(image, wide = "c"), aes(x = x, y = y, fill = rgb(c.1, c.2, c.3))) +
                       theme_void() +
                       ggtitle(paste("Original Image -", file_name)) +
                       scale_fill_identity()+
                       coord_fixed()
                     
                     # Plot segmented image
                     segmented_plot <- ggplot(segmented_image_data, aes(x = X_Coordinate, y = Y_Coordinate)) +
                       geom_tile(aes(fill = Color)) +
                       scale_fill_identity() +
                       theme_void() +
                       ggtitle(paste("Segmented Image -", file_name)) +
                       coord_fixed()
                     
                     # Combine both plots side by side
                     combined_plot <- plot_grid(original_plot, segmented_plot, labels = c("A", "B"), ncol = 2)
                     
                     # Save the combined plot
                     ggsave(filename = paste0("./out/SegmentedPics/Combined_", file_name), plot = combined_plot, width = 16, height = 6)
                     
                     # Return the file name and yam cover percentage
                     c(file_name, yam_cover_percentage)
                   }

# Stop the parallel cluster to release resources
stopCluster(cluster)

# Gather results into a data frame and save them -----------------------------
# Combine the results from all images into a data frame
results_df <- do.call(rbind, results) %>% as.data.frame()
colnames(results_df) <- c("Image_Name", "Yam_Cover_Percentage")

# Clean up the image file names for easier analysis
results_df$Image_Name <- gsub(".png", "", results_df$Image_Name)  # Remove file extension
results_df$Image_Name <- gsub("__", "_", results_df$Image_Name)  # Replace double underscores with a single underscore

# Separate the image name into variable components (e.g., Variable, Replicate, Date)
results_df <- separate(results_df, Image_Name, into = c("Variable", "Replicate", "Date"), sep = "_")
results_df$Yam_Cover_Percentage <- as.numeric(results_df$Yam_Cover_Percentage)  # Convert yam cover percentage to numeric

# Save the results to a CSV file for further analysis
write.csv2(results_df, "./out/Yam_Cover_Percentage.csv", row.names = FALSE, sep = ";", col.names = TRUE)
