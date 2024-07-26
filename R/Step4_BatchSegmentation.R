# Load Necessary Libraries ---------------------------------------------------
packages <- c("tidyverse", "imager", "colorscience", "randomForest", "doParallel")

# Function to install and load packages if necessary
install_if_necessary <- function(pack) {
    if (!require(pack, character.only = TRUE)) {
        install.packages(pack)
        require(pack, character.only = TRUE)
    }
}

# Apply the function to all packages
lapply(packages, install_if_necessary)

# Load Pre-trained Model -----------------------------------------------------
yam_classifier <- readRDS("./out/YamPixelClassifier.rds")

# Batch Segmentation of Images -----------------------------------------------
# Get the list of image files for processing
image_files <- list.files(path = "./out/CroppedFrame/", pattern = '\\.png$', ignore.case = TRUE)

# Register parallel backend to use multiple cores
registerDoParallel(cluster <- makeCluster(10))

# Parallel processing of images
results <- foreach(i = 1:length(image_files), .verbose = TRUE,
                   .packages = c('imager', 'tidyverse', 'colorscience', 'randomForest')) %dopar% {
                       file_name <- image_files[i]
                       cat(paste("Reading file:", file_name, "\n"))
                       image <- load.image(paste0("./out/CroppedFrame/", file_name))
                       image_dim <- dim(image)
                       
                       # Convert image to data frame with RGB channels
                       image_data <- channels(image) %>% as.data.frame() %>% spread(im, value)
                       colnames(image_data) <- c("x", "y", "R", "G", "B")
                       image_data$x <- rep(1:image_dim[1], each = image_dim[2])
                       image_data$y <- rep(image_dim[2]:1, image_dim[1])
                       
                       # Transform RGB to other color spaces and compute additional features
                       image_data <- cbind(image_data, as.data.frame(RGB2HSV(cbind(image_data$R * 255, image_data$G * 255, image_data$B * 255))))
                       image_data <- cbind(image_data, as.data.frame(RGB2XYZ(cbind(image_data$R, image_data$G, image_data$B))))
                       image_data <- cbind(image_data, as.data.frame(XYZ2Lab(cbind(image_data$V1, image_data$V2, image_data$V3))))
                       image_data$r.g <- image_data$R / (ifelse(image_data$G == 0, 0.001, image_data$G))
                       image_data$g2rb <- (2 * image_data$G) - image_data$R - image_data$B
                       
                       # Predict the class for each pixel using the pre-trained model
                       image_data$Class <- predict(yam_classifier, newdata = image_data[, -c(1:2)])
                       
                       # Calculate the percentage of yam cover in the image
                       yam_percentage <- nrow(subset(image_data, Class == "Y")) / nrow(image_data) * 100
                       c(file_name, yam_percentage)
                   }

# Stop the parallel cluster
stopCluster(cluster)

# Gather results into a data frame and save them -----------------------------
results_df <- do.call(rbind, results) %>% as.data.frame()
colnames(results_df) <- c("Image", "YamCoverPercentage")
results_df$Image <- gsub(".png", "", results_df$Image)
results_df$Image <- gsub("__", "_", results_df$Image)
results_df <- separate(results_df, Image, into = c("Variable", "Replicate", "Date"), sep = "_")
results_df$YamCoverPercentage <- as.numeric(results_df$YamCoverPercentage)

# Save the results to a CSV file
write.csv2(results_df, "./out/Yam_Cover_Percentage.csv", row.names = FALSE, sep = ";", col.names = TRUE)
