# Load Required Libraries -----------------------------------------------------
# Define the list of necessary packages excluding 'parallel' and 'ggplot2'
required_packages <- c("tidyverse", "imager", "tripack", "colorscience", 
                       "randomForest", "parallel")

# Function to install and load packages if they are not already installed
install_and_load <- function(package_name) {
    if (!require(package_name, character.only = TRUE)) {
        install.packages(package_name, dependencies = TRUE)
        library(package_name, character.only = TRUE)
    }
}

# Install and load each required package
lapply(required_packages, install_and_load)


# Load Image Files ------------------------------------------------------------
# Specify the directory containing image files
image_directory <- "./data/GC_Pics_Example/"

# Retrieve all JPG image file paths from the specified directory and its subdirectories
image_files <- list.files(path = image_directory, pattern = "\\.JPG$", 
    recursive = TRUE, full.names = TRUE)

# Define the Frame Detection Function -----------------------------------------
FrameDetection <- function(image_path) {
    tryCatch({
        # Extract components of the image path to create a unique identifier
        path_components <- strsplit(image_path, "/")[[1]]
        image_filename <- gsub("\\.JPG$", "", path_components[5])  # Remove '.JPG' extension
        parent_folder <- path_components[4]
        unique_image_id <- paste0(image_filename, "_", parent_folder)
        
        # Load the image and retrieve its original dimensions
        original_image <- load.image(image_path)
        original_dims <- dim(original_image)
        
        # Resize the image to a width of 600 pixels while maintaining aspect ratio
        resized_width <- 600
        resized_height <- 600 / original_dims[1] * original_dims[2]
        resized_image <- imager::resize(original_image, resized_width, resized_height)
        
        # Convert the resized image to a data frame with separate RGB channels
        image_df <- as.data.frame(resized_image, wide = "c")
        colnames(image_df) <- c("x", "y", "R", "G", "B")  # Ensure required column names
        
        # Convert RGB values to HSV color space and add H, S, V columns
        hsv_values <- as.data.frame(RGB2HSV(cbind(image_df$R * 255, image_df$G * 255, image_df$B * 255)))
        colnames(hsv_values) <- c("H", "S", "V")
        image_df <- cbind(image_df, hsv_values)
        
        # Convert RGB values to XYZ color space and add X, Y, Z columns
        xyz_values <- as.data.frame(RGB2XYZ(cbind(image_df$R, image_df$G, image_df$B)))
        colnames(xyz_values) <- c("X", "Y", "Z")
        image_df <- cbind(image_df, xyz_values)
        
        # Convert XYZ values to Lab color space and add L, a, b columns
        lab_values <- as.data.frame(XYZ2Lab(cbind(image_df$X, image_df$Y, image_df$Z)))
        colnames(lab_values) <- c("L", "a", "b")
        image_df <- cbind(image_df, lab_values)
        
        # Add additional feature columns required by the RF model
        image_df$r.g <- image_df$R / ifelse(image_df$G == 0, 0.001, image_df$G)
        image_df$g2rb <- (2 * image_df$G) - image_df$R - image_df$B
        
        # Load the pre-trained Random Forest classifier model
        classifier_model <- readRDS("./out/FramePixelClassifier.rds")
        
        # Predict the class of each pixel using the RF model
        pixel_predictions <- predict(classifier_model, newdata = image_df[, -c(1, 2)])  # Exclude 'x' and 'y'
        image_df$quadrant <- ifelse(pixel_predictions == "Background", 0, 1)
        
        # Post-process the image to remove noise and prepare for contour extraction
        quadrant_df <- image_df %>%
            dplyr::select(x, y, quadrant) %>%
            rename(value = quadrant)
        
        # Convert quadrant data frame to image format for morphological operations
        quadrant_image <- as.cimg(quadrant_df)
        opened_image <- mopening_square(quadrant_image, 3)  # Perform morphological opening
        opened_channels <- channels(opened_image)
        opened_df <- as.data.frame(opened_channels)
        opened_df <- opened_df[order(opened_df$x, opened_df$y), ]
        image_df$processed_quadrant <- opened_df$value
        
        # Identify pixels within the processed quadrant
        refined_pixels <- subset(image_df, processed_quadrant == 1)
        
        # Further refine quadrant information by performing another morphological opening
        refined_quadrant_df <- image_df %>%
            dplyr::select(x, y, quadrant) %>%
            rename(value = quadrant)
        
        refined_quadrant_image <- as.cimg(refined_quadrant_df)
        final_opened_image <- mopening_square(refined_quadrant_image, 3)
        image_df$refined_quadrant <- as.data.frame(channels(final_opened_image))$value
        
        # Extract frame contours from the refined quadrant
        contour_pixels <- image_df %>% filter(refined_quadrant == 1)
        
        # Create a triangular mesh for convex hull computation
        mesh <- tri.mesh(contour_pixels$x, contour_pixels$y)
        image_df$in_convex_hull <- in.convex.hull(mesh, image_df$x, image_df$y)
        convex_contour_pixels <- subset(image_df, in_convex_hull == TRUE)
        
        # Identify the four corners of the trapezoidal frame based on geometric properties
        corner_points <- rbind(
            convex_contour_pixels[which.max(convex_contour_pixels$y - convex_contour_pixels$x), ],
            convex_contour_pixels[which.max(convex_contour_pixels$y + convex_contour_pixels$x), ],
            convex_contour_pixels[which.min(convex_contour_pixels$y - convex_contour_pixels$x), ],
            convex_contour_pixels[which.min(convex_contour_pixels$y + convex_contour_pixels$x), ]
        )
        corner_matrix <- as.matrix(corner_points[, c("x", "y")])
        
        # Calculate the scaling factor based on the distance between the first two corner points
        scaling_factor <- sqrt((corner_matrix[1, "x"] - corner_matrix[2, "x"])^2 + 
                                   (corner_matrix[1, "y"] - corner_matrix[2, "y"])^2)
        print(paste("Scaling Factor (pxl1.5m):", scaling_factor))
        
        # Define destination points for homography based on the scaling factor
        destination_points <- rbind(
            c(min(corner_matrix[, "x"]), 
                ifelse(min(corner_matrix[, "y"])+scaling_factor/1.5>= max(image_df$y), 
                    max(image_df$y), min(corner_matrix[, "y"])+scaling_factor/1.5)),
            c(min(corner_matrix[, "x"]) + scaling_factor, 
                ifelse(min(corner_matrix[, "y"]) + scaling_factor / 1.5 >= max(image_df$y), 
                    max(image_df$y), min(corner_matrix[, "y"]) + scaling_factor / 1.5)),
            c(min(corner_matrix[, "x"]) + scaling_factor, 
                ifelse(min(corner_matrix[, "y"]) + scaling_factor / 1.5 >= max(image_df$y), 
                    max(image_df$y) - scaling_factor / 1.5, min(corner_matrix[, "y"]))),
            c(min(corner_matrix[, "x"]), 
                ifelse(min(corner_matrix[, "y"]) + scaling_factor / 1.5 >= max(image_df$y), 
                    max(image_df$y) - scaling_factor / 1.5, min(corner_matrix[, "y"]))))
        
        # Initialize lists to store matrices for homography calculation
        homography_A <- list()
        homography_b <- list()
        
        # Build matrices for solving the homography transformation
        for (i in 1:4) {
            homography_A[[paste0(i)]] <- matrix(c(
                corner_matrix[i, "x"], corner_matrix[i, "y"], 1, 0, 0, 0, 
                -destination_points[i, 1] * corner_matrix[i, "x"], 
                -destination_points[i, 1] * corner_matrix[i, "y"],
                0, 0, 0, corner_matrix[i, "x"], corner_matrix[i, "y"], 1, 
                -destination_points[i, 2] * corner_matrix[i, "x"], 
                -destination_points[i, 2] * corner_matrix[i, "y"]
            ), nrow = 2, byrow = TRUE)
            homography_b[[paste0(i)]] <- matrix(c(destination_points[i, 1], 
                                                  destination_points[i, 2]), 
                                                nrow = 2, byrow = TRUE)
        }
        
        # Combine all homography matrices into a single system of equations
        homography_matrix_A <- do.call(rbind, homography_A)
        homography_matrix_b <- do.call(rbind, homography_b)
        
        # Solve the system to obtain the homography vector
        homography_vector <- solve(homography_matrix_A, homography_matrix_b)
        homography_matrix <- matrix(c(homography_vector, 1), 3, 3, byrow = TRUE)
        
        # Define a function for inverse perspective mapping using the homography matrix
        inverse_perspective_map <- function(x_coords, y_coords, H_matrix) {
            transformed <- H_matrix %*% rbind(x_coords, y_coords, 1)
            list(x = transformed[1, ] / transformed[3, ],
                 y = transformed[2, ] / transformed[3, ])
        }
        
        # Apply the inverse perspective transformation to the resized image
        warped_image <- imwarp(resized_image, 
            map = function(x, y) inverse_perspective_map(x, y, solve(homography_matrix)),
            coordinates = "absolute", direction = "backward")
        
        # Crop the warped image to the frame boundaries defined by destination points
        cropped_image<-as.cimg(warped_image[min(destination_points[, 1]):max(destination_points[, 1]),
                                              min(destination_points[, 2]):max(destination_points[, 2]),,])
        
        # Save the cropped image in PNG format with the unique image identifier
        imager::save.image(cropped_image, paste0("./out/CroppedFrame/", 
                                                 unique_image_id, ".png"))
        
    }, error = function(e) {
        # Handle any errors that occur during processing and print the problematic image ID
        print(paste("Error processing image:", unique_image_id))
    })
}

# Execute Frame Detection Function in Parallel ---------------------------------
# Determine the number of available CPU cores and set up a cluster
total_cores <- detectCores()
available_cores <- max(total_cores - 5, 1)  # Ensure at least one core is used
image_processing_cluster <- makeCluster(getOption("cl.cores", available_cores))

# Load necessary libraries on each cluster node
clusterEvalQ(image_processing_cluster, {
    library(imager)
    library(tidyverse)
    library(tripack)
    library(colorscience)
    library(randomForest)
})

# Export necessary variables and functions to the cluster nodes
clusterExport(image_processing_cluster, varlist = c("FrameDetection"))

# Apply the FrameDetection function to all image files in parallel
parLapply(image_processing_cluster, image_files, FrameDetection)

# Terminate the cluster after processing is complete
stopCluster(image_processing_cluster)
