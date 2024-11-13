# ------------------------------------------------------------------------------
# R Script for Analyzing Plant Emergence Data
# ------------------------------------------------------------------------------

# --------------------------- 1. Load Required Libraries -----------------------
# Install and load pacman if not already installed
if (!require("pacman")) install.packages("pacman")
library(pacman)

# Use pacman to load/install required packages
pacman::p_load(
  tidyverse,      # Includes dplyr, ggplot2, readr, etc.
  drc,            # Dose-Response Curves
  FactoMineR,     # Principal Component Analysis
  factoextra      # Visualization for PCA
)


# ------------------------------ 2. Load Data ----------------------------------
# Read the 'dating.csv' file containing emergence dates and related information
dating <- read.table("./data/dating.csv",  stringsAsFactors = FALSE,
  header = TRUE,  sep = ";",  dec = ",",  strip.white = TRUE)

# Read the 'planting.csv' file containing planting dates and related information
pl <- read.csv("./data/planting.csv",  stringsAsFactors = FALSE,  header = TRUE,
               sep = ";",  dec = ",",  strip.white = TRUE)

# Read the 'count.csv' file containing observation counts
count <- read.table("./data/count.csv",  stringsAsFactors = FALSE,  header=TRUE,
                    sep = ";",  dec = ",",  strip.white = TRUE)


# --------------------- 3. Format Dates and Calculate DOY ----------------------
# Convert 'Planting_Date' to Date format and calculate Day of Year (DOY)
pl$Planting_Date <- as.Date(pl$Planting_Date, format = "%d/%m/%Y")
pl$DOY_Planting_Date <- as.numeric(format(pl$Planting_Date, "%j"))

# Convert 'Emergence_Date' to Date format and calculate DOY
dating$Emergence_Date <- as.Date(as.character(dating$Emergence_Date), format="%d/%m/%Y")
dating$DOY_Emergence_Date <- as.numeric(format(dating$Emergence_Date, "%j"))

# Summarize mean DOY of emergence grouped by Year and Site
datings <- dplyr::summarize(
  group_by(dating, Year, Site),
  Mean = mean(DOY_Emergence_Date, na.rm = TRUE)
)

# Convert 'Observation_Date' to Date format and calculate DOY
count$Observation_Date <- as.Date(as.character(count$Observation_Date), format = "%d/%m/%Y")
count$DOY_Observation_Date <- as.numeric(format(count$Observation_Date, "%j"))


# ------------------- 4. Plot Emergence Histogram by Year/Site -----------------
# Create a histogram of emergence dates colored by Site and faceted by Year
png(height=6,  width=10, res=300, filename= "./out/Emergence_Histogram.png",
  units = "in",  type = "cairo",  family = "Garamond")

ggplot(dating, aes(DOY_Emergence_Date, fill = Site)) +
  geom_histogram(alpha = 0.5) +
  theme_bw(base_size = 20) +
  geom_rug() +
  xlab("Emergence Day of the Year (DOY)") +
  ylab("Emergence Count") +
  theme(legend.position = "bottom") +
  # Add dashed vertical lines for mean DOY of emergence
  geom_vline(data = datings, aes(xintercept = as.numeric(Mean), color = Site),
    lty = "dashed", linewidth = 1.2) +
  # Add text labels for mean DOY
  geom_text(data = datings, aes(x = Mean + 3, y = 100, label = round(Mean, 0),
                                color = Site), hjust = 0, size = 6) +
  facet_grid(Year ~ .)

dev.off()


# --------------------- 5. Plot Mean Emergence by Var/Site ---------------------
# Calculate mean and standard deviation of DOY of emergence grouped by Year, Site, and Var
datings <- dplyr::summarize(
  group_by(dating, Year, Site, Var),
  Mean = mean(DOY_Emergence_Date, na.rm = TRUE),
  SD = sd(DOY_Emergence_Date, na.rm = TRUE)
)

# Create a bar plot of mean emergence DOY with error bars representing SD
png(height=11, width=7, res=300, filename="./out/Mean_Emergence_DOY.png",
  units = "in",  type = "cairo",  family = "Garamond")

ggplot(datings, aes(reorder(Var, Mean), Mean, fill = Site)) +
  geom_bar(position = "dodge", alpha = 0.5, stat = "identity") +
  theme_bw(base_size = 16) +
  # Add error bars for standard deviation
  geom_linerange(position=position_dodge(.9), aes(ymin=Mean, ymax=Mean+SD)) +
  coord_flip() +
  facet_grid(. ~ Year) +
  ylab("Emergence Day of the Year (DOY)") +
  theme(legend.position = "bottom", axis.title.y = element_blank())

dev.off()


# ----------------------- 6. Calculate Emergence Rate --------------------------
# Summarize the number of emergences and total plant number by Year, Site, and Var
sum <- summarise(
  group_by(dating, Year, Site, Var),
  Nb_emerg = sum(!is.na(Emergence_Date)),
  PlantNb = max(PlantNb)
)

# Merge with planting data to include planting information
sum <- left_join(sum, pl, by = c("Year", "Site", "Var", "PlantNb"))

# Calculate percentage of emergence
sum <- dplyr::mutate(
  group_by(sum, Year, Site, Var),
  pEmerg = round(Nb_emerg / min(PlantNb, PlantNb, na.rm = TRUE) * 100, 1)
)


# ------------------- 7. Calculate Cumulative Emergence by Observation Date ----
# Subset to records with non-missing Emergence_Date
datingNA <- subset(dating, !is.na(Emergence_Date))

# Summarize emergence counts by various grouping variables
emergence <- dplyr::summarise(
  group_by(datingNA, Year, Site, Var, Emergence_Date, DOY_Emergence_Date),
  N = n(),  PlantNb = mean(PlantNb)
)

# Calculate cumulative sum and emergence rate
emergence <- dplyr::mutate(
  group_by(emergence, Year, Site, Var),
  Nc = as.numeric(cumsum(N)),
  N = as.numeric(N),
  Emergence = Nc / PlantNb
)

# Arrange and mutate 'count' data for merging
count <- arrange(count, Year, Site, Var, DOY_Observation_Date)
count <- mutate(
  group_by(count, Year, Var, Site),
  Nc = EmergenceNb,
  # Calculate number of emergences since last observation
  EmergenceNb = ifelse(is.na(lag(Nc)), Nc, Nc - lag(Nc)),
  Emergence = Nc / PlantNb
)

# Select relevant columns and rename them to match 'emergence' data
count <- dplyr::select(count,  Year:Var,  Observation_Date, DOY_Observation_Date, 
                       EmergenceNb,  PlantNb,  Nc,  Emergence)
colnames(count) <- colnames(emergence)

# Combine 'emergence' and 'count' data
emergence <- rbind(ungroup(emergence),  ungroup(count))


# ------------------- 8. Prepare Data for Time-to-Event Analysis ---------------
# Merge with planting data to include planting dates
emergence <- ungroup(left_join(emergence, pl, by = c("Year", "Site", "Var", "PlantNb")))

# Calculate start and end DOY for each emergence event
emergence <- mutate(group_by(emergence, Year, Site, Var),
                    Start = ifelse(is.na(lag(DOY_Emergence_Date)),
                                   DOY_Planting_Date,  lag(DOY_Emergence_Date)),
                    End = DOY_Emergence_Date)

# Select relevant columns for time-to-event analysis
emergence <- dplyr::select(emergence,  Year:Var,  DOY_Emergence_Date,
                           DOY_Planting_Date,  Start,  End,  N,  Nc,  PlantNb)

# Adjust DOY values relative to planting date
emergence <- mutate(ungroup(emergence),  
                    DOY_Planting_Date = ifelse(is.na(DOY_Planting_Date),
                                               lag(DOY_Planting_Date),
                                               DOY_Planting_Date),
  End = End - DOY_Planting_Date,
  Start = Start - DOY_Planting_Date
)


# ------------------- 9. Add Initial Observation Post-Planting -----------------
# Insert a null observation one day after planting for each group
emergence %>% 
  group_by(Year, Site, Var) %>% 
  do(add_row(., .before = TRUE)) -> emergence

# Fill in missing values and adjust start/end dates
emergence <- mutate(
  group_by(emergence, Year, Site, Var),
  N = ifelse(is.na(N), 0, N),
  Start = ifelse(Start == 0, 1, Start),
  Start = ifelse(is.na(Start), 0, Start),
  End = ifelse(is.na(End), 1, End),
  N = ifelse(is.na(N), 0, N),
  PlantNb = ifelse(is.na(PlantNb), lead(PlantNb), PlantNb)
)

# Fill in remaining missing grouping variables
emergence <- mutate(
  ungroup(emergence),
  Site = ifelse(is.na(Site), lead(Site), Site),
  Year = ifelse(is.na(Year), lead(Year), Year),
  Var = ifelse(is.na(Var), lead(Var), Var),
  PlantNb = ifelse(is.na(PlantNb), lead(PlantNb), PlantNb)
)


# ------------------- 10. Add Final Observation with Infinite Date -------------
# Append a final observation with an infinite end date for each group
emergence %>% 
  group_by(Year, Site, Var) %>% 
  do(add_row(.)) -> emergence

# Fill in missing values for the final observation
emergence <- mutate(
  ungroup(emergence),
  Site = ifelse(is.na(Site), lag(Site), Site),
  Year = ifelse(is.na(Year), lag(Year), Year),
  Var = ifelse(is.na(Var), lag(Var), Var),
  PlantNb = ifelse(is.na(PlantNb), lag(PlantNb), PlantNb)
)

# Update start and end dates, cumulative counts, and emergences
emergence <- mutate(
  group_by(emergence, Year, Site, Var),
  N = ifelse(is.na(N), PlantNb - lag(Nc), N),
  Start = ifelse(is.na(Start), lag(End), Start),
  End = ifelse(is.na(End), Inf, End),
  Nc = cumsum(N)
)


# ------------------- 11. Filter Combinations with Sufficient Observations -----
# Create unique codes for Site, Year, and Var combinations
emergence$code <- paste(emergence$Site, emergence$Year, emergence$Var, sep = "_")
emergence$code0 <- paste(emergence$Site, emergence$Year, sep = "_")

# Define color palette for plotting
colorS <- c('#33a02c', '#ff7f00', '#6a3d9a')


# ------------------- 12. Plot Observed Emergence Dynamics ---------------------
# Create a scatter plot of observed emergence rates over time
png(height = 11,  width = 14,  res = 300, units = "in",  type = "cairo", 
    filename = "./out/ObservedEmergenceDynamic.png",  family = "Garamond")

ggplot(emergence, aes(End, Nc / PlantNb, color = code0)) +
  geom_point() +
  facet_wrap(~Var) +
  theme_bw(base_size = 18) +
  xlab("Days After Planting") +
  ylab("Emergence Rate") +
  scale_color_manual("Site - Year", values = colorS)

dev.off()


# ----------------------- 13. Fit Event-Time Models ----------------------------
# Initialize an empty list to store model fits
g <- list()

# Iterate over each unique Site-Year-Var combination to fit models
for (asitevar in unique(emergence$code)) {
  fit <- tryCatch({
    drm(N ~ Start + End,  data = subset(emergence, code == asitevar),
      fct = LL.3(),  type = "event",  upperl = c(Inf, 1, Inf))
  }, error = function(cond) { return(cond) })
  
  # Store the fit or error in the list
  fit <- list(fit)
  names(fit) <- asitevar
  g <- c(g, fit)
}

# Example plot for the 35th model (adjust index as needed)
plot(g[[35]])


# ------------------- 14. Extract Model Parameters and Results -----------------
# Initialize a dataframe to store results
res <- data.frame(code = NA,  Steepness = NA,  MaxEmRate = NA,  MedianEm = NA,
                  em90 = NA,  em95 = NA,  FirstEm = NA)

# Loop through each model fit to extract parameters
i <- 0
for (afit in g) {
  i <- i + 1
  # Check if the fit was successful by verifying parameter length
  if (length(afit) == 30) {  # Adjust condition based on actual structure
    res[i, ] <- c(
      names(g[i]),
      afit[["fit"]][["par"]],
      ED(afit, c(95, 97.5)) - ED(afit, c(5, 2.5)),
      ED(afit, 5)
    )  } else { next  # Skip if the model did not converge
  }
}

# Remove rows with any missing values
res <- res[rowSums(is.na(res)) == 0, ]

# Separate the 'code' into 'Site', 'Year', and 'Var'
res <- separate(res, code, c("Site", "Year", "Var"), sep = "_", remove = FALSE)

# Convert relevant columns to numeric
res[, 5:10] <- lapply(res[, 5:10], as.numeric)


# ------------------- 15. Predict Values for Plotting --------------------------
# Create a dataframe with unique codes for predictions
v <- data.frame(code = unique(res$code))
v <- separate(v, code, c("Site", "Year", "Var"), sep = "_", remove = FALSE)

# Duplicate rows to cover the range of Start times (0 to 127 days)
i <- 1
L <- nrow(v)
repeat {
  v <- rbind(v, v)
  i <- i + 1
  if (i == 8) { break }
}

# Assign Start and End days for prediction
v$Start <- rep(0:127, each = L)
v$End <- rep(c(1:127, Inf), each = L)

# Merge with model results
v <- left_join(v, res, by = c("code", "Year", "Site", "Var"))

# Predict emergence rates using the fitted models
for (i in 1:nrow(v)) {
  v[i, "Ncpred"] <- predict(g[[v$code[i]]], newdata = v[i, c("Start", "End")])
}

# Ensure 'Year' is integer and merge with 'emergence' data
v$Year <- as.integer(v$Year)
v <- left_join(v, emergence, by = c("code", "Year", "Site", "Var", "End"))


# ------------------- 16. Calculate Percentage of Converged Models -------------
# Calculate the number of unique observations and models
n_obs <- length(unique(paste0(v$Var, v$Year, v$Site)))
n_mod <- length(unique(paste0(emergence$Var, emergence$Year, emergence$Site)))

# Compute the percentage of models that converged
convergence_percentage <- n_obs / n_mod * 100
print(convergence_percentage)


# ------------------- 17. Visualize Predicted vs Observed Emergence ------------
# Plot the dynamic emergence curves with predictions overlaid on observations
png(height=9, width=13, res=300, filename="./out/DynamiqueEmergenceLL3.png",
  units = "in",  type = "cairo",  family = "Garamond")

ggplot(ungroup(v)) +
  geom_point(aes(End, Nc / PlantNb, color = paste0(Site, Year))) +
  geom_line(aes(End, Ncpred, color = paste0(Site, Year))) +
  facet_wrap(~Var) +
  theme_bw() +
  xlab("Number of Days After Planting") +
  ylab("Emergence Rate") +
  scale_color_manual("Site - Year", values = colorS)

dev.off()


# ------------------- 18. Specific Visualization for 2021 Oriental Variety -----
# Subset data for Year 2021 and Var "H2X74F"
v2021o <- subset(v, Year == 2021 & Var == "H2X74F")

# Plot emergence dynamics specifically for the 2021 Oriental variety
png(height=9, width=13, res=300, units="in", type="cairo", family="Garamond",
    filename = "./out/DynamiqueEmergenceLL3_2021_Oriental.png")

ggplot(ungroup(v2021o)) +
  geom_point(aes(End, Nc/PlantNb, color=paste0(Site, Year)), size=5, alpha=.5) +
  geom_line(aes(End, Ncpred, color = paste0(Site, Year))) +
  theme_bw(base_size = 36) +
  xlab("Number of Days After Planting") +
  ylab("Emergence Rate") +
  scale_color_manual("Site", values=c('#e31a1c', '#1f78b4'), 
                     labels=c("Godet", "Roujol")) +
  theme(legend.position = "bottom") +
  xlim(0, 75)

dev.off()


# ------------------- 19. Estimate Emergence Parameters for Non-Converged Models -------
# Calculate median emergence DOY and other observed metrics grouped by Year, Var, and Site
emergences <- summarize(
  group_by(ungroup(emergence), Year, Var, Site),
  DOYemMean = median(DOY_Emergence_Date - DOY_Planting_Date, na.rm = TRUE),
  MaxEmRate_obs = nth(Nc, -2) / last(PlantNb),
  FirstEm_obs = min(DOY_Emergence_Date, na.rm = TRUE) - min(DOY_Planting_Date, na.rm = TRUE)
)

# Handle specific cases where data should be set to NA
emergences$DOYemMean <- ifelse(emergences$Year == 2017, NA, emergences$DOYemMean)
emergences$FirstEm_obs <- ifelse(emergences$Site == 2017, NA, emergences$FirstEm_obs)

# Convert 'Year' to character for merging
emergences$Year <- as.character(emergences$Year)

# Merge with previously extracted model results
res <- left_join(emergences, res, by = c("Year", "Site", "Var"))

# Assign median emergence DOY where model did not converge
res$DOYem <- ifelse(is.na(res$MedianEm), res$DOYemMean, res$MedianEm)

# Calculate latency as the minimum of observed first emergence and model-based first emergence
res$Latency <- pmin(res$FirstEm_obs, res$FirstEm, na.rm = TRUE)

# Invert steepness for interpretability (optional based on model parameterization)
res$Steepness <- -res$Steepness


# ------------------- 20. Save Results to Files ---------------------------------------
# Ungroup the results dataframe for saving
res <- ungroup(res)

# Save the results as a CSV file
write.csv(res, "./out/emergence50.csv")

# Save the results and models as RDS files for later use
saveRDS(res, "./out/em50.RDS")
saveRDS(g, "./out/allModels.RDS")


# ------------------- 21. Summarize Emergence Dynamic Parameters ---------------------
# Summarize the number and percentage of converged models by Var
ress <- dplyr::summarize(group_by(res, Var), Nsy = n(),
                         Nconverged = Nsy - sum(is.na(Steepness)),
                         pConv = Nconverged / Nsy * 100)

# Reshape the results for visualization
resg <- pivot_longer(res, names_to = "Measure", values_to = "Value",
  c(MaxEmRate_obs, Latency, Steepness, em90, em95))

# Remove rows with missing values
resg <- subset(resg, !is.na(Value))

# Create a more descriptive factor for measures
resg$Measure2 <- as.factor(resg$Measure)
resg$Measure2 <- recode_factor(
  resg$Measure2, em90 = "90% Emergence Duration (days)",
  em95 = "95% Emergence Duration (days)", Latency = "Latency (days)",
  MaxEmRate_obs = "Emergence Rate", Steepness = "Steepness (Hill Slope)")

# Filter out unrealistic values
resg <- subset(resg, !(Measure == "MaxEmRate_obs" & Value > 1))
resg <- subset(resg, !(Measure == "Latency" & Value < 0))


# ------------------- 22. Plot Dispersion of Emergence Parameters ---------------------
# Create a boxplot with overlaid points to show dispersion of emergence parameters
png(height=9, width=13,  res=300, filename="./out/EmDynamicParamDispersion.png",
  units = "in",  type = "cairo",  family = "Garamond")

ggplot(subset(resg, Measure2 != "em95"), aes(x = reorder(Var, Value), y = Value)) +
  geom_boxplot(outlier.alpha = 0) +
  geom_point(alpha = 0.5, aes(color = Site)) +
  theme_bw(base_size = 18) +
  coord_flip() +
  facet_grid(. ~ Measure2, scales = "free_x") +
  theme(axis.title = element_blank(), legend.position = "bottom",
    axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) +
  scale_color_manual(values = c('#1b9e77', '#d95f02', '#7570b3'))

dev.off()


# ------------------- 23. Perform Principal Component Analysis (PCA) ------------------
# Select relevant columns for PCA and remove incomplete cases
resPCA <- dplyr::select(res, Site, Var, MaxEmRate_obs, Steepness, em90, Latency)
resPCA <- resPCA[complete.cases(resPCA), ]

# Conduct PCA on the selected emergence parameters
res.pca <- PCA(resPCA[, 3:6], scale.unit = TRUE, ncp = 5, graph = TRUE)

# Plot the PCA biplot
png(height = 4,  width = 5,  res = 300,  filename = "./out/PCA12.png",
  units = "in",  type = "cairo",  family = "Garamond")

fviz_pca_biplot(res.pca,
  col.ind = resPCA$Site,           # Color individuals by Site
  palette = "jco",                 # Color palette
  label = "var",                   # Label variables
  col.var = "black",               # Color variables in black
  repel = TRUE,                    # Avoid overlapping labels
  legend.title = "Site",
  alpha.ind = 0.5) +               # Transparency for points
  theme_bw(base_size = 12)

dev.off()
