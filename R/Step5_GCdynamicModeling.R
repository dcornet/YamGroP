# Install necessary packages if not already installed -------------------------
packs <- c("tidyverse", "drc", "scales")
InstIfNec <- function(pack) {
  if (!do.call(require, as.list(pack))) {
    do.call(install.packages, as.list(pack))
  }
  do.call(require, as.list(pack))
}
lapply(packs, InstIfNec)

# Data Preparation ------------------------------------------------------------
# Load data
PlantingDate <- "220602"
DOY_Plant <- as.numeric(format(as.Date(PlantingDate, "%y%m%d"), "%j"))
res <- read.csv2("./out/Yam_pRec.csv", sep = ";")
res$Date_Rec <- as.Date(as.character(res$Date), "%y%m%d")
res$Rep <- gsub(" ", "", res$Rep)

# Plot the data
png(height = 9, width = 15, res = 300, units = "in", type = "cairo", family = "Garamond",
    filename = "./out/GroundCoverDynamic_observations.png")
ggplot(data = res, aes(x = Date_Rec, y = pRec)) +
  geom_smooth(se = FALSE) + theme_bw(base_size = 16) +
  geom_point() +
  xlab("Day of the year") + ylab("Yam ground cover (%)") +
  scale_x_date(labels = date_format("%d %b")) +
  facet_wrap(~Var)
dev.off()

# Add initial ground cover data (t0 => pRec = 0)
res %>%
  as_tibble() %>%
  group_by(Var, Rep) %>%
  group_modify(~ add_row(.x, .before = 0)) -> res
res$Date <- ifelse(is.na(res$Date), PlantingDate, res$Date)
res$pRec <- ifelse(is.na(res$pRec), 0, res$pRec)
res$Date_Rec <- as.Date(as.character(res$Date), "%y%m%d")
res$Var <- as.factor(res$Var)
res$Rep <- as.factor(res$Rep)
res$DayDOY <- as.numeric(format(res$Date_Rec, "%j"))
res$DayDOY <- ifelse(res$DayDOY < 100, res$DayDOY + 365, res$DayDOY)
res$code <- paste(res$Var, res$Rep, sep = "_")

# Extract growth phase of the ground cover dynamics
res %>%
  group_by(Var) %>%
  mutate(max_pRec_date = DayDOY[which.max(pRec)],
         max_pRec = max(pRec)) %>%
  filter(DayDOY <= max_pRec_date) %>%
  ungroup() -> res0
res0 %>%
  as_tibble() %>%
  group_by(Var) %>%
  group_modify(~ add_row(.x, .before = 0)) -> res0
res0 <- mutate(group_by(res0, Var),
               DayDOY = ifelse(is.na(DayDOY), mean(max_pRec_date, na.rm = TRUE) + 5, DayDOY),
               pRec = ifelse(is.na(pRec), mean(max_pRec, na.rm = TRUE), pRec))
res0 %>%
  as_tibble() %>%
  group_by(Var) %>%
  group_modify(~ add_row(.x, .before = 0)) -> res0
res0 <- mutate(group_by(res0, Var),
               DayDOY = ifelse(is.na(DayDOY), mean(max_pRec_date, na.rm = TRUE) + 10, DayDOY),
               pRec = ifelse(is.na(pRec), mean(max_pRec, na.rm = TRUE), pRec))
res0 %>%
  as_tibble() %>%
  group_by(Var) %>%
  group_modify(~ add_row(.x, .before = 0)) -> res0
res0 <- mutate(group_by(res0, Var),
               DayDOY = ifelse(is.na(DayDOY), mean(max_pRec_date, na.rm = TRUE) + 50, DayDOY),
               pRec = ifelse(is.na(pRec), mean(max_pRec, na.rm = TRUE), pRec))


# Fit sigmoid models -----------------------------------------------------------
# Get upper limit based on maximum observed ground cover
var_levels <- unique(res$Var)
up_limits <- summarize(group_by(res, Var), GCx=max(pRec))$GCx
upll <- c()
for (i in 1:length(var_levels)) {
  upll <- c(upll, Inf, up_limits[i], 300)
}

# Fit various sigmoid models
sig_LL3 <- drm(pRec ~ DayDOY, curveid = Var, data = res0, fct = LL.3(), upperl = upll)
sig_L3 <- drm(pRec ~ DayDOY, curveid = Var, data = res0, fct = L.3(), upperl = upll)
sig_E3 <- drm(pRec ~ DayDOY, curveid = Var, data = res0, fct = G.3u(), upperl = upll)
sig_G3 <- drm(pRec ~ DayDOY, curveid = Var, data = res0, fct = G.3(), upperl = upll)
sig_W13 <- drm(pRec ~ DayDOY, curveid = Var, data = res0, fct = W1.3(), upperl = upll)
sig_W23 <- drm(pRec ~ DayDOY, curveid = Var, data = res0, fct = W2.3())
sig_LN3 <- drm(pRec ~ DayDOY, curveid = Var, data = res0, fct = LN.3())

# Model choice based on AIC and BIC
ModChoice <- data.frame(model = c("sig_LL3", "sig_L3", "sig_E3", "sig_G3",
                                  "sig_W13", "sig_W23", "sig_LN3"),
                        AIC = c(AIC(sig_LL3), AIC(sig_L3), AIC(sig_E3),
                                AIC(sig_G3), AIC(sig_W13), AIC(sig_W23), AIC(sig_LN3)),
                        BIC = c(BIC(sig_LL3), BIC(sig_L3), BIC(sig_E3),
                                BIC(sig_G3), BIC(sig_W13), BIC(sig_W23), BIC(sig_LN3)))
BestModel <- ModChoice[which.min(ModChoice$BIC), "model"]
plot(get(BestModel))
saveRDS(get(BestModel), "./out/BestModel_growth.rds")

# Extract model parameters by genotype
df <- as.data.frame(t(get(BestModel)$parmMat))
df$Var <- row.names(df)
df <- bind_cols(df, ED(get(BestModel), 5), ED(get(BestModel), 50), ED(get(BestModel), 95))
colnames(df) <- c("growth_b_Slope", "growth_d_MaxRec", "growth_e_TimeTo50", "Var", 
                  "growth_ED5", "growth_sdED5", "growth_ED50", "growth_sdED50", "growth_ED95", "growth_sdED95")

# Add key traits
df$Latency <- ED(get(BestModel), 3)[,1] - DOY_Plant
df$Growth <- df$growth_ED95 - df$growth_ED5

# Visualization of model fit
newdata <- expand.grid(DayDOY = c(100:410), Var = levels(as.factor(res$Var)))
pm <- predict(get(BestModel), newdata = newdata, interval = "confidence")
newdata$p <- pm[,1]
png(height = 9, width = 15, res = 300, units = "in", type = "cairo", family = "Garamond",
    filename = "./out/GroundCoverDynamic_growth.png")
ggplot(res, aes(x = DayDOY, y = pRec)) +
  geom_line(data = newdata, aes(x = DayDOY, y = p), color = "#4daf4a", linewidth = 1.2) +
  facet_wrap(Var ~ .) +
  xlab("Day of the year") + ylab("Yam ground cover (%)") +
  theme_bw(base_size = 15) +
  geom_vline(data = df, aes(xintercept = DOY_Plant), color = "#4daf4a", linetype = "dashed") + 
  geom_vline(data = df, aes(xintercept = growth_ED5), color = "#4daf4a", linetype = "dashed") + 
  geom_segment(data = df, arrow = arrow(length = unit(0.1, "inches"), ends = "both"), color = "#4daf4a",
               aes(x = DOY_Plant, y = growth_d_MaxRec + 5, xend = growth_ED5, yend = growth_d_MaxRec + 5)) +
  geom_text(data = df, color = "#4daf4a", 
            aes(x = DOY_Plant + (growth_ED5 - DOY_Plant) / 2, y = growth_d_MaxRec + 10, label = "Latency")) +
  geom_point(size = 3, alpha = .8) +
  theme(legend.position = "none")
dev.off()

# Plateau Detection ------------------------------------------------------------
# Parameters 
change_threshold <- 0.25
LoesSPAN <- 0.6

# Function to detect plateau near peak
detect_plateau_near_peak <- function(vector, change_threshold) {
  peak_index <- which.max(vector) # Find the index of the peak value
  # Initialize start and end indices of the plateau
  start_index <- peak_index
  end_index <- peak_index
  # Expand to the left from the peak
  for (i in (peak_index - 1):1) {
    if (abs(vector[i] - vector[i + 1]) <= change_threshold) {
      start_index <- i
    } else { break }
  }
  # Expand to the right from the peak
  for (i in (peak_index + 1):length(vector)) {
    if (abs(vector[i] - vector[i - 1]) <= change_threshold) {
      end_index <- i
    } else { break }
  }
  return(list(start_index = start_index, end_index = end_index))
}

# Apply plateau detection function
for (i in 1:nrow(df)) {
  re <- subset(res, Var == df$Var[i])
  print(df$Var[i])
  Lgth0 <- max(re$DayDOY)
  lissDAE <- seq(DOY_Plant, Lgth0, 1)
  fit <- loess(pRec ~ DayDOY, data = re, span = LoesSPAN)
  pRec <- predict(fit, lissDAE, se = FALSE)
  Lpred <- cbind.data.frame(Var = df$Var[i], DayDOY = lissDAE, pRec = pRec)
  cord <- detect_plateau_near_peak(Lpred$pRec, change_threshold)
  df$Plateau_n[i] <- ifelse(is.null(cord$start_index), NA, cord$start_index + DOY_Plant)
  df$Plateau_x[i] <- ifelse(is.null(cord$end_index), NA, cord$end_index + DOY_Plant)
}
df$Plateau <- df$Plateau_x - df$Plateau_n

# Visualization of plateau detection
png(height = 9, width = 15, res = 300, units = "in", type = "cairo", family = "Garamond",
    filename = "./out/GroundCoverDynamic_plateau.png")
ggplot(res, aes(x = DayDOY, y = pRec)) +
  geom_smooth(method = "loess", color = "#d95f02", alpha = 0.2, span = LoesSPAN, se = FALSE) +
  geom_vline(data = df, aes(xintercept = Plateau_n), color = "#d95f02", linetype = "dashed") + 
  geom_vline(data = df, aes(xintercept = Plateau_x), color = "#d95f02", linetype = "dashed") + 
  geom_segment(data = df, arrow = arrow(length = unit(0.1, "inches"), ends = "both"), color = "#d95f02",
               aes(x = Plateau_n, y = growth_d_MaxRec + 5, xend = Plateau_x, yend = growth_d_MaxRec + 5)) +
  geom_text(data = df, color = "#d95f02", 
            aes(x = Plateau_n + (Plateau_x - Plateau_n) / 2, y = growth_d_MaxRec + 12, label = "Plateau")) +
  facet_wrap(Var ~ .) +
  xlab("Day of the year") + ylab("Yam ground cover (%)") +
  geom_point(size = 3, alpha = .8) +
  theme_bw(base_size = 15) +
  ylim(0, 100)
dev.off()

# Senescence -------------------------------------------------------------------
# Extract senescence phase of the observations
res %>%
  group_by(Var) %>%
  mutate(max_pRec_date = DayDOY[which.max(pRec)],
         max_pRec = max(pRec)) %>%
  filter(DayDOY >= max_pRec_date) %>%
  ungroup() -> res1
res1 %>%
  as_tibble() %>%
  group_by(Var) %>%
  group_modify(~ add_row(.x, .before = 0)) -> res1
res1 <- mutate(group_by(res1, Var),
               DayDOY = ifelse(is.na(DayDOY), 420, DayDOY), 
               pRec = ifelse(is.na(pRec), 0, pRec))
res1 %>%
  as_tibble() %>%
  group_by(Var) %>%
  group_modify(~ add_row(.x, .before = 0)) -> res1
res1 <- mutate(group_by(res1, Var),
               DayDOY = ifelse(is.na(DayDOY), mean(max_pRec_date, na.rm = TRUE) - 5, DayDOY),
               pRec = ifelse(is.na(pRec), mean(max_pRec, na.rm = TRUE), pRec))
res1 %>%
  as_tibble() %>%
  group_by(Var) %>%
  group_modify(~ add_row(.x, .before = 0)) -> res1
res1 <- mutate(group_by(res1, Var),
               DayDOY = ifelse(is.na(DayDOY), mean(max_pRec_date, na.rm = TRUE) - 10, DayDOY),
               pRec = ifelse(is.na(pRec), mean(max_pRec, na.rm = TRUE), pRec))
res1 %>%
  as_tibble() %>%
  group_by(Var) %>%
  group_modify(~ add_row(.x, .before = 0)) -> res1
res1 <- mutate(group_by(res1, Var),
               DayDOY = ifelse(is.na(DayDOY), mean(max_pRec_date, na.rm = TRUE) - 50, DayDOY),
               pRec = ifelse(is.na(pRec), mean(max_pRec, na.rm = TRUE), pRec))

# Fit various models for senescence
sig_sLL3 <- drm(pRec ~ DayDOY, curveid = Var, data = res1, fct = LL.3())
sig_sL3 <- drm(pRec ~ DayDOY, curveid = Var, data = res1, fct = L.3())
sig_sE3 <- drm(pRec ~ DayDOY, curveid = Var, data = res1, fct = G.3u())
sig_sG3 <- drm(pRec ~ DayDOY, curveid = Var, data = res1, fct = G.3())
sig_sW13 <- drm(pRec ~ DayDOY, curveid = Var, data = res1, fct = W1.3())
sig_sW23 <- drm(pRec ~ DayDOY, curveid = Var, data = res1, fct = W2.3())
sig_sLN3 <- drm(pRec ~ DayDOY, curveid = Var, data = res1, fct = LN.3())

# Model choice for senescence
ModChoices <- data.frame(model = c("sig_sLL3", "sig_sL3", "sig_sE3", "sig_sG3",
                                   "sig_sW13", "sig_sW23", "sig_sLN3"),
                         AIC = c(AIC(sig_sLL3), AIC(sig_sL3), AIC(sig_sE3),
                                 AIC(sig_sG3), AIC(sig_sW13), AIC(sig_sW23), AIC(sig_sLN3)),
                         BIC = c(BIC(sig_sLL3), BIC(sig_sL3), BIC(sig_sE3),
                                 BIC(sig_sG3), BIC(sig_sW13), BIC(sig_sW23), BIC(sig_sLN3)))
BestModels <- "sig_sLN3" # based on visual assessment
plot(get(BestModels))
saveRDS(get(BestModels), "./out/BestModel_senescence.rds")

# Extract senescence parameters
se <- data.frame("Senescence95" = as.data.frame(ED(get(BestModels), 5))$Estimate, 
                 "Senescence50" = as.data.frame(ED(get(BestModels), 50))$Estimate, 
                 "Senescence5" = as.data.frame(ED(get(BestModels), 95))$Estimate)
df <- bind_cols(df, se)
se2 <- as.data.frame(t(get(BestModels)$parmMat))
df <- bind_cols(df, data.frame("sen_b_Slope" = se2[,1]))
df$Senescence <- df$Senescence5 - df$Senescence95

# Visualization of model fit for senescence
newdatas <- expand.grid(DayDOY = c(200:450), Var = levels(as.factor(res$Var)))
pms <- predict(sig_sL3, newdata = newdatas, interval = "confidence")
newdatas$p <- pms[,1]

png(height = 9, width = 15, res = 300, units = "in", type = "cairo", family = "Garamond",
    filename = "./out/GroundCoverDynamic_senescence.png")
ggplot(res, aes(x = DayDOY, y = pRec)) +
  geom_line(data = newdatas, aes(x = DayDOY, y = p), color = "#984ea3", linewidth = 1.2) +
  facet_wrap(Var ~ .) +
  xlab("Day of the year") + ylab("Yam ground cover (%)") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none") +
  geom_point(size = 3, alpha = .8) +
  ylim(0, 100)
dev.off()

# Comprehensive Visualization of all stages ------------------------------------
ress<-subset(res, Var %in% c("A01", "A109"))
newdataS<-subset(newdata, Var %in% c("A01", "A109"))
newdatasS<-subset(newdatas, Var %in% c("A01", "A109"))
dfs<-subset(df, Var %in% c("A01", "A109"))
png(height = 5, width = 10, res = 300, units = "in", type = "cairo", family = "Garamond",
    filename = "./out/GroundCoverDynamic_all.png")
ggplot(ress, aes(x = DayDOY, y = pRec)) +
  geom_smooth(se = FALSE, color = "#d95f02", linewidth = 1.2) +
  geom_line(data = newdataS, aes(x = DayDOY, y = p), color = "#4daf4a", linewidth = 1.2) +
  geom_segment(data = dfs, arrow = arrow(length = unit(0.1, "inches"), ends = "both"), color = "#4daf4a",
               aes(x = growth_ED5, y = growth_d_MaxRec + 5, xend = growth_ED95, yend = growth_d_MaxRec + 5)) +
  geom_text(data = dfs, color = "#4daf4a", 
            aes(x = growth_ED5 + (growth_ED95 - growth_ED5) / 2, y = growth_d_MaxRec + 8, label = "Growth")) +
  geom_vline(data = dfs, aes(xintercept = DOY_Plant), color = "#377eb8", linetype = "dashed") + 
  geom_vline(data = dfs, aes(xintercept = growth_ED5), color = "#377eb8", linetype = "dashed") + 
  geom_segment(data = dfs, arrow = arrow(length = unit(0.1, "inches"), ends = "both"), color = "#377eb8",
               aes(x = DOY_Plant, y = growth_d_MaxRec + 8, xend = growth_ED5, yend = growth_d_MaxRec + 8)) +
  geom_text(data = dfs, color = "#377eb8", 
            aes(x = DOY_Plant + (growth_ED5 - DOY_Plant) / 2, y = growth_d_MaxRec + 11, label = "Latency")) +
  geom_vline(data = dfs, aes(xintercept = Plateau_n), color = "#d95f02", linetype = "dashed") + 
  geom_vline(data = dfs, aes(xintercept = Plateau_x), color = "#d95f02", linetype = "dashed") + 
  geom_segment(data = dfs, arrow = arrow(length = unit(0.1, "inches"), ends = "both"), color = "#d95f02",
               aes(x = Plateau_n, y = growth_d_MaxRec + 8, xend = Plateau_x, yend = growth_d_MaxRec + 8)) +
  geom_text(data = dfs, color = "#d95f02", 
            aes(x = Plateau_n + (Plateau_x - Plateau_n) / 2, y = growth_d_MaxRec + 11, label = "Plateau")) +
  geom_line(data = newdatasS, aes(x = DayDOY, y = p), color = "#984ea3", linewidth = 1.2) +
  geom_vline(data = dfs, aes(xintercept = Senescence95), color = "#984ea3", linetype = "dashed") + 
  geom_vline(data = dfs, aes(xintercept = Senescence5), color = "#984ea3", linetype = "dashed") + 
  geom_segment(data = dfs, arrow = arrow(length = unit(0.1, "inches"), ends = "both"), color = "#984ea3",
               aes(x = Senescence95, y = growth_d_MaxRec + 5, xend = Senescence5, yend = growth_d_MaxRec + 5)) +
  geom_text(data = dfs, color = "#984ea3", 
            aes(x = Senescence95 + (Senescence5 - Senescence95) / 2, y = growth_d_MaxRec + 8, label = "Senescence")) +
  geom_segment(data = dfs, arrow = arrow(length = unit(0.1, "inches"), ends = "both"), color = "#a65628",
               aes(x = growth_ED5, y = growth_d_MaxRec + 15, xend = Senescence5, yend = growth_d_MaxRec + 15)) +
  geom_text(data = dfs, color = "#a65628", 
            aes(x = growth_ED5 + (Senescence5 - growth_ED5) / 2, y = growth_d_MaxRec + 18, label = "Crop cycle")) +
  facet_wrap(Var ~ .) +
  xlab("Day of the year") + ylab("Yam ground cover (%)") +
  geom_point(size = 3, alpha = .8) +
  geom_text(data = dfs, label = c("Growth \n(Log-normal)", ""), x = 100, y = 7,
            hjust = 0, nudge_x = -0.2, color = "#4daf4a") +
  geom_text(data = dfs, label = c("Plateau \n(Loess)", ""), x = 373, y = 25,
            hjust = 0, color = "#d95f02") +
  geom_text(data = dfs, label = c("Senescence \n(Log-normal)", ""), x = 450, y = 8,
            hjust = 1, color = "#984ea3") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none") +
  ylim(0, 100)
dev.off()

# Save final dataset
DF <- dplyr::select(df, Var, Latency, Growth, Plateau, Senescence,
                    growth_b_Slope:growth_e_TimeTo50, sen_b_Slope)
write.csv2(DF, "./out/Yam_GroundCover_KeyTraits.csv", col.names = TRUE, row.names = FALSE, sep = ";")
