# Apply non linear models to GC
# devtools::install_github("DoseResponse/drcData")
# devtools::install_github("DoseResponse/medrc")
# Get dataframe ready ----------------------------------------------------------
# Load libraries
packs <- c("tidyverse",
           # "MASS",
           # "gridExtra", 
           "drc",
           # "multcomp", 
           # "medrc", 
           # "nlme", 
           # "nls2"
           "scales")
InstIfNec<-function (pack) {
  if (!do.call(require,as.list(pack))) {
    do.call(install.packages,as.list(pack))  }
  do.call(require,as.list(pack)) }
lapply(packs, InstIfNec)


# 0. Data preparation ----------------------------------------------------------
# Load Dataframe
PlantingDate<-"220602"
DOY_Plant<-as.numeric(format(as.Date(PlantingDate, "%y%m%d"), "%j"))
res<-read.csv2("./out/Yam_pRec.csv", sep=";")
res$Date_Rec<-as.Date(as.character(res$Date), "%y%m%d")
res$Rep<-gsub(" ", "", res$Rep)

            
# Mise en graphique
png(height=12, width=20, res=300, units="in", type="cairo",family="Garamond",
    filename="./out/pRec_Rep.png")
ggplot(data=res, aes(x=Date_Rec, y=pRec, color=factor(Rep)))+
  geom_smooth(se=F)+ theme_bw(base_size = 16)+
  geom_point()+
  xlab("Date")+ylab("Recouvrement (%)")+ 
  scale_x_date(labels = date_format("%d %b"))+
  facet_wrap(~Var)+
  theme(legend.position = "none")
dev.off()

# Supprimer les génotypes problématiques
res<-subset(res, !Var %in%c("A103", "A29", "A97"))
res<-subset(res, !(Var=="A23" & Rep=="RP1"))
res<-subset(res, !(Var=="A120" & Rep=="RP2"))
res$Var<-ifelse(res$Var=="A73" & res$Date=="221007", "A79", res$Var)
png(height=9, width=15, res=300, units="in", type="cairo",family="Garamond",
    filename="./out/GroundCoverDynamic_observations.png")
ggplot(data=res, aes(x=Date_Rec, y=pRec))+
  geom_smooth(se=F)+ theme_bw(base_size = 16)+
  geom_point()+
  xlab("Day of the year") + ylab("Yam groun cover (%)")+
  scale_x_date(labels = date_format("%d %b"))+
  facet_wrap(~Var)
dev.off()


# Rajout du recouvrement initiale (t0 => pLeaves=0) 
res %>%
  as_tibble() %>%
  group_by(Var, Rep) %>% 
  group_modify(~ add_row(.x,.before=0)) -> res
res$Date<-ifelse(is.na(res$Date), PlantingDate, res$Date) 
res$pRec<-ifelse(is.na(res$pRec), 0, res$pRec) 
res$Date_Rec<-as.Date(as.character(res$Date), "%y%m%d")
res$Var<-as.factor(res$Var)
res$Rep<-as.factor(res$Rep)
res$DayDOY<-as.numeric(format(res$Date_Rec, "%j"))
res$DayDOY<-ifelse(res$DayDOY<100, res$DayDOY+365, res$DayDOY)
res$code<-paste(res$Var, res$Rep, sep="_")


# Extraction de la partie croissance de la dynamique de recouvrement
res %>%
  group_by(Var) %>%
  mutate(max_pRec_date = DayDOY[which.max(pRec)],
         max_pRec=max(pRec)) %>%
  filter(DayDOY <= max_pRec_date) %>%
  ungroup() -> res0
res0 %>%
  as_tibble() %>%
  group_by(Var) %>% 
  group_modify(~ add_row(.x,.before=0)) -> res0
res0<-mutate(group_by(res0, Var),
             DayDOY=ifelse(is.na(DayDOY), mean(max_pRec_date, na.rm=T)+5, DayDOY),
             pRec=ifelse(is.na(pRec),  mean(max_pRec, na.rm=T), pRec)) 
res0 %>%
  as_tibble() %>%
  group_by(Var) %>% 
  group_modify(~ add_row(.x,.before=0)) -> res0
res0<-mutate(group_by(res0, Var),
             DayDOY=ifelse(is.na(DayDOY), mean(max_pRec_date, na.rm=T)+10, DayDOY),
             pRec=ifelse(is.na(pRec),  mean(max_pRec, na.rm=T), pRec)) 
res0 %>%
  as_tibble() %>%
  group_by(Var) %>% 
  group_modify(~ add_row(.x,.before=0)) -> res0
res0<-mutate(group_by(res0, Var),
             DayDOY=ifelse(is.na(DayDOY), mean(max_pRec_date, na.rm=T)+50, DayDOY),
             pRec=ifelse(is.na(pRec),  mean(max_pRec, na.rm=T), pRec)) 


# 1. Ajustement d'une sigmoide -------------------------------------------------
# Get upper limit based on max observed ground cover
var_levels <- df$Var
up_limits<-df$growth_d_MaxRec
upll <- c()
for (i in 1:length(var_levels)) {
  upll <- c(upll, Inf, up_limits[i], 300)
}

# Fit the model
sig_LL3<-drm(pRec~DayDOY, curveid=Var, data=res0, fct=LL.3(), upperl=upll)
sig_L3<-drm(pRec~DayDOY, curveid=Var, data=res0, fct=L.3(), upperl=upll)
sig_E3<-drm(pRec~DayDOY, curveid=Var, data=res0, fct=G.3u(), upperl=upll)
sig_G3<-drm(pRec~DayDOY, curveid=Var, data=res0, fct=G.3(), upperl=upll)
sig_W13<-drm(pRec~DayDOY, curveid=Var, data=res0, fct=W1.3(), upperl=upll)
sig_W23<-drm(pRec~DayDOY, curveid=Var, data=res0, fct=W2.3())
sig_LN3<-drm(pRec~DayDOY, curveid=Var, data=res0, fct=LN.3())

# Model choice
ModChoice<-data.frame(model=c("sig_LL3", "sig_L3", "sig_E3", "sig_G3",
                              "sig_W13", "sig_W23", "sig_LN3"),
                      AIC=c(AIC(sig_LL3), AIC(sig_L3), AIC(sig_E3),
                            AIC(sig_G3),  AIC(sig_W13), AIC(sig_W23), AIC(sig_LN3)),
                      BIC=c(BIC(sig_LL3), BIC(sig_L3), BIC(sig_E3),
                            BIC(sig_G3),  BIC(sig_W13), BIC(sig_W23), BIC(sig_LN3)))
BestModel<-ModChoice[which.min(ModChoice$BIC), "model"]
plot(get(BestModel))
saveRDS(get(BestModel), "./out/BestModel_growth.rds")

# Getting model parameters out of fit by genotype
df<-as.data.frame(t(get(BestModel)$parmMat))
df$Var<-row.names(df)
df<-bind_cols(df, ED(get(BestModel), 5), ED(get(BestModel), 50), ED(get(BestModel), 95))
colnames(df)<-c("growth_b_Slope", "growth_d_MaxRec", "growth_e_TimeTo50", "Var", 
                "growth_ED5", "growth_sdED5", "growth_ED50", "growth_sdED50", "growth_ED95", "growth_sdED95")

# Add key traits
df$Latency<-ED(get(BestModel), 3)[,1]-DOY_Plant
df$Growth<-df$growth_ED95-df$growth_ED5

# Visualization of model fit
newdata<-expand.grid(DayDOY=c(100:410), Var=levels(as.factor(res$Var)))
pm <- predict(get(BestModel), newdata=newdata, interval="confidence")
newdata$p<-pm[,1]
png(height=9, width=15, res=300, units="in", type="cairo",family="Garamond",
    filename="./out/GroundCoverDynamic_growth.png")
ggplot(res, aes(x = DayDOY, y = pRec)) +
  geom_line(data=newdata, aes(x=DayDOY, y=p), color="#4daf4a", linewidth=1.2) +
  facet_wrap(Var~.)+
  xlab("Day of the year") + ylab("Yam groun cover (%)")+
  theme_bw(base_size = 15)+
  geom_vline(data=df, aes(xintercept=DOY_Plant), color="#4daf4a", linetype="dashed") + 
  geom_vline(data=df, aes(xintercept=growth_ED5), color="#4daf4a", linetype="dashed") + 
  geom_segment(data=df, arrow = arrow(length = unit(0.1, "inches"), ends="both"), color="#4daf4a",
               aes(x=DOY_Plant, y=growth_d_MaxRec+5, xend=growth_ED5, yend=growth_d_MaxRec+5))+
  geom_text(data=df, color="#4daf4a", 
            aes(x=DOY_Plant+(growth_ED5-DOY_Plant)/2, y=growth_d_MaxRec+10, label="Latency"))+
  geom_point(size=3, alpha=.8) +
  theme(legend.position="none")
dev.off()


# 2. Plateau detection ---------------------------------------------------------
# Params 
change_threshold<-0.25
LoesSPAN<-0.6

# Function
detect_plateau_near_peak <- function(vector, change_threshold) {
  peak_index <- which.max(vector) # Find the index of the peak value
  # Initialize start and end indices of the plateau
  start_index <- peak_index
  end_index <- peak_index
  # Expand to the left from the peak
  for(i in (peak_index-1):1) {
    if(abs(vector[i] - vector[i+1]) <= change_threshold) {
      start_index <- i
    } else { break }}
  # Expand to the right from the peak
  for(i in (peak_index+1):length(vector)) {
    if(abs(vector[i] - vector[i-1]) <= change_threshold) {
      end_index <- i
    } else { break }}
  return(list(start_index = start_index, end_index = end_index))
}

# Apply function
for (i in 1:nrow(df)) {
  re <- subset(res, Var==df$Var[i])
  print(df$Var[i])
  Lgth0 <- max(re$DayDOY)
  lissDAE <- seq(DOY_Plant, Lgth0, 1)
  fit<-loess(pRec~DayDOY, data=re, span=LoesSPAN)
  pRec <- predict(fit, lissDAE, se=F)
  Lpred <- cbind.data.frame(Var=df$Var[i], DayDOY=lissDAE, pRec=pRec)
  cord <- detect_plateau_near_peak(Lpred$pRec, change_threshold)
  df$Plateau_n[i]<-ifelse(is.null(cord$start_index), NA, cord$start_index+DOY_Plant)
  df$Plateau_x[i]<-ifelse(is.null(cord$end_index), NA, cord$end_index+DOY_Plant)
}
df$Plateau<-df$Plateau_x-df$Plateau_n

# Vizualisation
png(height=9, width=15, res=300, units="in", type="cairo",family="Garamond",
    filename="./out/GroundCoverDynamic_plateau.png")
ggplot(res, aes(x = DayDOY, y = pRec)) +
  geom_smooth(method = "loess", color="#d95f02", alpha=0.2, span=LoesSPAN, se=F)+
  geom_vline(data=df, aes(xintercept=Plateau_n), color="#d95f02", linetype="dashed") + 
  geom_vline(data=df, aes(xintercept=Plateau_x), color="#d95f02", linetype="dashed") + 
  geom_segment(data=df, arrow = arrow(length = unit(0.1, "inches"), ends="both"), color="#d95f02",
               aes(x=Plateau_n, y=growth_d_MaxRec+5, xend=Plateau_x, yend=growth_d_MaxRec+5))+
  geom_text(data=df, color="#d95f02", 
            aes(x=Plateau_n+(Plateau_x-Plateau_n)/2, y=growth_d_MaxRec+12, label="Plateau"))+
  facet_wrap(Var~.)+
  xlab("Day of the year") + ylab("Yam groun cover (%)")+
  geom_point(size=3, alpha=.8) +
  theme_bw(base_size = 15)+
  ylim(0,100)
dev.off()


# 3. Senescence ----------------------------------------------------------------
# Extract senescence part of the observations
res %>%
  group_by(Var) %>%
  mutate(max_pRec_date = DayDOY[which.max(pRec)],
         max_pRec=max(pRec)) %>%
  filter(DayDOY >= max_pRec_date) %>%
  ungroup() -> res1
res1 %>% # Add 0 ground cover at the end of cropping season
  as_tibble() %>%
  group_by(Var) %>% 
  group_modify(~ add_row(.x,.before=0)) -> res1
res1<-mutate(group_by(res1, Var),
             DayDOY=ifelse(is.na(DayDOY), 420, DayDOY), 
             pRec=ifelse(is.na(pRec),  0, pRec)) 
res1 %>% # Add max growth few days before max growth to allow for better sigmoid adjustment
  as_tibble() %>%
  group_by(Var) %>% 
  group_modify(~ add_row(.x,.before=0)) -> res1
res1<-mutate(group_by(res1, Var),
             DayDOY=ifelse(is.na(DayDOY), mean(max_pRec_date, na.rm=T)-5, DayDOY),
             pRec=ifelse(is.na(pRec),  mean(max_pRec, na.rm=T), pRec)) 
res1 %>% # Add max growth few days before max growth to allow for better sigmoid adjustment
  as_tibble() %>%
  group_by(Var) %>% 
  group_modify(~ add_row(.x,.before=0)) -> res1
res1<-mutate(group_by(res1, Var),
             DayDOY=ifelse(is.na(DayDOY), mean(max_pRec_date, na.rm=T)-10, DayDOY),
             pRec=ifelse(is.na(pRec),  mean(max_pRec, na.rm=T), pRec)) 
res1 %>% # Add max growth few days before max growth to allow for better sigmoid adjustment
  as_tibble() %>%
  group_by(Var) %>% 
  group_modify(~ add_row(.x,.before=0)) -> res1
res1<-mutate(group_by(res1, Var),
             DayDOY=ifelse(is.na(DayDOY), mean(max_pRec_date, na.rm=T)-50, DayDOY),
             pRec=ifelse(is.na(pRec),  mean(max_pRec, na.rm=T), pRec)) 


# Model choice 
sig_sLL3<-drm(pRec~DayDOY, curveid=Var, data=res1, fct=LL.3())
sig_sL3<-drm(pRec~DayDOY, curveid=Var, data=res1, fct=L.3())
sig_sE3<-drm(pRec~DayDOY, curveid=Var, data=res1, fct=G.3u())
sig_sG3<-drm(pRec~DayDOY, curveid=Var, data=res1, fct=G.3())
sig_sW13<-drm(pRec~DayDOY, curveid=Var, data=res1, fct=W1.3())
sig_sW23<-drm(pRec~DayDOY, curveid=Var, data=res1, fct=W2.3())
sig_sLN3<-drm(pRec~DayDOY, curveid=Var, data=res1, fct=LN.3())
ModChoices<-data.frame(model=c("sig_sLL3", "sig_sL3", "sig_sE3", "sig_sG3",
                              "sig_sW13", "sig_sW23", "sig_sLN3"),
                      AIC=c(AIC(sig_sLL3), AIC(sig_sL3), AIC(sig_sE3),
                            AIC(sig_sG3),  AIC(sig_sW13), AIC(sig_sW23), AIC(sig_sLN3)),
                      BIC=c(BIC(sig_sLL3), BIC(sig_sL3), BIC(sig_sE3),
                            BIC(sig_sG3),  BIC(sig_sW13), BIC(sig_sW23), BIC(sig_sLN3)))
BestModels<-"sig_sLN3" # based on visual assessment
plot(get(BestModels))
saveRDS(get(BestModels), "./out/BestModel_senescence.rds")

# Gather parameters
se<-data.frame("Senescence95"=as.data.frame(ED(get(BestModels), 5))$Estimate, 
               "Senescence50"=as.data.frame(ED(get(BestModels), 50))$Estimate, 
               "Senescence5"=as.data.frame(ED(get(BestModels), 95))$Estimate)
df<-bind_cols(df, se)
se2<-as.data.frame(t(get(BestModels)$parmMat))
df<-bind_cols(df, data.frame("sen_b_Slope"=se2[,1]))
df$Senescence<-df$Senescence5-df$Senescence95

# Visualization of model fit
newdatas<-expand.grid(DayDOY=c(200:450), Var=levels(as.factor(res$Var)))
pms <- predict(sig_sL3, newdata=newdatas, interval="confidence")
newdatas$p<-pms[,1]

png(height=9, width=15, res=300, units="in", type="cairo",family="Garamond",
    filename="./out/GroundCoverDynamic_senescence.png")
ggplot(res, aes(x = DayDOY, y = pRec)) +
  geom_line(data=newdatas, aes(x=DayDOY, y=p), color="#984ea3", linewidth=1.2) +
  facet_wrap(Var~.)+
  xlab("Day of the year") + ylab("Yam groun cover (%)")+
  theme_bw(base_size = 15)+
  theme(legend.position="none")+
  geom_point(size=3, alpha=.8) +
  ylim(0,100)
dev.off()

png(height=9, width=15, res=300, units="in", type="cairo",family="Garamond",
    filename="./out/GroundCoverDynamic_all.png")
ggplot(res, aes(x = DayDOY, y = pRec)) +
  geom_smooth(se=F, color="#d95f02", linewidth=1.2)+
  geom_line(data=newdata, aes(x=DayDOY, y=p), color="#4daf4a", linewidth=1.2) +
  geom_segment(data=df, arrow = arrow(length = unit(0.1, "inches"), ends="both"), color="#4daf4a",
               aes(x=growth_ED5, y=growth_d_MaxRec+5, xend=growth_ED95, yend=growth_d_MaxRec+5))+
  geom_text(data=df, color="#4daf4a", 
            aes(x=growth_ED5+(growth_ED95-growth_ED5)/2, y=growth_d_MaxRec+8, label="Growth"))+
  geom_vline(data=df, aes(xintercept=DOY_Plant), color="#377eb8", linetype="dashed") + 
  geom_vline(data=df, aes(xintercept=growth_ED5), color="#377eb8", linetype="dashed") + 
  geom_segment(data=df, arrow = arrow(length = unit(0.1, "inches"), ends="both"), color="#377eb8",
               aes(x=DOY_Plant, y=growth_d_MaxRec+8, xend=growth_ED5, yend=growth_d_MaxRec+8))+
  geom_text(data=df, color="#377eb8", 
            aes(x=DOY_Plant+(growth_ED5-DOY_Plant)/2, y=growth_d_MaxRec+11, label="Latency"))+
  geom_vline(data=df, aes(xintercept=Plateau_n), color="#d95f02", linetype="dashed") + 
  geom_vline(data=df, aes(xintercept=Plateau_x), color="#d95f02", linetype="dashed") + 
  geom_segment(data=df, arrow = arrow(length = unit(0.1, "inches"), ends="both"), color="#d95f02",
               aes(x=Plateau_n, y=growth_d_MaxRec+8, xend=Plateau_x, yend=growth_d_MaxRec+8))+
  geom_text(data=df, color="#d95f02", 
            aes(x=Plateau_n+(Plateau_x-Plateau_n)/2, y=growth_d_MaxRec+11, label="Plateau"))+
  geom_line(data=newdatas, aes(x=DayDOY, y=p), color="#984ea3", linewidth=1.2) +
  geom_vline(data=df, aes(xintercept=Senescence95), color="#984ea3", linetype="dashed") + 
  geom_vline(data=df, aes(xintercept=Senescence5), color="#984ea3", linetype="dashed") + 
  geom_segment(data=df, arrow = arrow(length = unit(0.1, "inches"), ends="both"), color="#984ea3",
               aes(x=Senescence95, y=growth_d_MaxRec+5, xend=Senescence5, yend=growth_d_MaxRec+5))+
  geom_text(data=df, color="#984ea3", 
            aes(x=Senescence95+(Senescence5-Senescence95)/2, y=growth_d_MaxRec+8, label="Senescence"))+
  geom_segment(data=df, arrow = arrow(length = unit(0.1, "inches"), ends="both"), color="#a65628",
               aes(x=growth_ED5, y=growth_d_MaxRec+15, xend=Senescence5, yend=growth_d_MaxRec+15))+
  geom_text(data=df, color="#a65628", 
            aes(x=growth_ED5+(Senescence5-growth_ED5)/2, y=growth_d_MaxRec+18, label="Crop cycle"))+
  facet_wrap(Var~.)+
  xlab("Day of the year") + ylab("Yam groun cover (%)")+
  geom_point(size=3, alpha=.8) +
  geom_text(data=df, label=c("Growth \n(Log-normal)", rep("", 4)), x=100, y=7,
            hjust = 0, nudge_x = -0.2, color="#4daf4a") +
  geom_text(data=df, label=c("Plateau \n(Loess)", rep("", 4)), x=373, y=25,
            hjust = 0, color="#d95f02") +
  geom_text(data=df, label=c("Senescence \n(Log-normal)", rep("", 4)), x=450, y=8,
            hjust = 1, color="#984ea3") +
  theme_bw(base_size = 15)+
  theme(legend.position="none")+
  ylim(0,100)
dev.off()

# Save final dataset
DF<-dplyr::select(df, Var, Latency, Growth, Plateau, Senescence,
                  growth_b_Slope:growth_e_TimeTo50, sen_b_Slope)
write.csv2(DF, "./out/Yam_GroundCover_KeyTraits.csv", col.names = T, row.names = F, sep=";")
