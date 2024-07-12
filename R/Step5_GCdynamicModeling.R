# Apply non lienar models to GC

# Get dataframe ready ---------------------------------
# Load libraries
packs <- c("tidyverse", "MASS","gridExtra", "drc", "scales", 
           "multcomp", "medrc", "nlme", "nls2")
InstIfNec<-function (pack) {
  if (!do.call(require,as.list(pack))) {
    do.call(install.packages,as.list(pack))  }
  do.call(require,as.list(pack)) }
lapply(packs, InstIfNec)

# Params
PlantingDate<-"220602"
DOY_Plant<-as.numeric(format(as.Date(PlantingDate, "%y%m%d"), "%j"))

# Load Dataframe
res<-read.csv2("./out/POPA_2021_pRec.csv", sep=";")
res$Date_Rec<-as.Date(as.character(res$Date), "%y%m%d")
res$Var<-toupper(res$Var)
res$pRec<-as.numeric(res$pRec)
res$Rep<-toupper(res$Rep)
res$Rep<-gsub(" ", "", res$Rep)
res$Rep<-ifelse(res$Rep=="R1", "RP1",
                ifelse(res$Rep=="RP1(2)", "RP1",
                       ifelse(res$Rep=="16", "RP1",
                              ifelse(res$Rep=="R2", "RP2",
                                     ifelse(res$Rep=="RP2(2)", "RP2",
                                            ifelse(res$Rep=="0535", "RP2",
                                                   ifelse(res$Rep=="0436RP1", "RP1",res$Rep)))))))

            
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
png(height=12, width=20, res=300, units="in", type="cairo",family="Garamond",
    filename="./out/pRec_Rep_cleaned.png")
ggplot(data=res, aes(x=Date_Rec, y=pRec, color=factor(Rep)))+
  geom_smooth(se=F)+ theme_bw(base_size = 16)+
  geom_point()+
  xlab("Date")+ylab("Recouvrement (%)")+ 
  scale_x_date(labels = date_format("%d %b"))+
  facet_wrap(~Var)+
  theme(legend.position = "none")
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
source('./R/Step0_NonlinearSSfct.R')
res$code<-paste(res$Var, res$Rep, sep="_")

# Ajustement d'une sigmoide 
ggplot(data=res, aes(x=DayDOY, y=pRec, color=factor(Rep)))+
  geom_smooth(se=F)+ theme_bw(base_size = 16)+
  geom_point()+
  xlab("DOY")+ylab("Recouvrement (%)")+ 
  facet_wrap(~Var)+
  theme(legend.position = "none")


# Sigmoid modeling of growing period ------------------------------
res %>%
  group_by(code) %>%
  mutate(max_pRec_date = DayDOY[which.max(pRec)],
         max_pRec=max(pRec)) %>%
  filter(DayDOY <= max_pRec_date) %>%
  ungroup() -> res0
res0 %>%
  as_tibble() %>%
  group_by(code, Var, Rep) %>% 
  group_modify(~ add_row(.x,.before=0)) -> res0
res0<-mutate(group_by(res0, Var, Rep),
             DayDOY=ifelse(is.na(DayDOY), mean(max_pRec_date, na.rm=T)+10, DayDOY),
             pRec=ifelse(is.na(pRec),  mean(max_pRec, na.rm=T), pRec)) 

sig_LL3<-drm(pRec~DayDOY, curveid=code, data=res0, 
        fct=LL.3(), upperl=c(Inf,100,350))
saveRDS(sig_LL3, "./out/sig_LL3.rds" )
sig_L3<-drm(pRec~DayDOY, curveid=code, data=res0, fct=L.3())
saveRDS(sig_L3, "./out/sig_L3.rds")
sig_E3<-drm(pRec~DayDOY, curveid=code, data=res0, fct=G.3u())
saveRDS(sig_E3, "./out/sig_E3.rds")
sig_G3<-drm(pRec~DayDOY, curveid=code, data=res0, fct=G.3(), upperl=c(Inf,100,350))
saveRDS(sig_G3, "./out/sig_G3.rds")
sig_W13<-drm(pRec~DayDOY, curveid=code, data=res0, fct=W1.3(), upperl=c(Inf,100,350))
saveRDS(sig_W13, "./out/sig_W13.rds")
sig_W23<-drm(pRec~DayDOY, curveid=code, data=res0, fct=W2.3(), upperl=c(Inf,100,350))
saveRDS(sig_W23, "./out/sig_W23.rds")

# Model choice
ModChoice<-data.frame(model=c("drmLL3", "drmL3", "drmE3", "drmG3",
                              "drmW13", "drmW23"),
                      AIC=c(AIC(sig_LL3), AIC(sig_L3), AIC(sig_E3),
                            AIC(sig_G3),  AIC(sig_W13), AIC(sig_W23)),
                      BIC=c(BIC(sig_LL3), BIC(sig_L3), BIC(sig_E3),
                            BIC(sig_G3),  BIC(sig_W13), BIC(sig_W23)))

# Visualization of model fit
newdata<-expand.grid(DayDOY=c(100:410), code=levels(as.factor(res$code)))
pm <- predict(sig_W23, newdata=newdata, interval="confidence")
newdata$p<-pm[,1]
newdata<-separate(newdata, code, c("Var", "Bloc"), "_", remove=F)
png(height=12, width=20, res=300, units="in", type="cairo",family="Garamond",
    filename="./out/pRec_PredObs_LL3.png")
ggplot(res, aes(x = DayDOY, y = pRec)) +
  geom_point() +
  geom_line(data=newdata, aes(x=DayDOY, y=p), color="grey50") +
  facet_wrap(code~.)+
  xlab("Jour de l'année") + ylab("Recouvrement du sol par l'igname (%)")+
  theme_bw(base_size = 8)+
  theme(legend.position="none")
dev.off()

# Getting model parameters out of fit by Var
df<-as.data.frame(t(sig_W23$parmMat))
df$code<-row.names(df)
df<-bind_cols(df, ED(sig_W23, 5), ED(sig_W23, 50), ED(sig_W23, 95))
colnames(df)<-c("croiss_b_Slope", "croiss_d_MaxRec", "croiss_e_TimeTo50", "code", 
                "croiss_ED5", "croiss_sdED5", "croiss_ED50", "croiss_sdED50", "croiss_ED95", "croiss_sdED95")
newdata$DOY_pRec3<-ifelse(newdata$p>3, "yes", "no")
newdatas<-subset(newdata, DOY_pRec3=="yes")
newdatas<-slice(group_by(newdatas, code), which.min(DayDOY))
newdatas<-dplyr::select(newdatas, DayDOY, code)
colnames(newdatas)<-c("DOY_pRec3", "code")
df<-left_join(df, newdatas, by="code")
df$Latence<-df$DOY_pRec3-DOY_Plant
df$Latence<-ifelse(df$Latence<0, 1, df$Latence)
df$Croissance<-df$croiss_ED95-df$croiss_ED5

# Plateau detection -------------------------------------
# Params 
change_threshold<-0.3
LoesSPAN<-0.75

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
  print(i)
  re <- subset(res, code==df$code[i])
  Lgth0 <- max(re$DayDOY)
  lissDAE <- seq(DOY_Plant, Lgth0, 1)
  fit<-loess(pRec~DayDOY, data=re, span=LoesSPAN)
  pRec <- predict(fit, lissDAE, se=F)
  Lpred <- cbind.data.frame(code=df$code[i], DayDOY=lissDAE, pRec=pRec)
  cord <- detect_plateau_near_peak(Lpred$pRec, change_threshold)
  df$Plateau_n[i]<-ifelse(is.null(cord$start_index), NA, cord$start_index+DOY_Plant)
  df$Plateau_x[i]<-ifelse(is.null(cord$end_index), NA, cord$end_index+DOY_Plant)
}
df$Plateau<-df$Plateau_x-df$Plateau_n


# Senescence -----------------
# Sigmoid modeling of growing period ------------------------------
res %>%
  group_by(code) %>%
  mutate(max_pRec_date = DayDOY[which.max(pRec)],
         max_pRec=max(pRec)) %>%
  filter(DayDOY >= max_pRec_date) %>%
  ungroup() -> res1
res1 %>%
  as_tibble() %>%
  group_by(code, Var, Rep) %>% 
  group_modify(~ add_row(.x,.before=0)) -> res1
res1<-mutate(group_by(res1, Var, Rep),
             DayDOY=ifelse(is.na(DayDOY), 420, DayDOY), 
             pRec=ifelse(is.na(pRec),  0, pRec)) 

sig_sLL3<-drm(pRec~DayDOY, curveid=code, data=res1, 
             fct=LL.3(), lowerl=c(-Inf,0,350))
saveRDS(sig_sLL3, "./out/sig_sLL3.rds" )
sig_sL3<-drm(pRec~DayDOY, curveid=code, data=res1, fct=L.3())
saveRDS(sig_sL3, "./out/sig_sL3.rds")
sig_sE3<-drm(pRec~DayDOY, curveid=code, data=res1, fct=G.3u())
saveRDS(sig_sE3, "./out/sig_sE3.rds")
sig_sG3<-drm(pRec~DayDOY, curveid=code, data=res1, fct=G.3(), lowerl=c(-Inf,0,350))
saveRDS(sig_sG3, "./out/sig_sG3.rds")
sig_sW13<-drm(pRec~DayDOY, curveid=code, data=res1, fct=W1.3(), lowerl=c(-Inf,0,350))
saveRDS(sig_sW13, "./out/sig_sW13.rds")
sig_sW23<-drm(pRec~DayDOY, curveid=code, data=res1, fct=W2.3(), lowerl=c(-Inf,0,350))
saveRDS(sig_sW23, "./out/sig_sW23.rds")

# Model choice
ModChoices<-data.frame(model=c("drmsLL3", "drmsL3", "drmsE3", "drmsG3",
                              "drmsW13", "drmWs23"),
                      AIC=c(AIC(sig_sLL3), AIC(sig_sL3), AIC(sig_sE3),
                            AIC(sig_sG3),  AIC(sig_sW13), AIC(sig_sW23)),
                      BIC=c(BIC(sig_sLL3), BIC(sig_sL3), BIC(sig_sE3),
                            BIC(sig_sG3),  BIC(sig_sW13), BIC(sig_sW23)))

# Visualization of model fit
newdatas<-expand.grid(DayDOY=c(200:410), code=levels(as.factor(res$code)))
pms <- predict(sig_sL3, newdata=newdatas, interval="confidence")
newdatas$p<-pms[,1]
newdatas<-separate(newdatas, code, c("Var", "Rep"), "_", remove=F)

png(height=12, width=20, res=300, units="in", type="cairo",family="Garamond",
    filename="./out/pRec_All.png")
ggplot(res, aes(x = DayDOY, y = pRec)) +
  geom_smooth(method = "loess", color="blue", alpha=0.2, span=LoesSPAN, se=F)+
  geom_point() +
  geom_line(data=newdata, aes(x=DayDOY, y=p), color="red", alpha=0.5) +
  geom_line(data=newdatas, aes(x=DayDOY, y=p), color="purple", alpha=0.5) +
  geom_vline(data=df, aes(xintercept=Plateau_n)) + 
  geom_vline(data=df, aes(xintercept=Plateau_x)) + 
  geom_vline(data=df, aes(xintercept=DOY_Plant), color="darkgreen", linetype="dashed") + 
  geom_vline(data=df, aes(xintercept=croiss_ED5), color="darkgreen", linetype="dashed") + 
  facet_wrap(code~.)+
  xlab("Jour de l'année") + ylab("Recouvrement du sol par l'igname (%)")+
  theme_bw(base_size = 8)+
  theme(legend.position="none")+
  ylim(0,100)
dev.off()

# Gather parameters
se<-data.frame("Senescence95"=as.data.frame(ED(sig_sL3, 5))$Estimate, 
              "Senescence50"=as.data.frame(ED(sig_sL3, 50))$Estimate, 
              "Senescence5"=as.data.frame(ED(sig_sL3, 95))$Estimate)
df<-bind_cols(df, se)
se2<-as.data.frame(t(sig_sL3$parmMat))
df<-bind_cols(df, data.frame("sen_b_Slope"=se2[,1]))
df$Senescence<-df$Senescence95-df$Senescence5

# Save final dataset ----------------------
df<-separate(df, code, into=c("Var", "Rep"), sep="_", remove = F)
DF<-dplyr::select(df, code, Var, Rep, Latence, croiss_b_Slope:croiss_e_TimeTo50, 
                  Croissance, sen_b_Slope, Senescence)
write.csv2(DF, "./out/DynRec_PopA_2022.csv", col.names = T, row.names = F, sep=";")

rese<-subset(res, code=="A30_RP1")
newdatae<-subset(newdata, code=="A30_RP1")
newdatase<-subset(newdatas, code=="A30_RP1")
dfe<-subset(df, code=="A30_RP1")
png(height=6, width=10, res=300, units="in", type="cairo",family="Garamond",
    filename="./out/pRec_All_eg.png")
ggplot(rese, aes(x = DayDOY, y = pRec)) +
  geom_smooth(method = "loess", color="blue", size=1.2, span=LoesSPAN, se=F)+
  geom_point(size=4, alpha=0.8) +
  geom_line(data=newdatae, aes(x=DayDOY, y=p), color="darkred", size=1.2) +
  geom_line(data=newdatase, aes(x=DayDOY, y=p), color="purple", size=1.2) +
  geom_vline(data=dfe, color="blue", aes(xintercept=Plateau_n), linetype="dashed") + 
  geom_vline(data=dfe, color="blue", aes(xintercept=Plateau_x), linetype="dashed") + 
  geom_segment(data=dfe, aes(x = Plateau_n, y = 95, xend = Plateau_x, yend = 95),
               arrow=arrow(length=unit(0.5, "cm"), ends="both"), 
               color="blue", lineend='round', size=1.2)+
  geom_text(label="Plateau", x=dfe$Plateau_n+(dfe$Plateau_x-dfe$Plateau_n)/2, y=100,
            hjust = 'outside', color="blue", nudge_x = -0.2) +
  geom_vline(data=dfe, aes(xintercept=DOY_Plant), color="darkgreen", linetype="dashed") + 
  geom_vline(data=dfe, aes(xintercept=croiss_ED5), color="darkgreen", linetype="dashed") + 
  geom_segment(data=dfe, aes(x=DOY_Plant, y = 95, xend=croiss_ED5, yend=95),
               arrow=arrow(length=unit(0.5, "cm"), ends="both"), 
               color="darkgreen", lineend='round', size=1.2)+
  geom_text(label="Latence", x=DOY_Plant+(dfe$croiss_ED5-DOY_Plant)/2, y=100,
            hjust = 'outside', nudge_x = -0.2, color="darkgreen") +
  xlab("Jour de l'année") + ylab("Recouvrement du sol par l'igname (%)")+
  theme_bw(base_size = 18)+
  theme(legend.position="none")+
  geom_vline(data=dfe, aes(xintercept=Senescence95), color="purple", linetype="dashed") + 
  geom_vline(data=dfe, aes(xintercept=Senescence5), color="purple", linetype="dashed") +
  geom_segment(data=dfe, aes(x=Senescence5, y = 95, xend=Senescence95, yend=95),
               arrow=arrow(length=unit(0.5, "cm"), ends="both"), 
               color="purple", lineend='round', size=1.2)+
  geom_text(label="Senescence", x=dfe$Senescence95+(dfe$Senescence5-dfe$Senescence95)/2, y=100,
            hjust = 'outside', nudge_x = -0.2, color="purple") +
  geom_text(label="Croissance \n(Weibull)", x=100, y=5,
            hjust = 0, nudge_x = -0.2, color="darkred") +
  geom_text(label="Senescence \n(Logistic)", x=365, y=8,
            hjust = 1, color="purple") +
  geom_text(label="Plateau \n(Loess)", x=373, y=25,
            hjust = 0, color="blue") +
  geom_vline(data=dfe, aes(xintercept=croiss_ED95), color="darkred", linetype="dashed") + 
  geom_vline(data=dfe, aes(xintercept=croiss_ED5), color="darkred", linetype="dashed") +
  geom_segment(data=dfe, aes(x=croiss_ED5, y = 95, xend=croiss_ED95, yend=95),
               arrow=arrow(length=unit(0.5, "cm"), ends="both"), 
               color="darkred", lineend='round', size=1.2)+
  geom_text(label="Croissance", x=dfe$croiss_ED5+(dfe$croiss_ED95-dfe$croiss_ED5)/2, y=100,
            hjust = 'outside', nudge_x = -0.2, color="darkred") +
  ylim(0,100)
dev.off()
