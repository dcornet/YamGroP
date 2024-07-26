# Chargement des librairies ------------------------------------------------
packs <- c("tidyverse", "imager", "tripack","colorscience", "crayon",
           "randomForest", "MASS")
InstIfNec<-function (pack) {
  if (!do.call(require,as.list(pack))) {
    do.call(install.packages, as.list(pack))  }
  do.call(require, as.list(pack)) }
lapply(packs, InstIfNec)


# Getting location of data for classification ----------------------------------
res<-data.frame()
NbPic<-10 # Number of pic to build the classification model
PxlNbByClass<-20 # Number of pixel to get from each class on each pic
cat("\014")  
for (i in 1:NbPic) {
  cat(red(paste0("\nChoose pic ", i, "/", NbPic, "\n")))
  image <- load.image(file.choose()) # Choose the image file
  image <- resize(image, 600, 400)
  par(mar=c(0,0,0,0))
  plot(image)
  
  # Collect points for Frame and Other
  cat(green((paste0("\nPlease choose ",  PxlNbByClass, 
                    " pixel corresponding to the blue frame\n"))))
  Frame <- as.data.frame(lapply(locator(PxlNbByClass), round, 0))
  cat(green((paste0("\nPlease choose ",  PxlNbByClass, 
                    " pixel corresponding to the backgroun\n"))))
  Other <- as.data.frame(lapply(locator(PxlNbByClass), round, 0))
  Frame$Class <- "Q"
  Other$Class <- "O"
  df <- bind_rows(Frame, Other)
  
  # Extract color channels and coordinates
  df <- cbind(df$Class, 
    diag(image[df$x, df$y, 1, 1]), # Red channel
    diag(image[df$x, df$y, 1, 2]), # Green channel
    diag(image[df$x, df$y, 1, 3]), # Blue channel
    df$x, df$y)
  
  # Bind the results to the final data frame
  res <- bind_rows(as.data.frame(res), as.data.frame(df))
}


# Getting color spaces values for classification data into a data frame --------
colnames(res)<-c("class", "R", "G", "B", "x", "y")
res[,-1] <- lapply(res[,-1], as.character)
res[,-1] <- lapply(res[,-1], as.numeric)
mat<-cbind(res, as.data.frame(RGB2HSV(cbind(res$R*255, res$G*255, res$B*255))))
mat<-cbind(mat, as.data.frame(RGB2XYZ(cbind(mat$R, mat$G, mat$B))))
mat<-cbind(mat, as.data.frame(XYZ2Lab(cbind(mat$V1, mat$V2, mat$V3))))
mat<-select(mat, class, R:B, H:b)
mat$r.g<-mat$R/mat$G
mat$g2rb<-(2*mat$G)-mat$R-mat$B
mat$class<-as.factor(mat$class)


# Applying a classifier and creating output files of data sets  ----------------
fit <- randomForest(class ~ ., data=mat, importance=T, proximity=T, 
                    mtry=5, ntree=450, nodesize=7)
fit
plot(fit)
importance(fit)
saveRDS(fit, "./out/FramePixelClassifier.rds" )
