# Chargement des librairies ------------------------------------------------
packs <- c("tidyverse", "imager", "tripack","colorscience", "randomForest")
InstIfNec<-function (pack) {
  if (!do.call(require,as.list(pack))) {
    do.call(install.packages, as.list(pack))  }
  do.call(require, as.list(pack)) }
lapply(packs, InstIfNec)


# Getting location of data for classification out of 5 sample pictures
image<-load.image(file.choose())
image<-resize(image, 600, 400)
par(mar=c(0,0,0,0))
plot(image)
Yam<-as.data.frame(lapply(locator(10), round, 0))
Other<-as.data.frame(lapply(locator(10), round, 0))
Yam$Class<-"Y"
Other$Class<-"O"
df<-bind_rows(Yam, Other)
df<-cbind(df$Class, diag(image[df$x, df$y,1,1]),diag(image[df$x, df$y,1,2]),
          diag(image[df$x, df$y,1,3]),df$x, df$y)
res<-df

image<-load.image(file.choose())
image<-resize(image, 600, 400)
par(mar=c(0,0,0,0))
plot(image)
Yam<-as.data.frame(lapply(locator(10), round, 0))
Other<-as.data.frame(lapply(locator(10), round, 0))
Yam$Class<-"Y"
Other$Class<-"O"
df<-bind_rows(Yam, Other)
df<-cbind(df$Class, diag(image[df$x, df$y,1,1]),diag(image[df$x, df$y,1,2]),
          diag(image[df$x, df$y,1,3]),df$x, df$y)
res<-bind_rows(as.data.frame(res), as.data.frame(df))

image<-load.image(file.choose())
image<-resize(image, 600, 400)
par(mar=c(0,0,0,0))
plot(image)
Yam<-as.data.frame(lapply(locator(10), round, 0))
Other<-as.data.frame(lapply(locator(10), round, 0))
Yam$Class<-"Y"
Other$Class<-"O"
df<-bind_rows(Yam, Other)
df<-cbind(df$Class, diag(image[df$x, df$y,1,1]),diag(image[df$x, df$y,1,2]),
          diag(image[df$x, df$y,1,3]),df$x, df$y)
res<-bind_rows(as.data.frame(res), as.data.frame(df))

image<-load.image(file.choose())
image<-resize(image, 600, 400)
par(mar=c(0,0,0,0))
plot(image)
Yam<-as.data.frame(lapply(locator(10), round, 0))
Other<-as.data.frame(lapply(locator(10), round, 0))
Yam$Class<-"Y"
Other$Class<-"O"
df<-bind_rows(Yam, Other)
df<-cbind(df$Class, diag(image[df$x, df$y,1,1]),diag(image[df$x, df$y,1,2]),
          diag(image[df$x, df$y,1,3]),df$x, df$y)
res<-bind_rows(as.data.frame(res), as.data.frame(df))

image<-load.image(file.choose())
image<-resize(image, 600, 400)
par(mar=c(0,0,0,0))
plot(image)
Yam<-as.data.frame(lapply(locator(10), round, 0))
Other<-as.data.frame(lapply(locator(10), round, 0))
Yam$Class<-"Y"
Other$Class<-"O"
df<-bind_rows(Yam, Other)
df<-cbind(df$Class, diag(image[df$x, df$y,1,1]),diag(image[df$x, df$y,1,2]),
          diag(image[df$x, df$y,1,3]),df$x, df$y)
res<-bind_rows(as.data.frame(res), as.data.frame(df))


# Getting color spaces values for classification data into a data frame ------------
colnames(res)<-c("class", "R", "G", "B", "x", "y")
res[,-1] <- lapply(res[,-1], as.character)
res[,-1] <- lapply(res[,-1], as.numeric)
mat<-cbind(res, as.data.frame(RGB2HSV(cbind(res$R*255, res$G*255, res$B*255))))
mat<-cbind(mat, as.data.frame(RGB2XYZ(cbind(mat$R, mat$G, mat$B))))
mat<-cbind(mat, as.data.frame(XYZ2Lab(cbind(mat$V1, mat$V2, mat$V3))))
mat<-dplyr::select(mat, class, R:B, H:b)
mat$r.g<-mat$R/mat$G
mat$g2rb<-(2*mat$G)-mat$R-mat$B
mat$class<-as.factor(mat$class)


# Applying a classifier and creating output files of data sets  --------
fit_yam <- randomForest(class ~ ., data=mat, importance=T, proximity=T, 
                    mtry=5, ntree=300, nodesize=3)
fit_yam
plot(fit_yam)
importance(fit_yam)
saveRDS(fit_yam, "./out/YamPixelClass.rds" )
