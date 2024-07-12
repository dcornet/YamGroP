# Libraries
packs <- c("tidyverse","imager", "MASS","gridExtra", "data.table",
           "drc", "scales", "multcomp", "medrc", "nlme", "nls2",
           "magick", "colorscience", "randomForest", "doSNOW")
InstIfNec<-function (pack) {
    if (!do.call(require,as.list(pack))) {
        do.call(install.packages,as.list(pack))  }
    do.call(require,as.list(pack)) }
lapply(packs, InstIfNec)

# Batch segmentation of pics --------------------------------------
# Pixel classifier model 
fit_yam <- readRDS("./out/YamPixelClass.rds")

# Applying the function to all jpg files in a directory
alldata <- list.files(path="./out/TrapezCorrectedPic/", patt='.png', ignore.case=TRUE)
library(doParallel)
registerDoParallel(cl <- makeCluster(10))
temp<-foreach(i=1:length(alldata), .verbose=T,
              .packages=c('imager','tidyverse', 'colorscience', 'randomForest')) %dopar% {
    afile<-alldata[i]
    cat(paste("reading in file: ", afile, "\n"))
    pic<-load.image(paste0("./out/TrapezCorrectedPic/", afile))
    dim1<-dim(pic)
    mat<-channels(pic)
    mat<-as.data.frame(mat)
    mat<-spread(mat, im, value)
    colnames(mat)<-c("x", "y", "R", "G", "B")
    mat$x<-rep(1:dim1[1], each=dim1[2])
    mat$y<-rep(dim1[2]:1, dim1[1])
    mat<-cbind(mat, as.data.frame(RGB2HSV(cbind(mat$R*255, mat$G*255, mat$B*255))))
    mat<-cbind(mat, as.data.frame(RGB2XYZ(cbind(mat$R, mat$G, mat$B))))
    mat<-cbind(mat, as.data.frame(XYZ2Lab(cbind(mat$V1, mat$V2, mat$V3))))
    mat$r.g<-mat$R/(ifelse(mat$G==0,0.001,mat$G))
    mat$g2rb<-(2*mat$G)-mat$R-mat$B
    mat$Class<-predict(fit_yam, newdata=mat[,-c(1:2)])
    Leaves<-nrow(subset(mat, Class=="Y"))/nrow(mat)*100
    c(afile, Leaves)
}
stopCluster(cl)

# Gather results into dataframe and save it
bdf <- do.call(rbind, temp)
bdf<-as.data.frame(bdf)
colnames(bdf)<-c("Pic", "pRec")
bdf$Pic<-gsub(".png", "", bdf$Pic)
bdf$Pic<-gsub("__", "_", bdf$Pic)
bdf<-separate(bdf, Pic, into=c("Var", "Rep", "Date"), sep="_")
fwrite(bdf, "./out/POPA_2021_pRec.csv", col.names=T, sep=";", row.names=F)
