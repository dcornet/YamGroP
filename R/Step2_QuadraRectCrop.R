# Chargement des librairies ------------------------------------------------
packs <- c("tidyverse", "imager", "tripack","colorscience", "parallel", "ggplot2", "randomForest")
InstIfNec<-function (pack) {
    if (!do.call(require,as.list(pack))) {
        do.call(install.packages, as.list(pack))  }
    do.call(require, as.list(pack)) }
lapply(packs, InstIfNec)

# Chargement des images ------------------------------------------------
alldataFile <- grep(list.files("D:/BigData/", recursive=TRUE, full.names = T), 
                    pattern='JPG', value=TRUE)
# pic<-alldataFile[368]

# 1 - QuadraDetection ------------------------------------------------
QuadraDetection <- function(pic) {
    tryCatch({
        # lapply(packs, library, character.only = TRUE)
        picname<-paste(gsub(".JPG", "", unlist(strsplit(pic, "/"))[5]),
                       unlist(strsplit(pic, "/"))[4], sep="_")
        
        img<-load.image(pic)
        dim0<-dim(img)
        imgs<-imager::resize(img, 600, 600/dim0[1]*dim0[2])
        # plot(imgs)
        
        # Get matrix from pic
        dim1<-dim(imgs)
        mat<-channels(imgs)
        mat<-as.data.frame(mat)
        mat<-spread(mat, im, value)
        colnames(mat)<-c("x", "y", "R", "G", "B")
        mat$x<-rep(1:dim1[1], each=dim1[2])
        mat$y<-rep(dim1[2]:1, dim1[1])
        
        # Keep only blue pxl
        mat<-cbind(mat, as.data.frame(RGB2HSV(cbind(mat$R*255, mat$G*255, mat$B*255))))
        mat<-cbind(mat, as.data.frame(RGB2XYZ(cbind(mat$R, mat$G, mat$B))))
        mat<-cbind(mat, as.data.frame(XYZ2Lab(cbind(mat$V1, mat$V2, mat$V3))))
        mat$r.g<-mat$R/(ifelse(mat$G==0,0.001,mat$G))
        mat$g2rb<-(2*mat$G)-mat$R-mat$B
        fit<-readRDS("./out/QuadraPixelClass.rds")
        temp<-predict(fit, newdata=mat[,-(1:2)])
        mat$quad<-ifelse(temp=="O",0,1)
        
        # Get mat back to cimg, delete noise and turn cimg back to mat
        matq<-mat[,c(1,2,ncol(mat))]
        colnames(matq)<-c("x", "y", "value")
        imq<-as.cimg(matq)
        # plot(mirror(imq, "y"))
        imqo<-mopening_square(imq, 3)
        # imqo<-erode_square(imq, 2)
        # imqo<-dilate_square(imqo, 2)
        # imqo<-dilate_square(imqo, 6)
        # plot(mirror(imqo, "y"))
        matqo<-channels(imqo)
        matqo<-as.data.frame(matqo)
        matqo<-matqo[with(matqo, order(x, y)), ]
        mat$qo<-matqo$value
        
        x<-subset(mat, qo==1)$x
        y<-subset(mat, qo==1)$y
        
        # Extract only pixels from trapezoid
        obj<-tri.mesh(x, y)
        mat$trapez<-in.convex.hull(obj, mat$x, mat$y)
        mats<-subset(mat, trapez==TRUE)
        
        # Get corners coordinates
        p<-rbind(mats[which.max(mats$y-mats$x),],
                 mats[which.max(mats$y+mats$x),],
                 mats[which.min(mats$y-mats$x),],
                 mats[which.min(mats$y+mats$x),])
        
        p<-as.matrix(p[,1:2])
        
        pxl1.5m<-sqrt((p[1,"x"]-p[2, "x"])^2+(p[1,"y"]-p[2, "y"])^2)
        pp<-rbind(c(min(p[,1]), ifelse(min(p[,2])+pxl1.5m/1.5>=max(mat$y), 
                                       max(mat$y), min(p[,2])+pxl1.5m/1.5)),
                  c(min(p[,1])+pxl1.5m, ifelse(min(p[,2])+pxl1.5m/1.5>=max(mat$y), 
                                               max(mat$y), min(p[,2])+pxl1.5m/1.5)),
                  c(min(p[,1])+pxl1.5m, ifelse(min(p[,2])+pxl1.5m/1.5>=max(mat$y), 
                                               max(mat$y)-pxl1.5m/1.5, min(p[,2]))),
                  c(min(p[,1]), ifelse(min(p[,2])+pxl1.5m/1.5>=max(mat$y), 
                                       max(mat$y)-pxl1.5m/1.5, min(p[,2])))) 
        A <- list()
        b <- list()
        
        ##Build RHS and LHS matrix components for each point pair
        for (i in 1:4) {
            A[[paste0(i)]] <- matrix(c(
                p[i,1],p[i,2],1, 0,      0,      0, -pp[i,1]*p[i,1], -pp[i,1]*p[i,2],
                0,     0,     0, p[i,1], p[i,2], 1, -pp[i,2]*p[i,1], -pp[i,2]*p[i,2]),
                2,8, byrow=TRUE)
            b[[paste0(i)]] <- matrix(c(pp[i,1], pp[i,2]),2,1)
        }
        
        ##Glue matrices together
        A_matrix <- do.call(rbind, A)
        b_matrix <- do.call(rbind, b)
        
        ##Solve equation system
        h_vec <- solve(A_matrix, b_matrix)
        
        ##Form H matrix from the solution
        H <- matrix(c(h_vec,1), 3,3, byrow=TRUE)
        
        ##Transform image coordinates (x',y') to (x,y), i.e. note we specify
        ##the back transformation p = H * p', so H here is the inverse.
        map.persp.inv <- function(x,y, H) {
            out_image <- H %*% rbind(x,y,1)
            list(x=out_image[1,]/out_image[3,], y=out_image[2,]/out_image[3,])
        }
        warp <- imwarp(imgs, map=function(x,y) map.persp.inv(x,y,solve(H)),
                       coordinates="absolute", direction="backward")
        # plot(warp)
        
        # Get corrected image back to matrix format
        mat<-channels(warp)
        mat<-as.data.frame(mat)
        mat<-spread(mat, im, value)
        colnames(mat)<-c("x", "y", "R", "G", "B")
        
        # Keep only blue pxl
        mat<-cbind(mat, as.data.frame(RGB2HSV(cbind(mat$R*255, mat$G*255, mat$B*255))))
        mat<-cbind(mat, as.data.frame(RGB2XYZ(cbind(mat$R, mat$G, mat$B))))
        mat<-cbind(mat, as.data.frame(XYZ2Lab(cbind(mat$V1, mat$V2, mat$V3))))
        mat$r.g<-mat$R/mat$G
        mat$g2rb<-(2*mat$G)-mat$R-mat$B
        temp<-predict(fit, newdata=mat[,-(1:2)])
        mat$quad<-ifelse(temp=="O",0,1)
        
        # Get mat back to cimg, delete noise and turn cimg back to mat
        matq<-mat[,c(1,2,ncol(mat))]
        colnames(matq)<-c("x", "y", "value")
        imq<-as.cimg(matq)
        # plot(mirror(imq, "y"))
        imqo<-mopening_square(imq, 3)
        # plot(mirror(imqo, "y"))
        matqo<-channels(imqo)
        matqo<-as.data.frame(matqo)
        matqo<-matqo[with(matqo, order(x, y)), ]
        mat$qo<-matqo$value
        
        x<-subset(mat, qo==1)$x
        y<-subset(mat, qo==1)$y
        
        # Extract only pixels from trapezoid
        obj<-tri.mesh(x, y)
        mat$trapez<-in.convex.hull(obj, mat$x, mat$y)
        mats<-subset(mat, trapez==TRUE)
        
        # Get corners coordinates
        p<-rbind(mats[which.max(mats$y-mats$x),],
                 mats[which.max(mats$y+mats$x),],
                 mats[which.min(mats$y-mats$x),],
                 mats[which.min(mats$y+mats$x),])
        xn<-max(p$x[c(1,4)])
        xx<-min(p$x[c(2,3)])
        yx<-min(p$y[c(1,2)])
        yn<-max(p$y[c(3,4)])
        
        mats<-subset(mat, x<xx & x>xn & y<yx & y>yn)
        mats<-as.data.frame(mats)
        mats<-dplyr::select(mats, x, y, R, G, B)
        tMat <-mats
        
        mats<-pivot_longer(mats, names_to="cc", values_to="value", c(R, G, B))
        mats$cc<-ifelse(mats$cc=="R", 1, ifelse(mats$cc=="G", 2, 3))
        mats<-mats[with(mats, order(cc, y, x)), ]
        imq<-as.cimg(mats$value, length(unique(mats$x)), length(unique(mats$y)), cc=3)
        # imager::save.image(imq, paste0("./out/TrapezCorrectedPic/", picname, ".png"))
        imager::save.image(imq, paste0("./out/tc2/", picname, ".png"))
    }, error = function(e) print(paste('error: ', picname)))
}


# 2. Execute the function in parallel #### ------------------
no_cores <- detectCores()-1
cl <- makeCluster(getOption("cl.cores", no_cores))
clusterEvalQ(cl, {
    library(imager)
    library(tidyverse)
    library(tripack)
    library(colorscience) 
    library(randomForest)
    })
parLapply(cl, alldataFile, QuadraDetection)
stopCluster(cl)


# 3. Check pic ratio to detect anomalies ----------------------
alltrapc <- list.files("./out/TrapezCorrectedPic/", recursive=TRUE, full.names = T)
df<-as.data.frame(alltrapc)
df$alltrapc<-gsub("./out/TrapezCorrectedPic/","",df$alltrapc)
df$alltrapc<-gsub(".png","",df$alltrapc)
df<-separate(df, alltrapc, into=c("Var", "Rep", "Date"), sep="_")
df$id<-paste0("D:/BigData//", df$Date, "/", df$Var, "_", df$Rep, ".JPG")
alldataFile2<-setdiff(alldataFile, df$id)

no_cores <- detectCores()-1
cl <- makeCluster(getOption("cl.cores", no_cores))
clusterEvalQ(cl, {
    library(imager)
    library(tidyverse)
    library(tripack)
    library(colorscience) 
    library(randomForest)
})
parLapply(cl, alldataFile2, QuadraDetection)
stopCluster(cl)
