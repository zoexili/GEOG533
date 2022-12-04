# Data preprocessing
library(raster)
# get Landsat data
ras1 <- list.files("E:/sample_data/2000",pattern="tif$",full.names=TRUE)
ras2 <- list.files("E:/sample_data/2017",pattern="tif$",full.names=TRUE)

#create raster stack
stack1 <- raster::stack(ras1)
stack2 <- raster::stack(ras2)

# raster stack properties
stack1
nlayers(stack1)
crs(stack1) # coordinate reference system
ncell(stack1) # cells
dim(stack1) 
res(stack1) # spatial resolution

# visualize raster
## 321 true color image
plotRGB(stack1,r=3,g=2,b=1,axes=TRUE,stretch="lin",main="True Color Composite")

## 432 false color image
plotRGB(stack1,r=4,g=3,b=2,axes=TRUE,stretch="lin",main="False Color Composite")

## get 2000 band values
tif1 <- "E:/sample_data/2000/LT05_L1TP_125052_20000927_20161213_01_T1_sr_band1_clip.tif"
## convert raster to matrix
tif1 <- as.matrix(raster(tif1))

tif2 <- "E:/sample_data/2000/LT05_L1TP_125052_20000927_20161213_01_T1_sr_band2_clip.tif"
tif2 <- as.matrix(raster(tif2))
tif3 <- "E:/sample_data/2000/LT05_L1TP_125052_20000927_20161213_01_T1_sr_band3_clip.tif"
tif3 <- as.matrix(raster(tif3))
tif4 <- "E:/sample_data/2000/LT05_L1TP_125052_20000927_20161213_01_T1_sr_band4_clip.tif"
tif4 <- as.matrix(raster(tif4))
tif5 <- "E:/sample_data/2000/LT05_L1TP_125052_20000927_20161213_01_T1_sr_band5_clip.tif"
tif5 <- as.matrix(raster(tif5))
tif7 <- "E:/sample_data/2000/LT05_L1TP_125052_20000927_20161213_01_T1_sr_band7_clip.tif"
tif7 <- as.matrix(raster(tif7))

# get 2017 band values
tif_1 <- "E:/sample_data/2017/LC08_L1TP_125052_20170129_20170214_01_T1_sr_band2_clip.tif"
tif_2 <- "E:/sample_data/2017/LC08_L1TP_125052_20170129_20170214_01_T1_sr_band3_clip.tif"
tif_3 <- "E:/sample_data/2017/LC08_L1TP_125052_20170129_20170214_01_T1_sr_band4_clip.tif"
tif_4 <- "E:/sample_data/2017/LC08_L1TP_125052_20170129_20170214_01_T1_sr_band5_clip.tif"
tif_5 <- "E:/sample_data/2017/LC08_L1TP_125052_20170129_20170214_01_T1_sr_band6_clip.tif"
tif_6 <- "E:/sample_data/2017/LC08_L1TP_125052_20170129_20170214_01_T1_sr_band7_clip.tif"
tif_1 <- as.matrix(raster(tif_1))
tif_2 <- as.matrix(raster(tif_2))
tif_3 <- as.matrix(raster(tif_3))
tif_4 <- as.matrix(raster(tif_4))
tif_5 <- as.matrix(raster(tif_5))
tif_6 <- as.matrix(raster(tif_6))

# Training and testing sampling
#2000
library(caTools)
dataset <- read.csv("20000927.csv")
set.seed(101)
split <- sample.split(dataset,SplitRatio=0.5)
train <- subset(dataset,split==TRUE)
test <- subset(dataset,split==TRUE)

#2017
library(caTools)
dataset2 <- read.csv("20170214.csv")
set.seed(101)
split2 <- sample.split(dataset2,SplitRatio=0.5)
train.new <- subset(dataset2,split==TRUE)
test.new <- subset(dataset2,split==TRUE)

# Random Forests Regression
#2000
library(randomForest)
x <- train[3:8]
rf <- randomForest(x,train$Percentage,ntree=300)
y_pred <- predict(rf,x,type="response")

#2017
x.new <- train.new[3:8]
rf.new <- randomForest(x.new,train.new$Percentage,ntree=300)
y_pred.new <- predict(rf.new,x.new,type="response")

# 2000
## reshape image
b1 <- as.vector(tif1)
b2 <- as.vector(tif2)
b3 <- as.vector(tif3)
b4 <- as.vector(tif4)
b5 <- as.vector(tif5)
b7 <- as.vector(tif7)
tif2000 <- matrix(c(b1,b2,b3,b4,b5,b7),nrow=length(b1))

## predict
tif2000[is.na(tif2000)] <- 0
rf2000 <- predict(rf,tif2000,type="response")

## reshape rf2000
new_rf2000 <- matrix(rf2000,nrow=1455,ncol=1464)

# visualizaing the Random Forest regression results
par(pty="s")
image(new_rf2000,axes=FALSE)
title(main="Ho Chi Minh City Urban Area 2000")

# 2017
a1 <- as.vector(tif_1)
a2 <- as.vector(tif_2)
a3 <- as.vector(tif_3)
a4 <- as.vector(tif_4)
a5 <- as.vector(tif_5)
a6 <- as.vector(tif_6)
tif2017 <- matrix(c(a1,a2,a3,a4,a5,a6),nrow=length(a1))

## predict
tif2017[is.na(tif2017)] <- 0
rf2017 <- predict(rf,tif2017,type="response")

## reshape rf2000
new_rf2017 <- matrix(rf2017,nrow=1455,ncol=1464)

## visualize result
par(pty="s")
image(new_rf2017,axes=FALSE)
title(main="Ho Chi Minh City Urban Area 2017")

# Accuracy accessment
# 2000
y_test <- test[2]
MAE <- 1/105*(sum(abs(y_pred-y_test)))
SE <- 1/105*(sum(y_pred-y_test))

# 2017
y_test.new <- test.new[2]
MAE2 <- 1/105*(sum(abs(y_pred.new-y_test.new)))
SE2 <- 1/105*(sum(y_pred.new-y_test.new))