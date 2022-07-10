library(stringr)
library(syuzhet)
library(Hmisc)
library(caTools)
setwd("E://DW and DM Datasets//ProjectMScCSPart2//Amazon Reviews Mobile Phones")

############################################################################

data <- read.csv("Amazon_Mobile.csv")
data <- na.omit(data)
data <- data[!(data$Brand.Name==''),]
data <- unique(data)
data$Reviews <- as.character(data$Reviews)
data$Brand.Name <- tolower(data$Brand.Name)
############################################################################
data$Camera <- 0
decimalPoint <- "\\d+(\\.\\d{1,2})?"
cameraDetailRegex <- regex(paste(decimalPoint,"[ \t]*mp", sep = ""),ignore_case = TRUE)
for(i in 1:nrow(data)){
  megaPixel <- str_extract_all(data$Product.Name[i],cameraDetailRegex)[[1]]
  if(length(megaPixel) == 0){
    data$Camera[i] <- 0
  }else{
    megaPixelValue <- max(str_extract(megaPixel, decimalPoint))
    data$Camera[i] <- megaPixelValue
  }
}
data$Camera <- as.double(data$Camera)
data <- data[!(data$Camera == 0),]
data[str_detect(data$Brand.Name ,"lg"),]$Brand.Name = "lg"
data[str_detect(data$Brand.Name ,"sony"),]$Brand.Name = "sony"
data[str_detect(data$Brand.Name ,"htc"),]$Brand.Name = "htc"
############################################################################
brandReviewCount <- aggregate(data$Review.Votes, by=list(Brand.Name=data$Brand.Name), FUN=sum)
sortBrandReviewCount <- brandReviewCount[order(brandReviewCount$x, decreasing = TRUE),]
topTenBrand <- sortBrandReviewCount[1:10,]$Brand.Name
topTenBrand
rm(brandReviewCount)
rm(sortBrandReviewCount)
data <- data[data$Brand.Name %in% topTenBrand,]
write.csv(data,"MobileReviewWithCamera.csv", row.names=FALSE)
############################################################################
funcCleanReviews <- function (reviews) {
  clean_reviews <- gsub('[[:punct:]]', ' ', reviews)
  clean_reviews <- gsub('[[:digit:]]', ' ', clean_reviews)
  clean_reviews <- gsub('http\\w+', ' ', clean_reviews)
  clean_reviews <- gsub('[ \t]{2,}', ' ', clean_reviews)
  clean_reviews <- gsub('^\\s+|\\s+$', ' ', clean_reviews)
  clean_reviews <- gsub('<.*>', '', enc2native(clean_reviews))
  clean_reviews <- gsub('[^\x01-\x7F]', '', clean_reviews)
  clean_reviews <- tolower(clean_reviews)
  clean_reviews
}
############################################################################
set.seed(500)
sample <- sample.split(data$Brand.Name, SplitRatio = (500 / nrow(data)))
dataSample <- subset(data, sample == TRUE)
dataSample$Reviews <- funcCleanReviews(dataSample$Reviews)
dataSample <- na.omit(dataSample)
dataSample <- dataSample[!(dataSample$Brand.Name == ''),]
dataSample <- unique(dataSample)
age <- c("Below 16", "17 - 30", "30 - 40", "40 - 60", "More Than 60")
ageData <- list()
i <- 1
for (megaPixel in dataSample$Camera) {
  if(megaPixel >= 20){
    indexProb <- sample(5, 1, prob = c(0.05, 0.55, 0.2, 0.1, 0.1))
  }else if(megaPixel >= 13){
    indexProb <- sample(5, 1, prob = c(0.05, 0.45, 0.3, 0.1, 0.1))
  }else if(megaPixel >= 8){
    indexProb <- sample(5, 1, prob = c(0.1, 0.3, 0.2, 0.3, 0.1))
  }else if(megaPixel >= 5){
    indexProb <- sample(5, 1, prob = c(0.15, 0.25, 0.3, 0.2, 0.1))
  }else{
    indexProb <- sample(5, 1, prob = c(0.2, 0.1, 0.2, 0.2, 0.3))
  }
  ageData[i] <- age[indexProb]
  i <- i + 1  
}
dataSample$Age <- as.character(ageData)
############################################################################
dataSample <- read.csv("SampleMobileReview.csv")
dataSample <- dataSample[order(dataSample$Camera),]
dataSample$CustID <- 0
cameraTable <- aggregate(Reviews ~ Camera, dataSample, FUN = length)
for(cam in c(23,41)){
  sizeCameraList = nrow(dataSample[dataSample$Camera == cam,])
  custIDList = seq(1, sizeCameraList + 10)
  dataSample[dataSample$Camera == cam,]$CustID <- sample(custIDList, sizeCameraList)
}
startCustID <- max(dataSample$CustID) + 1
for(cam in c(12,16)){
  sizeCameraList = nrow(dataSample[dataSample$Camera == cam,])
  custIDList = seq(startCustID, startCustID + sizeCameraList + 10)
  dataSample[dataSample$Camera == cam,]$CustID <- sample(custIDList, sizeCameraList)
}
startCustID <- max(dataSample$CustID) + 1
for(cam in c(8,13)){
  sizeCameraList = nrow(dataSample[dataSample$Camera == cam,])
  custIDList = seq(startCustID, startCustID + sizeCameraList + 10)
  dataSample[dataSample$Camera == cam,]$CustID <- sample(custIDList, sizeCameraList)
}
startCustID <- max(dataSample$CustID) + 1
for(cam in c(3,3.20)){
  sizeCameraList = nrow(dataSample[dataSample$Camera == cam,])
  custIDList = seq(1, sizeCameraList + 10)
  dataSample[dataSample$Camera == cam,]$CustID <- sample(custIDList, sizeCameraList)
}
startCustID <- max(dataSample$CustID) + 1
for(cam in c(2,5)){
  sizeCameraList = nrow(dataSample[dataSample$Camera == cam,])
  custIDList = seq(startCustID, startCustID + sizeCameraList + 10)
  dataSample[dataSample$Camera == cam,]$CustID <- sample(custIDList, sizeCameraList)
}
startCustID <- max(dataSample$CustID) + 1
sizeCameraList <- nrow(dataSample[dataSample$CustID == 0,])
custIDList = seq(startCustID, startCustID + sizeCameraList)
dataSample[dataSample$CustID == 0,]$CustID <-  sample(custIDList, sizeCameraList, replace = TRUE)
x <- aggregate(CustID ~ Camera, dataSample, FUN = length)
############################################################################
dataSample$Year <- sample(seq(2008,2017), nrow(dataSample), 
              prob = c(0.05, 0.08, 0.07, 0.09, 0.09, 0.1, 0.12, 0.13, 0.12 ,0.15), replace = TRUE)

############################################################################
emotions <- get_nrc_sentiment(as.character(dataSample$Reviews))
emotionTable <- emotions[,1:8]
polarityTable <- emotions[,9:10]
dataSample$Polarity <- capitalize(colnames(polarityTable)[apply(polarityTable,1,which.max)])
dataSample$Emotions <- capitalize(colnames(emotionTable)[apply(emotionTable,1,which.max)])
write.csv(dataSample,"SampleMobileReview.csv", row.names=FALSE)
############################################################################
set.seed(500)
sample <- sample.split(dataSample$Brand.Name, SplitRatio = 0.75)
trainData <- subset(dataSample, sample == TRUE)
write.csv(trainData,"SampleMobileReviewTrain.csv", row.names=FALSE)
testData <- subset(dataSample, sample == FALSE)
write.csv(testData,"SampleMobileReviewTest.csv", row.names=FALSE)
rm(emotions)
rm(emotionTable)
rm(polarityTable)
############################################################################
rm(list=ls())
gc()