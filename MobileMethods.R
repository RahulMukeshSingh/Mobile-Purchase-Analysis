library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(plyr)
library(arules)
library(arulesViz)
library(RColorBrewer)
library(ggplot2)
library(graph)
library(Rgraphviz)
library(topicmodels)
library(ggConvexHull)
library(caTools)
library(naivebayes)
library(forecast)
library(ggfortify)
library(caret)
############################################################################
polarityOfReviewEach <- function(eachBrandData, reverse){
  plot <- ggplot(data = eachBrandData, aes(x=Polarity,fill=Polarity)) +
         geom_bar(stat = "count",size = 1) +
         xlab("Polarity") + ylab("Polarity Count")
  if(reverse){
    plot <- plot + coord_flip() + theme(legend.position = "top")
  }
  plot
}

polarityOfReviewAll <- function(allBrandData, reverse){
  plot <- ggplot(data = allBrandData, aes(x=Brand.Name,fill=Polarity)) +
          geom_bar(stat = "count",size = 1) +
    xlab("All Brands") + ylab("Polarity Count")
  if(reverse){
    plot <- plot + coord_flip() + theme(legend.position = "top")
  }
  plot
}

emotionOfReviewEach <- function(eachBrandData, reverse){
  plot <- ggplot(data = eachBrandData, aes(x=Emotions,fill=Emotions)) +
    geom_bar(stat = "count",size = 1) +
    xlab("Emotions") + ylab("Polarity Count")
  if(reverse){
    plot <- plot + coord_flip() + theme(legend.position = "top")
  }
  plot
}

emotionOfReviewAll <- function(allBrandData, reverse){
  plot <- ggplot(data = allBrandData, aes(x=Brand.Name,fill=Emotions)) +
    geom_bar(stat = "count",size = 1) +
    xlab("All Brands") + ylab("Emotion Count")
  if(reverse){
    plot <- plot + coord_flip() + theme(legend.position = "top")
  }
  plot
}

distributionOfRatingsEach <- function(eachBrandData, reverse){
  plot <- ggplot(eachBrandData, aes(x = Rating, y = ..count../tapply(..count..,..PANEL..,sum)[..PANEL..], 
                            fill = Brand.Name )) + geom_freqpoly(binwidth = 1, col = "blue") + 
    xlab("Rating") + ylab("Percent") + 
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(breaks = c(1,2,3,4,5))
  if(reverse){
    plot <- plot + coord_flip() + theme(legend.position = "top")
  }
  plot
}

distributionOfRatingsAll <- function(allBrandData, reverse){
  plot <- ggplot(allBrandData, aes(x = Rating, y = ..count../tapply(..count..,..PANEL..,sum)[..PANEL..], 
                                   fill = Brand.Name, color = Brand.Name)) +
    geom_bar(stat = "count", size = 1)+
    xlab("Rating") + ylab("Percent") + 
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(breaks = c(1,2,3,4,5))
  if(reverse){
    plot <- plot + coord_flip() + theme(legend.position = "top")
  } 
  plot
}

distributionOfRatingsbyPolarityEach <- function(eachBrandData, reverse){
  plot <- ggplot(eachBrandData, aes(x = Rating, y = ..count../tapply(..count..,..PANEL..,sum)[..PANEL..], 
                                    fill = Polarity, color = Polarity )) + 
          geom_freqpoly(binwidth = 1) + 
          xlab("Rating") + ylab("Percent") + 
          scale_y_continuous(labels = scales::percent) +
          scale_x_continuous(breaks = c(1,2,3,4,5))
  
  if(reverse){
    plot <- plot + coord_flip() + theme(legend.position = "top")
  } 
  plot
}

distributionOfRatingsbyPolarityAll <- function(allBrandData, reverse){
  plot <- ggplot(allBrandData, aes(x = Rating, y = ..count../tapply(..count..,..PANEL..,sum)[..PANEL..], 
                           fill = Polarity, color = Polarity )) +
    geom_freqpoly(binwidth = 1) +
    xlab("Rating") + ylab("Percent") + 
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(breaks = c(1,2,3,4,5)) +
    facet_wrap(~Brand.Name)
  if(reverse){
    plot <- plot + coord_flip() + theme(legend.position = "top")
  } 
  plot
}

distributionOfRatingsbyEmotionEach <- function(eachBrandData, reverse){
  plot <- ggplot(eachBrandData, aes(x = Rating, y = ..count../tapply(..count..,..PANEL..,sum)[..PANEL..], 
                                    fill = Emotions, color = Emotions )) + 
    geom_freqpoly(binwidth = 1) + 
    xlab("Rating") + ylab("Percent") + 
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(breaks = c(1,2,3,4,5))
  
  if(reverse){
    plot <- plot + coord_flip() + theme(legend.position = "top")
  } 
  plot
}

distributionOfRatingsbyEmotionAll <- function(allBrandData, reverse){
  plot <- ggplot(allBrandData, aes(x = Rating, y = ..count../tapply(..count..,..PANEL..,sum)[..PANEL..], 
                                   fill = Emotions, color = Emotions )) +
    geom_freqpoly(binwidth = 1) +
    xlab("Rating") + ylab("Percent") + 
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(breaks = c(1,2,3,4,5)) +
    facet_wrap(~Brand.Name)
  if(reverse){
    plot <- plot + coord_flip() + theme(legend.position = "top")
  } 
  plot
}

############################################################################
getData <- function(brandData, brand_Name){
  if(brand_Name == "All Brands"){
    brandData
  }else{
    brandData[brandData$Brand.Name == brand_Name,]
  }
}

getFilteredData <- function(brandData, filterCategory, brand_Name){
  dataset <- getData(brandData, brand_Name)
  if(filterCategory == "All"){
    dataset
  }else if(filterCategory == "Positive Polarity"){
    dataset[dataset$Polarity == "Positive", ]
  }else if(filterCategory == "Negative Polarity"){
    dataset[dataset$Polarity == "Negative", ]
  }else if(filterCategory == "Ratings > 3"){
    dataset[dataset$Rating > 3,]
  }else if(filterCategory == "Ratings < 3"){
    dataset[dataset$Rating < 3,]
  }
}

noOfReview <- function(allBrandData){
  brandReviewData <- allBrandData %>%
    group_by(Brand.Name)%>%
    summarise(Count = n())
  plot_ly(brandReviewData, labels = ~Brand.Name, values = ~Count) %>% add_pie(hole = 0.6)
}

polarityOfReview <- function(brandData, reverse, brand_Name){
  brandData <- getData(brandData, brand_Name)
  if(brand_Name == "All Brands"){
    plot <- polarityOfReviewAll(brandData, reverse)
  }else{
    plot <- polarityOfReviewEach(brandData, reverse)
  }
  plot
}

emotionOfReview <- function(brandData, reverse, brand_Name){
  brandData <- getData(brandData, brand_Name)
  if(brand_Name == "All Brands"){
    plot <- emotionOfReviewAll(brandData, reverse)
  }else{
    plot <- emotionOfReviewEach(brandData, reverse)
  }
  plot
}

distributionOfRatings <- function(brandData, reverse, brand_Name){
  brandData <- getData(brandData, brand_Name)
  if(brand_Name == "All Brands"){
    plot <- distributionOfRatingsAll(brandData, reverse)
  }else{
    plot <- distributionOfRatingsEach(brandData, reverse)
  }
  plot
}

distributionOfRatingsbyPolarity <- function(brandData, reverse, brand_Name){
  brandData <- getData(brandData, brand_Name)
  if(brand_Name == "All Brands"){
    plot <- distributionOfRatingsbyPolarityAll(brandData, reverse)
  }else{
    plot <- distributionOfRatingsbyPolarityEach(brandData, reverse)
  }
  plot
}

confusionMatrixPlot <- function(dataSample, brand_Name){
  dataSample <- getData(dataSample, brand_Name)
  polaritySample <- dataSample[dataSample$Rating > 3 | dataSample$Rating < 3,]
  polaritySample$ActualPolarity <- ""
  polaritySample[polaritySample$Rating > 3,]$ActualPolarity <- "Positive"
  polaritySample[polaritySample$Rating < 3,]$ActualPolarity <- "Negative"
  confusionMat <- confusionMatrix(as.factor(polaritySample$Polarity), as.factor(polaritySample$ActualPolarity))
  fourfoldplot(confusionMat$table,
               conf.level = 0, margin = 1, main = "Confusion Matrix")
}

distributionOfRatingsbyEmotion <- function(brandData, reverse, brand_Name){
  brandData <- getData(brandData, brand_Name)
  if(brand_Name == "All Brands"){
    plot <- distributionOfRatingsbyEmotionAll(brandData, reverse)
  }else{
    plot <- distributionOfRatingsbyEmotionEach(brandData, reverse)
  }
  plot
}

############################################################################

removeNonASCII <- function(reviewList){
  removeRule <- grep("reviewList", iconv(reviewList, "latin1", "ASCII", sub="reviewList"))
  reviewList[-removeRule]
}

getTermDocMatrix <- function(dataSet){
  sortedData <- dataSet[order(dataSet$Review.Votes,decreasing = TRUE),]
  sampleReview <- sortedData$Reviews
  reviewsDF <- data.frame(sampleReview)
  reviewForCorpus <- reviewsDF$sampleReview
  corpus <- Corpus(VectorSource(reviewForCorpus))
  corpus <- tm_map(corpus, removeWords, c("phone", stopwords("english")))
  trans <- content_transformer(function(x,pattern)gsub(pattern," ",x))
  corpus <- tm_map(corpus, stemDocument)
  corpus <- tm_map(corpus,trans,"\\s+\\w\\s+")
  termMatrix <- TermDocumentMatrix(corpus, control = list(wordLengths = c(1, Inf)))
  termMatrix
}

getDataFrameMatrix <- function(termMatrix){
  matrix <- as.matrix(termMatrix)
  sortMatrix <- sort(rowSums(matrix),decreasing=TRUE)
  dataFrameMatrix <- data.frame(words = names(sortMatrix),freq = sortMatrix)
  dataFrameMatrix
}
############################################################################

wordFreqGraph <- function (dataFrameMatrix){
  ggplot(dataFrameMatrix[1:22,], aes(x = reorder(words, freq), y = freq, fill = freq)) + 
    geom_bar(stat = "identity", col = "grey") + 
    xlab("Words") + ylab("Count") +
    ggtitle("Most Frequent 22 words") +
    coord_flip() +
    scale_fill_gradient("Count", low = "blue", high = "red")
}

wordCloudGraph <- function(dataFrameMatrix){
  wordcloud(dataFrameMatrix$words,dataFrameMatrix$freq, max.words=97,random.order=FALSE,
            rot.per=0.35,colors=brewer.pal(8,"Dark2"))
}

topicModelGraph <- function(dataSet, termMatrix, noOfCluster, noOfTopics){
  docTermMatrix <- as.DocumentTermMatrix(termMatrix)
  wordSumDoc <- apply(docTermMatrix , 1, sum)
  docTermMatrix <- docTermMatrix[wordSumDoc > 0, ]
  docs <- as.integer(docTermMatrix[["dimnames"]][["Docs"]])
  lda <- LDA(docTermMatrix, k = noOfCluster)
  term <- terms(lda, noOfTopics)
  term <- apply(term, MARGIN = 2, paste, collapse = ", ")
  topic <- topics(lda, 1)
  topics <- data.frame(brandName = dataSet[docs,]$Brand.Name, topic)
  plot <- ggplot(data = topics, aes(x=brandName, y = ..count.., fill=term[topic], color = term[topic])) + 
              geom_bar(stat = "count", size = 1, col = "grey") +
              coord_flip() + theme(legend.position = "top") +
              xlab("All Brands") + ylab("Count") +
              facet_wrap(~term[topic])
  plot
}
############################################################################
getTransaction <-  function(dataSample, brand_name){
  dataSample <- getData(dataSample, brand_name)
  dataSampleCamera <- dataSample[order(dataSample$Camera),]
  aprioriDataSample <- unique(dataSampleCamera[,c("CustID","Camera")])
  set.seed(200)
  sample <- sample.split(aprioriDataSample$Camera, SplitRatio = 0.8)
  aprioriDataSample <- subset(aprioriDataSample, sample == TRUE)
  aprioriDataSample$Camera <- paste(aprioriDataSample$Camera, "mp")
  prepareAprioriData <- ddply(aprioriDataSample,c("CustID"), function(subset) paste(subset$Camera,
                                                                                    collapse = ","))
  prepareAprioriData$CustID <- NULL
  write.csv(prepareAprioriData,"cameraList.csv", row.names = TRUE)
  transaction = read.transactions(file="cameraList.csv", rm.duplicates= TRUE, format="basket",sep=",",cols=1)
  transaction@itemInfo$labels <- gsub("\"","",transaction@itemInfo$labels)
  transaction
}
getAssociationPlot <- function(transaction){
  cameraRules <- apriori(transaction, 
                         parameter = list(sup = 0.01, conf = 0.01, target="rules"));
  plot(cameraRules, method = "scatter",  engine = "htmlwidget")
}

getItemFreqPlot <- function(transaction){
  itemFrequencyPlot(transaction, topN = 5,type="absolute",
                    col = brewer.pal(5, "Dark2"), 
                    main="Frequency according to mobile's camera")
}
############################################################################
getClassification <-  function(dataSample, cameraMP, priceMobile){
  cameraMP <- as.double(cameraMP)
  priceMobile <- as.double(priceMobile)
  trainNaive <- naive_bayes(Age ~ Camera + Price, dataSample)
  testNaive <- data.frame(Camera = c(cameraMP), Price  = c(priceMobile))
  predict(trainNaive, testNaive)
}
############################################################################
getSalesForecast <- function(dataSample, forecastYear, brand_name){
  dataSample <- getData(dataSample, brand_name)
  revenueYear <- aggregate(list(Revenue = dataSample$Price), by=list(Year = dataSample$Year), FUN=sum)
  revenueYear <- revenueYear[order(revenueYear$Year),]
  tsRevenue <- ts(revenueYear$Revenue, start = revenueYear$Year[1])
  arimaModel <- auto.arima(tsRevenue)
  forecastRevenue <- forecast(arimaModel, h = forecastYear)
  autoplot(forecastRevenue, xlab = "Year", ylab = "Revenue")
}

############################################################################

getCluster <- function(dataSample, noOfClusters, isFrame, pointSize){
  set.seed(400)
  sample <- sample.split(dataSample$Age, SplitRatio = 0.20)
  dataSampleAge <- subset(dataSample, sample == TRUE)
  set.seed(80)
  custCluster <- kmeans(dataSampleAge[, c("Price","Camera")], noOfClusters, nstart = 80)
  custCluster$cluster <- as.factor(custCluster$cluster)
  plot <-ggplot(dataSampleAge, aes(Price, Camera, col = custCluster$cluster)) + 
        geom_point(aes(shape = dataSampleAge$Age), size = pointSize) + 
        guides(shape = guide_legend("Age"), fill = guide_legend("Clusters"), col = FALSE) + ylim(0, 50)
  if(isFrame){  
      plot <- plot + geom_convexhull(alpha = 0.1, aes(fill = custCluster$cluster)) 
  }
  plot
}

############################################################################

getFeatureData <- function(topBrands, PositiveData, NegativeData){
  categories = c("screen","batteri", "app", "camera", "sim", 
                 "call", "button" , "gb", "play", "qualiti", "price", "music", "featur", 
                 "video","storag", "ram")
  featureData <- data.frame(brandName = character(), feature = character(), freq = numeric(), 
                            polarity = character(), stringsAsFactors = FALSE)
  for(brand in topBrands){
    eachBrandDataPos <- PositiveData[PositiveData$Brand.Name == brand,]
    eachBrandDataNeg <- NegativeData[NegativeData$Brand.Name == brand,]
    posTDM <- getTermDocMatrix(eachBrandDataPos)
    posDFM <- getDataFrameMatrix(posTDM)
    negTDM <- getTermDocMatrix(eachBrandDataNeg)
    negDFM <- getDataFrameMatrix(negTDM)
    for (category in categories) {
      if(category %in% posDFM$words){
        posCategoryFreq <- posDFM[posDFM$words == category,]$freq  
      } else {
        posCategoryFreq <- 0
      }
      
      if(category %in% negDFM$words){
        negCategoryFreq <- negDFM[negDFM$words == category,]$freq
      } else {
        negCategoryFreq <- 0
      }
      featureData[nrow(featureData) + 1,] <- list(brand, category, posCategoryFreq, 'Positive')
      featureData[nrow(featureData) + 1,] <- list(brand, category, negCategoryFreq, 'Negative')    
    }
  }
  featureData
}

############################################################################

featureByPolarity <- function(featureData, polarityValue, reverse){
  plot <- ggplot(data = featureData[featureData$polarity == polarityValue,], aes(x = brandName, y = freq, fill = brandName, color = brandName)) + 
    geom_bar(stat = "identity", size = 1, col = "grey") +
    xlab("All Brands") + ylab("Frequency") + 
    ggtitle(paste(polarityValue,"Features in each Brands")) +
    facet_wrap(~feature)
  if(reverse){
    plot <- plot + coord_flip() + theme(legend.position = "top")
  }
  plot
}

featureByBrand <- function(featureData, brand_Name, reverse){
  plot <- ggplot(data = featureData[featureData$brandName == brand_Name,], aes(x = feature, y = freq, fill = feature, color = feature)) + 
    geom_bar(stat = "identity", size = 1, col = "grey") +
    xlab("Features") + ylab("Frequency") + 
    ggtitle(paste("Features in", brand_Name)) +
    facet_wrap(~polarity)
  if(reverse){
    plot <- plot + coord_flip() + theme(legend.position = "top")
  }
  plot
}
