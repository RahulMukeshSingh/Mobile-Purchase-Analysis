library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(plotly)
library(plyr)
library(dplyr)
library(shinycssloaders)
source("E:/R/MobileMethods.R")
setwd("E:/DW and DM Datasets/ProjectMScCSPart2/Amazon Reviews Mobile Phones")
options(shiny.maxRequestSize = 30*1024^2)
############################################################################
# allBrandData <- read.csv("SampleMobileReview.csv", stringsAsFactors = FALSE)
# topBrands <- unique(allBrandData$Brand.Name)
# allBrandName <- c("All Brands", topBrands)
filterCategory <- c("All", "Positive Polarity", "Negative Polarity", "Ratings > 3", "Ratings < 3")
filterPos <- c("Positive Polarity", "Ratings > 3")
filterNeg <- c("Negative Polarity", "Ratings < 3")
allBrandData <- NULL
topBrands <- NULL
allBrandName <- NULL

############################################################################

ui <- dashboardPage(
  ####################
  dashboardHeader(title = "Mobile Brand Analysis"),
  ####################
  dashboardSidebar(sidebarMenu(
    useShinyjs(),
    style = "position: fixed; overflow: visible;",
    menuItem("Review Count", tabName = "review", icon = icon("chart-bar")),
    menuItem("Polarity Analysis", tabName = "polarityAnalysis", icon = icon("poll-h")),
    menuItem("Emotion Analysis", tabName = "emotionAnalysis", icon = icon("grin")),
    menuItem("Distribution Of Ratings", tabName = "distributionOfRatings", icon = icon("star")),
    menuItem("Ratings Vs Polarity", tabName = "ratingsByPolarity", icon = icon("balance-scale")),
    menuItem("Ratings Vs Emotions", tabName = "ratingsByEmotion", icon = icon("balance-scale")),
    menuItem("WordFrequency & WordCloud", tabName = "wordFrequency", icon = icon("cloudsmith")),
    menuItem("Associativity", tabName = "associativity", icon = icon("link")),
    menuItem("Clustering", tabName = "clustering", icon = icon("layer-group")),
    menuItem("Features Analysis", tabName = "features", icon = icon("elementor")),
    menuItem("Topic Model", tabName = "topicModel", icon = icon("chart-bar")),
    menuItem("Sales Forecast", tabName = "salesForecast", icon = icon("chart-area")),
    menuItem("Classification", tabName = "classify", icon = icon("hand-point-right"))
  )),
  ####################
  dashboardBody(
  tags$head(
    includeCSS("E:/R/style.css")
  ),
  tabItems(
    tabItem(tabName = "review",
              fluidRow(
                box(title = p("Mobile Dataset : "), width = 9,
                    fileInput("mobileDataset", "Choose CSV File",
                              accept = c(
                                "text/csv",
                                "text/comma-separated-values",
                                ".csv")
                    )),
                hidden(
                  div(id = "datasetID",
                    infoBox(title = p("No. Of Reviews for all Brands")),
                    box(
                      width = 6, 
                      plotlyOutput("reviewCount", height = 500, width = 500) %>% withSpinner(color="#248f24")
                      )
                ))
              )
            ),
    tabItem(tabName = "polarityAnalysis",
             fluidRow(
               box(title = p("Polarity Count of Review"),
                      selectInput("brandSelectPolarity",label = h4("Select Brand : "),choices = allBrandName),
                      materialSwitch("polarityReverse",label = h4("Flip Co-ordinates ?"),value = FALSE, status = "primary"),
                        actionButton("donePolarityCount", "Done")
                      ),
               hidden(
                 div(id = "polarityAnalysisID",
                 box(
                   width = 9,
                   plotlyOutput("polarityCount", height = 500, width = 600) %>% withSpinner(color="#248f24")
                 )))
            )
          ),
    tabItem(tabName = "emotionAnalysis",
            fluidRow(
              box(title = p("Emotion Count of Reviews"),
                  selectInput("brandSelectEmotion",label = h4("Select Brand : "),choices = allBrandName),
                  materialSwitch("emotionReverse",label = h4("Flip Co-ordinates ?"),value = FALSE, status = "primary"),
                  actionButton("doneEmotionCount", "Done")
              ),
              hidden(
                div(id = "emotionAnalysisID",
                    box(
                      width = 9,
                      plotlyOutput("emotionCount", height = 500, width = 600) %>% withSpinner(color="#248f24")
                    )))
            )
    ),
    tabItem(tabName = "distributionOfRatings",
            fluidRow(
              box(title = p("Distribution of the Ratings"),
                  selectInput("brandSelectRatings",label = h4("Select Brand : "),choices = allBrandName),
                  materialSwitch("distOfRatingsReverse",label = h4("Flip Co-ordinates ?"),value = FALSE, status = "primary"),
                  actionButton("doneDistOfRatings", "Done")
              ),
              hidden(
                div(id = "distributionOfRatingsID",
                box(
                  width = 9,
                  plotlyOutput("distOfRatings", height = 500, width = 600) %>% withSpinner(color="#248f24")
                )))
            )
      ),
    tabItem(tabName = "ratingsByPolarity",
            fluidRow(
              box(title = p("Distribution of the Ratings by Polarity"),
                  selectInput("brandSelectPolarityRatings",label = h4("Select Brand : "),choices = allBrandName),
                  materialSwitch("distOfPolarityRatingsReverse",label = h4("Flip Co-ordinates ?"),value = FALSE, status = "primary"),
                  actionButton("doneDistOfPolarityRatings", "Done")
              ),
              hidden(
                div(id = "ratingsByPolarityID",
                    box(
                      width = 9,
                      plotlyOutput("distOfPolarityRatings", height = 500, width = 500) %>% withSpinner(color="#248f24")
                    ),
                    box(
                      width = 9,
                      plotOutput("confusionMatrixGraph", height = 500, width = 500) %>% withSpinner(color="#248f24")
                    ))
              ))
    ),
    tabItem(tabName = "ratingsByEmotion",
            fluidRow(
              box(title = p("Distribution of the Ratings by Emotions"),
                  selectInput("brandSelectEmotionRatings",label = h4("Select Brand : "),choices = allBrandName),
                  materialSwitch("distOfEmotionRatingsReverse",label = h4("Flip Co-ordinates ?"),value = FALSE, status = "primary"),
                  actionButton("doneDistOfEmotionRatings", "Done")
              ),
              hidden(
                div(id = "ratingsByEmotionID",
                    box(
                      width = 9,
                      plotlyOutput("distOfEmotionRatings", height = 500, width = 500) %>% withSpinner(color="#248f24")
                    )))
            )
    ),
    tabItem(tabName = "wordFrequency",
            fluidRow(
              box(title = p("Word Frequency and WordCloud"),
                  selectInput("brandSelectWordFrequency",label = h4("Select Brand : "),choices = allBrandName),
                  selectInput("filterSelectWordFrequency",label = h4("Filter : "), choices = filterCategory),
                  actionButton("doneWordFrequency", "Done")
              )),
              fluidRow(
                hidden(
                  div(id = "wordFrequencyID",
                      box(
                        width = 6,
                        plotlyOutput("wordFrequencyGraph", height = 500, width = 500) %>% withSpinner(color="#248f24")
                      ),
                      box(
                        width = 6,
                        plotOutput("wordCloudGraph", height = 500, width = 500) %>% withSpinner(color="#248f24")
                      ))
                ))
            ),
    tabItem(tabName = "associativity",
            fluidRow(
              box(title = p("Associative words of Most Frequently used Word"),
                  selectInput("brandSelectAssociativity",label = h4("Select Brand : "),choices = allBrandName),
                  actionButton("doneAssociativity", "Done")
              ),
              hidden(
                div(id = "associativityID",
                    box(
                      width = 9,
                      plotlyOutput("associativityGraph", height = 500, width = 500) %>% withSpinner(color="#248f24")
                    ),
                    box(
                      width = 9,
                      plotOutput("itemFreqGraph", height = 800, width = 800) %>% withSpinner(color="#248f24")
                    )
              ))
            )
      ),
    tabItem(tabName = "clustering",
            fluidRow(
              box(title = p("Clustering"), width = 3,
                  sliderInput("pointSize",label = h4("Point Size : "), min = 0.1, max = 3.0, value = 2.5),
                  materialSwitch("frameConvexHull",label = h4("Frame (Convex Hull) ?"),value = FALSE, status = "primary"),
                  sliderInput("noOfClusters",label = h4("No. Of Clusters : "), min = 0, max = 10, value = 4)
              ),
              hidden(
                div(id = "clusteringID",
                    box(
                      width = 3,
                      plotOutput("clusterGraph", height = 600, width = 800) %>% withSpinner(color="#248f24")
                    ))
              ))
    ),
    tabItem(tabName = "features",
            fluidRow(
              box(title = p("Features"),
                  selectInput("brandSelectFeatures",label = h4("Select Brand : "),choices = allBrandName),
                  selectInput("posFilter",label = h4("Positive Filter : "),choices = filterPos),
                  selectInput("negFilter",label = h4("Negative Filter : "),choices = filterNeg),
                  materialSwitch("featureReverse",label = h4("Flip Co-ordinates ?"),value = TRUE, status = "primary"),
                  actionButton("doneFeature", "Done")
              )),
            fluidRow(
              hidden(
                div(id = "featureID1",
                    box(
                      width = 10,
                      plotlyOutput("featureGraph1", height = 700, width = 700) %>% withSpinner(color="#248f24")
                    ))),
              hidden(
                div(
                  id = "featureID2",
                  box(
                    width = 10,
                    plotlyOutput("featureGraph2", height = 700, width = 700) %>% withSpinner(color="#248f24")
                  ))
              ))
    ),
    tabItem(tabName = "topicModel",
            fluidRow(
              box(title = p("Topics Discussed"),
                  sliderInput("noOfClustersTopics",label = h4("No. Of Clusters : "), min = 0, max = 10, value = 8),
                  sliderInput("noOfTopics",label = h4("No. Of Topics : "), min = 0, max = 10, value = 4),
                  actionButton("doneTopics", "Done")
              ),
              hidden(
                div(id = "topicsID",
                    box(
                      width = 12,
                      plotlyOutput("topicsGraph", height = 800, width = 800) %>% withSpinner(color="#248f24")
                    ))
              ))
    ),
    tabItem(tabName = "salesForecast",
            fluidRow(
              box(title = p("Sales Forecast"),
                  selectInput("brandSelectForecast",label = h4("Select Brand : "),choices = allBrandName),
                  sliderInput("forecastYear",label = h4("No Of Years To Forecast : "), min = 1, max = 10, value = 3),
                  actionButton("doneForecast", "Done")
              ),
              hidden(
                div(id = "forecastID",
                    box(
                      width = 12,
                      plotlyOutput("forecastGraph", height = 800, width = 800) %>% withSpinner(color="#248f24")
                    ))
              ))
    ),
    tabItem(tabName = "classify",
            fluidRow(
              box(title = p("Age Group to Target"),
                  textInput("cameraMP",label = h4("Mobile Camera (in MegaPixels) : "), placeholder = "16.5"),
                  textInput("priceMobile",label = h4("Estimated Price : "), placeholder = "400.5"),
                  actionButton("doneClassify", "Done")
              ),
              hidden(
                div(id = "classifyID",
                 box(
                   width = 7,
                   verbatimTextOutput("ageGroup")
                 ))
              ))
    )
))
  ####################
)

###########################################################################################################################
server <- function(input, output, session) {
  observeEvent(input$mobileDataset,
               {
                 mobileData <- input$mobileDataset
                 allBrandData <<- read.csv(mobileData$datapath, stringsAsFactors = FALSE)
                 topBrands <<- unique(allBrandData$Brand.Name)
                 allBrandName <<- c("All Brands", topBrands)
                 updateSelectInput(session, "brandSelectPolarity", choices = allBrandName)
                 updateSelectInput(session, "brandSelectEmotion", choices = allBrandName)
                 updateSelectInput(session, "brandSelectRatings", choices = allBrandName)
                 updateSelectInput(session, "brandSelectPolarityRatings", choices = allBrandName)
                 updateSelectInput(session, "brandSelectEmotionRatings", choices = allBrandName)
                 updateSelectInput(session, "brandSelectWordFrequency", choices = allBrandName)
                 updateSelectInput(session, "brandSelectAssociativity", choices = allBrandName)
                 updateSelectInput(session, "brandSelectFeatures", choices = allBrandName)
                 updateSelectInput(session, "brandSelectForecast", choices = allBrandName)
                 shinyjs::show("datasetID")
                 output$reviewCount <- renderPlotly({noOfReview(allBrandData)})
               })
  observeEvent(input$donePolarityCount,
                {
                  shinyjs::show("polarityAnalysisID")
                  reverse <- input$polarityReverse
                  brand_name <- input$brandSelectPolarity
                  output$polarityCount <- renderPlotly({polarityOfReview(allBrandData, reverse, brand_name)})
              })
  observeEvent(input$doneEmotionCount,
               {
                 shinyjs::show("emotionAnalysisID")
                 reverse <- input$emotionReverse
                 brand_name <- input$brandSelectEmotion
                 output$emotionCount <- renderPlotly({emotionOfReview(allBrandData, reverse, brand_name)})
               })
  observeEvent(input$doneDistOfRatings,
               {
                 shinyjs::show("distributionOfRatingsID")
                 reverse <- input$distOfRatingsReverse
                 brand_name <- input$brandSelectRatings
                 output$distOfRatings <- renderPlotly({distributionOfRatings(allBrandData, reverse, brand_name)})
               })
  observeEvent(input$doneDistOfPolarityRatings,
               {
                 shinyjs::show("ratingsByPolarityID")
                 reverse <- input$distOfPolarityRatingsReverse
                 brand_name <- input$brandSelectPolarityRatings
                 output$distOfPolarityRatings <- renderPlotly({distributionOfRatingsbyPolarity(allBrandData, reverse, brand_name)})
                 output$confusionMatrixGraph <- renderPlot({confusionMatrixPlot(allBrandData, brand_name)})
              })
  observeEvent(input$doneDistOfEmotionRatings,
               {
                 shinyjs::show("ratingsByEmotionID")
                 reverse <- input$distOfEmotionRatingsReverse
                 brand_name <- input$brandSelectEmotionRatings
                 output$distOfEmotionRatings <- renderPlotly({distributionOfRatingsbyEmotion(allBrandData, reverse, brand_name)})
               })
  observeEvent(input$doneWordFrequency,
               {
                 shinyjs::show("wordFrequencyID")
                 brand_name <- input$brandSelectWordFrequency
                 filterCategory <- input$filterSelectWordFrequency
                 dataSet <- getFilteredData(allBrandData,filterCategory,brand_name)
                 termMatrix <- getTermDocMatrix(dataSet)
                 dataFrameMatrix <- getDataFrameMatrix(termMatrix)
                 output$wordFrequencyGraph <- renderPlotly({wordFreqGraph(dataFrameMatrix)})
                 output$wordCloudGraph <- renderPlot({wordCloudGraph(dataFrameMatrix)})
               })
  observeEvent(input$doneAssociativity,
               {
                 shinyjs::show("associativityID")
                 brand_name <- input$brandSelectAssociativity
                 transaction <- getTransaction(allBrandData, brand_name)
                 output$associativityGraph <- renderPlotly({getAssociationPlot(transaction)})
                 output$itemFreqGraph <- renderPlot({getItemFreqPlot(transaction)})                  
                })
  observeEvent({
                input$pointSize
                input$frameConvexHull
                input$noOfClusters
                },
               {
                 shinyjs::show("clusteringID")
                 pointSize <- input$pointSize
                 isFrame <- input$frameConvexHull
                 noOfClusters <- input$noOfClusters
                 output$clusterGraph <- renderPlot({getCluster(allBrandData, noOfClusters, isFrame, pointSize)})
                })
  observeEvent(input$doneFeature,
               {
                 brand_name <- input$brandSelectFeatures
                 if(brand_name == "All Brands"){
                   shinyjs::show("featureID1")
                   shinyjs::show("featureID2")
                 }else{
                   shinyjs::show("featureID1")
                   shinyjs::hide("featureID2")
                 }
                 posFilter <- input$posFilter
                 posFilterData <- getFilteredData(allBrandData,posFilter,brand_name)
                 negFilter <- input$negFilter
                 negFilterData <- getFilteredData(allBrandData,negFilter,brand_name)
                 featureReverse <- input$featureReverse
                 featureData <- getFeatureData(topBrands, posFilterData, negFilterData)
                 if(brand_name == "All Brands"){
                   output$featureGraph1 <- renderPlotly({featureByPolarity(featureData,'Positive', featureReverse)})
                   output$featureGraph2 <- renderPlotly({featureByPolarity(featureData,'Negative', featureReverse)})
                 }else{
                   output$featureGraph1 <- renderPlotly({featureByBrand(featureData, brand_name, featureReverse)})
                 }
               })
  observeEvent(input$doneTopics,
               {
                 shinyjs::show("topicsID")
                 noOfClusters <- input$noOfClustersTopics
                 noOfTopics <- input$noOfTopics
                 termMatrix <- getTermDocMatrix(allBrandData)
                 output$topicsGraph <- renderPlotly({topicModelGraph(allBrandData, termMatrix, noOfClusters, noOfTopics)})
               })
  observeEvent(input$doneForecast,
               {
                 shinyjs::show("forecastID")
                 brand_name <- input$brandSelectForecast
                 forecastYear <- input$forecastYear
                 output$forecastGraph <- renderPlotly({getSalesForecast(allBrandData,forecastYear,brand_name)})
               })
  observeEvent(input$doneClassify,
               {
                 shinyjs::show("classifyID")
                 cameraMP <- input$cameraMP
                 priceMobile <- input$priceMobile
                 ageGroupText <- paste("Age Group To Target :", getClassification(allBrandData, cameraMP, priceMobile),"years")
                 output$ageGroup <- renderText({ageGroupText})
               })
}

shinyApp(ui, server)