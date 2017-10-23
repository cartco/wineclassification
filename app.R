# load packages
library(shiny)
library(shinydashboard)
library(readr)
library(ggplot2)
library(plotly)
library(corrplot)
library(rpart)
library(rpart.plot)
library(randomForest)
library(rsconnect)


# data cleaning and manipulation
{ 
  # read in data from UCI Machine Learning Repository
    red <- read_delim("winequality-red.csv", ";", escape_double = FALSE, trim_ws = TRUE)
  # two NA values for total sulfur dioxide
  # impute these NAs with the median value (38) 
    red$`total sulfur dioxide`[1296:1297] <- 38
    white <- read_delim("winequality-white.csv", ";", escape_double = FALSE, trim_ws = TRUE)
  # add new type variable (encoding red = 0, white = 1)
    red$type <- 0
    white$type <- 1
  # combine data sets and scale numeric predictors
    data <- rbind(red, white)
    data <- cbind(scale(data[,-c(12,13)]), data[, c(12,13)])
  # assign Quality groupings for classification
    data$rating <- ifelse(data$quality > 6, "excellent", ifelse(data$quality <= 6 & data$quality >= 5, "good", "poor"))
    data$quality <- NULL
    names(data) <- make.names(names(data))
  # partition data into 70% training and 30% testing 
    set.seed(12345)
    samp <- sample(nrow(data), 0.7*nrow(data), replace = FALSE)
    train <- data[samp, ]
    test <- data[-samp, ]
    test.rating <- test$rating
    test$rating <- NULL
  # decision Tree
  # build and print decision tree 
    d.tree <- rpart(rating ~., data = train, method = "class"); d.tree
  # random forest 
    rf1 <- randomForest(as.factor(rating) ~., data = train, importance = TRUE, ntree = 5000, mtry = 5)
    prediction.rf <- predict(rf1, test)
}

# user interface
ui <- {
  dashboardPage(
    # dashboard title
    dashboardHeader(title = "Wine Classification Dashboard", titleWidth = 450),
    # side bar menu
    dashboardSidebar(
         sidebarMenu(
                  menuItem("Analysis", tabName = "intro", icon = icon("glyphicon glyphicon-sort-by-attributes", lib = "glyphicon")),
                  menuItem("Distribution of Ratings", tabName = "dist", icon = icon("glyphicon glyphicon-equalizer", lib = "glyphicon")),
                  menuItem("Variable Correlations", tabName = "cor", icon = icon("glyphicon glyphicon-link", lib = "glyphicon")),
                  menuItem("Decision Tree Structure", tabName = "str", icon = icon("glyphicon glyphicon-tree-deciduous", lib = "glyphicon")),
                  menuItem("Variable Importance", tabName = "rfimp", icon = icon("glyphicon glyphicon-road", lib = "glyphicon"))
                    )
                  ),
    # dashboard body
    dashboardBody(
         tabItems(
            # intro
            tabItem(tabName = "intro",
                  fluidPage(
                    fluidRow(box(img(src='winecountry.jpg', align = "left"), title = "Predicting Wine Preferences Based on Physiochemical Properties", width = 6),
                             box('In order to gain a deeper understanding of the data, it is typical to conduct exploratory analysis. The following information can be observed in the Distributions of ratings tab: There are more white wine samples (4898) than red wine samples (1599). In addition, the distribution of ratings for both types of wine are centered around the means with few outliers. It also is important to note that a few of the predictors are correlated. I will choose the predictors that describe the output variable and remove highly correlated predictors to improve model performance. It is important to address the multicollinearity problem when implementing models that assume predictors are uncorrelated in nature. In the red wine data set citric acid, fixed acidity, and volatile acidity are correlated, more so than in the white wine data set. Fixed acidity is also correlated with denisty and pH. Feature selection removes the negative effects of multicollinearity and ensures model acceptance. In order to frame this data as a classification problem, I coded the quality score into a rating system. Wine with a quality score less than 5 received a poor rating, between 5 and 6 a good rating, and greater than 6 an excellent rating. I implemented this rating system based on the distribution of quality scores shown below. I included the wine type in my models by encoding red equal to 0 and white equal to 1; however, the variable importance plot confirmed my belief that the wine type does not influence the quality score. If the critics were skewed to favor red wine over white wine, this variable would be significant in the model. Data robustness is essential when researching the implications of specific factors on an outcome.', title = "Exploratory Analysis")),
                    fluidRow(box('The following research is focused on wine preferences data collected by Cortez et al. 2009. The data is made available by the University of California, Irvine Machine Learning Repository. The data pertains to Vino Verde red and white wines from the Minho region in Portugal and consists of 11 input variables, and 1 output variable, describing the median quality score from at least 3 wine critic evaluations. 0 describes a very bad score and 10 a very excellent score. The data is partitioned into a white and red wine dataset, but I will combine them into a single training set and assign a new variable "type" to describe whether the wine is red or white. The purpose of this paper is to explore which statistical models can identify the significant physiochemical properties of wine and produce a useful classification model for quality scoring. I am using R Studio to conduct my research, machine learning computation, and inclusion of code chunks to support my findings.', title = "Introduction"), 
                             box('Prior to building statistical models statisticians must clean, transform, and manipulate the data in order to avoid computational issues and improve feature engineering. The data set provided is very clean compared to data scraped from the web, reducing the need for additional work. As mentioned above, I encoded the wine type as a new variable to maintain data integrity even though this variable may not contribute to the classification model. In addition, I normalized the numeric input variables to reduce the likelihood that arbitrary differences in magnitude influence the model. Standardizing the data also improves the convergence rate of support vector machines and other gradient based algorithms. I implemented the rating system by encoding wine with a quality score less than 5 receiving a poor rating, between 5 and 6 a good rating, and greater than 6 an excellent rating. Next, I removed the quality score from the list of predictors and renamed the predictors to avoid issues when building statistical models. Finally, I partitioned the data into a training (70%) and testing (30%) set.', title = "Data Pre-processing")),
                    fluidRow(box('K-Nearest Neighbors is a useful non-parametric algorithm for classification and regression problems. I implemented KNN utilizing the caret package and cross validation training control. Cross validation is helpful when estimating model accuracy and ability to generalize to new data. I chose a 10 fold cross validation method for simplicity and the algorithm optimized KNN on 7 nearest neighbors. K-Nearest Neighbors is the first model I ran and it resulted in an misclassification rate of 20.41% on the testing data.', title = "K-Nearest Neighbors"),
                             box('Wine preferences are based on qualia, a number of heuristic qualities that differ between critics. The physiochemical properties of wine contribute to favorable or unfavorable qualities and it is useful to understand which of these properties are significant when determining which wines will be received well. The data targets physiochemical properties and describes the median quality score for at least three reviews per sample. This data can be modeled using a variety of methods; I utilized K-nearest neighbors, a decision tree, and a random forest to build a model for classification of ratings. While each of these models have their merits, the random forest produced the most accurate classification based on the test data and allowed for variable importance measures. Some of the input variables are correlated which reduces their ability to influence the model, and alcohol content proved to be the most significant predictor. In addition, free sulfur dioxide, volatile acidity, sulphates, and residual sugar were significant predictors for this classification problem. The models struggled to identify the poor rated wines which implies that further efforts should focus on the signals to identify lower quality wines. Future research on non-physiochemical properties such as brand, grape type, price, age, and other factors could improve upon this study.', title = "Findings and Conclusion")),
                             box('P. Cortez, A. Cerdeira, F. Almeida, T. Matos and J. Reis. 
                                  Modeling wine preferences by data mining from physicochemical properties. 
                                  In Decision Support Systems, Elsevier, 47(4):547-553. ISSN: 0167-9236.', title = "Citation", collapsible = TRUE)       
                            )
                    ),
            # distribution of quality scores with reactive data set and slider input 
            tabItem(tabName = "dist",
                  h2("Distribution of Ratings"),
                    fluidRow(box(sliderInput("n", label = h3("Number of Observations:"), min = 50, max = nrow(white), value = 50, ticks = FALSE), width = 6),
                             box(h3("There are more samples of white wine resulting in a normal shaped distribution. There are few outliers and the distributions are centered around the mean. Use the slider to select the number of observations."))),
                    fluidRow(box(plotlyOutput("redPlot", width = "100%")), box(plotlyOutput("whitePlot", width = "100%"))) 
                  ),
            # correlation plots
            tabItem(tabName = "cor",
                  h2("Variable Correlations"),
                    fluidRow(box(plotOutput("corplot1", width = "100%")), 
                             box(plotOutput("corplot2", width = "100%"))),
                             box('Utilizing the corrplot package is helpful when determining which predictors are correlated with each other and with the outcome variable.')
                    ),
            # decision tree structure
            tabItem(tabName = "str",
                  h2("Decision Tree Structure"),
                    box(plotOutput("decisiontree", width = "100%"), width = 6),
                    box('The decision tree algorithm built in the rpart package is another method for solving for classification problems. It is useful when considering which predictors are significant. The plot shows where the algorithm creates a split on predictors in order to improve classification by decision nodes. Alcohol followed by a few other predictors appear to be significant in this model. The misclassification rate was slightly higher than the KNN model at 21.589%, but is a significant improvement in understanding the relationship between the input and output variables.', title = "Decision Tree Results")
                    ),
            # random forest variable importance
            tabItem(tabName = "rfimp",
                  h2("Random Forest Variable Importance"),
                    box(plotOutput("rfvarimportance", width = "100%")),
                    box('While the KNN model and decision tree performed well, I decided to continue testing other methods of classification. Random forests build on many decision trees by taking advantage of the overfitting problem. With 5000 trees and 5 variables sampled as candidates at each split, I improved the misclassifcation rate to 15.538%. A 5% improvement in classification problems is significant and the random forest proved to be the best model. In addition to improving accuracy, the random forest package in R includes a variable importance plotting function which is useful when improving feature selection. As shown in the graph below, Alcohol, Volatile Acidity, Density, Free Sulfur Dioxide, Sulphates, and Residual Sugar were important to the model. As previously discussed, the type of wine did not influence the quality score and rating.', title = "Random Forest Results")
                    )
                )
          ), skin = "red"
     )
}

# shiny server
server <- function(input, output) {
  # reactive histogram data
  reddata <- reactive({
                if (input$n > 1599) {
                  samp <- sample(nrow(red), 1599)
                  } else {
                samp <- sample(nrow(red), input$n)
                  }
                red[samp, ]     
                })
  # reactive histogram data 
  whitedata <- reactive({
                samp1 <- sample(nrow(white), input$n)
                white[samp1, ]
                })
  # red histogram
  output$redPlot <- renderPlotly({
            red1 <- reddata()
            distred <- ggplot(data = red1, aes(quality)) + geom_bar(aes(fill = factor(quality))) + scale_fill_manual(breaks = c("3", "4", "5", "6", "7", "8"), values = c("darkred", "#DE1A1A", "#FF5555", "#FF6565", "#FF8E8E", "pink")) + labs(title = "Distribution of Red Wine Quality Scores\n", x = "Quality Score\n", y = "Count\n") + theme(legend.position="none") + scale_x_continuous(breaks = round(seq(min(red$quality), max(red$quality), by = 1),1))
            ggplotly(distred, tooltip = "Count")
            })
  # white histogram
  output$whitePlot <- renderPlotly({
            white1 <- whitedata()
            distwhite <- ggplot(data = white1, aes(quality)) + geom_bar(aes(fill = factor(quality))) + scale_fill_manual(breaks = c("3", "4", "5", "6", "7", "8", "9"), values = c("darkred", "#DE1A1A", "#FF5555", "#FF6565", "#FF8E8E", "pink", "black")) + labs(title = "Distribution of White Wine Quality Scores\n", x = "Quality Score\n", y = "Count\n") + theme(legend.position="none") + scale_x_continuous(breaks = round(seq(min(white$quality), max(white$quality), by = 1),1))
            ggplotly(distwhite, tooltip = c("x", "y"))
            })
  # red correlation plot
  output$corplot1 <- renderPlot({
          red.cor <- cor(red[,-13])
          corrplot(red.cor, title = "Correlation between Predictors; Red Wine", mar = c(0,0,2,0))
          })
  # white correlation plot
  output$corplot2 <- renderPlot({ 
        white.cor <- cor(white[,-13])
        corrplot(white.cor, title = "Correlation between Predictors; White Wine", mar = c(0,0,2,0))
        })
  # plot decision tree
  output$decisiontree <- renderPlot({
    rpart.plot(d.tree, main = "Decision Tree Structure")
    })
  # random forest variable importance plot
  output$rfvarimportance <- renderPlot({
    # Variable Importance
    varImpPlot(rf1, main = "Variable Importance")
    }) 
}

# run the application 
shinyApp(ui = ui, server = server)

