# Classifying Wine Preferences Based on Physiochemical Properties

The following research was conducted under the guidance of Vivian Lew, Lecturer with UCLA Department of Statistics. The data is 
provided by University of California, Irvine Machine Learning Repository. 

Initially, I submitted a research paper exploring the influence of physiochemical properties of wine on median quality scores given by atleast three experts. I created a shiny application to display the relevant visualizations for exploratory analysis, model performance, and findings. 

## Getting Started

I carried out this research using R Studio: Download [RStudio](https://www.rstudio.com/products/rstudio/download/) 

In addition, I installed and loaded the following packages.

```
install.packages(c("readr", "ggplot2", "plotly", "corrplot", "rpart", "rpart.plot", "randomForest", "shiny", "shinydashboard", "data.table", "caret"))
```

## Deployment

Save the files in the same directory, with the jpg in a www directory. Please note that the dashboard takes a few moments to load.
Run the application with the following command:
```
shinyApp(ui = ui, server = server)
```


## Built With

* [RStudio](https://www.rstudio.com/) - Open source statistical software
* [Shiny](https://shiny.rstudio.com/) - Application framework

## Acknowledgments

* Special thanks to Vivian Lew for her guidance and enthusiasm for learning R 
* P. Cortez, A. Cerdeira, F. Almeida, T. Matos and J. Reis. Modeling wine preferences by data mining from physicochemical properties. In Decision Support Systems, Elsevier, 47(4):547-553, 2009.
