library(caret)
library(randomForest)
library('tidyverse')
library('plyr')
library('naivebayes')


testing <- read.csv('Testing.csv')
testingvariables <- testing[,c(1:132)]
training <- read.csv('Training.csv')
training <- training[,-134]
trainingvariables <- training[,c(1:132)]

choices <- colnames(training) %>% str_replace_all('_|\\.|[0-9]', ' ') %>% 
    str_to_title()
choices_01 <- colnames(training[,c(1:132)]) %>% str_replace_all('_|\\.|[0-9]', ' ') %>%
    str_to_title()

colnames(training) <- choices
colnames(trainingvariables) <- choices_01
colnames(testing) <- choices
colnames(testingvariables) <- choices_01

names(training) <- make.names(names(training))
names(trainingvariables) <- make.names(names(trainingvariables))
names(testing) <- make.names(names(testing))
names(testingvariables) <- make.names(names(testingvariables))

load('RandomForestModel.Rdata') 

ui <- fluidPage(

    # Application title
    titlePanel("Disease Prediction Application"),
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput("Conditions",
                        "Conditions:",
                        choices = unique(choices_01)),
        actionButton("button", "Run Model and Generate Results")),
        mainPanel(
           tableOutput("distPlot")
        )
    )
)

server <- function(input, output) {
    
            randomVals <- observeEvent(input$button, {
            sample <- matrix(NA, nrow = 132)
            sample <- data.frame(sample)
            sample[,1] <- choices_01
 
            results <- c(input$Conditions)
            print(results)
            transformed_results <- c()
            for (i in results) {
                new_results <- paste0('^',i,'$')
                transformed_results <- c(transformed_results, new_results)
            }
    
            print(results)
            sample$results <- as.numeric(str_detect(sample[,1],
                                  paste(transformed_results, collapse='|')))
            
            sample$results <- as.character(sample$results)
            print(sample)
            sample <- data.frame(t(sample))
            sample <- sample[-1,]
            colnames(sample) <- colnames(training[,c(1:132)])
            rownames(sample) <- c(1:nrow(sample))
            
            Model <- naive_bayes(as.factor(Prognosis) ~ ., data = training, 
                                      method = 'class')
            Predictions <- predict(Model, newdata = testingvariables)
            Predictions
            ConfusionMatrix <- table(testing$Prognosis, Predictions)
            confusionMatrix(ConfusionMatrix)
            
            total <- rbind(trainingvariables, testingvariables)
            
            if (nrow(match_df(total, sample) != 0)) {
                sample <- match_df(total, sample)[1,]
            } else {
                sample <- data.frame(t(as.numeric(sample)))
            }
            
            Predictions <- predict(Model, newdata = sample, type='prob')
            Predictions <- data.frame(t(Predictions))

            Predictions$Condition <- rownames(Predictions)

            Predictions <- Predictions %>% arrange(desc
                            (Predictions[,grepl('^t', colnames(Predictions))]))
            Predictions <- Predictions %>% relocate(Condition) %>% slice(1:7)
            Predictions$t.Predictions. <- as.character(Predictions$t.Predictions.)
            
           output$distPlot <- renderTable({
                print(Predictions)
             
             # output$distPlot <- renderPlot({
             # x    <- Predictions[c(1:132)] 
             # bins <- seq(min(x), max(x), length.out = input$bins + 1)
             
             # draw the histogram with the specified number of bins
             # hist(x, breaks = bins, col = 'darkgray', border = 'white')
             #})
            })
        })
    }

shinyApp(ui = ui, server = server)
