#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(caret)
library(dplyr)
library(e1071)
library(klaR)
library(randomForest)
library(pROC)
library(stringi)
library(NeuralNetTools)
library(kernlab)
load("credit_raw.Rdata")

# Define UI for application that draws a histogram

ui <- fluidPage(
  titlePanel("Predict Models for Credit Card Default"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(id = "tabset",
                  tabPanel("Data Cleaning",
                           helpText("Click Clean Data to delete NAs in data set and change some variables into factor."),
                           actionButton("datacleaning", "Clean Data")
                  ),
                  tabPanel("Data Pre-processing",
                           helpText("Click Scale Data to Scale numeric data with center"),
                           actionButton("datascale", "Scale Data"),
                           helpText("Click Create Dummy Variable to change factors into dummy variables."),
                           helpText("Note: if you're going to use Random Forest only, you don't have to do it"),
                           actionButton("dummy", "Create Dummy Variable")
                           
                  ),
                  tabPanel("Split Dataset",

                           numericInput("train", "Proportion of Train Data", 0.6,min = 0, max = 1, step = 0.1),
                           numericInput("validation", "Proportion of Validation Data", 0.3,min = 0, max = 1, step = 0.1),
                           numericInput("test", "Proportion of test Data", 0.1,min = 0, max = 1, step = 0.1),
                           # Include clarifying text ----
                           helpText("Note: The sum of all three proportions should be 1."),
                           actionButton("split", "Split")
                  ),
                  tabPanel("Chose Model",
                          h4("Random Forest"),
                          numericInput("mtry", "mtry", 5,min = 1, max = 23, step = 1),
                          numericInput("ntree", "ntree", 500,min = 1, max = 1000, step = 100),
                          actionButton("rf", "Run Random Forest"),
                          h4("SVM"),
                          numericInput("C", "C", 0.5,min = 0, max = 10, step = 0.1),
                          numericInput("gama", "gama", 0.5,min = 0, max = 1, step = 0.01),
                          actionButton("svm","Run SVM"),
                          h4("Neural Network"),
                          numericInput("size", "size", 3,min = 1, max = 10, step = 1),
                          numericInput("decay", "decay", 0.005,min = 0, max = 1, step = 0.001),
                          actionButton("nn","Run Neural Network")
                  )
              )
    ),
    mainPanel(
      h4("Summary"),
      verbatimTextOutput(outputId = "data"),
      verbatimTextOutput(outputId = "scale"),
      verbatimTextOutput(outputId = 'dummyoutput'),
      verbatimTextOutput(outputId = 'datasplit'),
      verbatimTextOutput(outputId = 'rfoutput'),
      verbatimTextOutput(outputId = 'svmoutput'),
      verbatimTextOutput(outputId = 'nnoutput')
      
    )

  ),
  sidebarLayout(
    sidebarPanel(
      
    ),
    mainPanel(
      h4("Plot"),
      plotOutput(outputId = 'rfplot'),
      plotOutput(outputId = 'svmplot'),
      plotOutput(outputId = 'nnplot'),
      plotOutput(outputId = 'nnplot2')
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  v <- reactiveValues(data = NULL)
  observeEvent(input$datacleaning, {
    v$data <- 1
  })
  
  observeEvent(input$datascale, {
    v$data <- 2
  })
  observeEvent(input$dummy, {
    v$data <- 3
  })
  observeEvent(input$split, {
    v$data <- 4
  })
  observeEvent(input$rf, {
    v$data <- 5
  })
  observeEvent(input$svm, {
    v$data <- 6
  })
  observeEvent(input$nn, {
    v$data <- 7
  })
  output$data<-renderPrint({
    if (is.null(v$data)) return()
    if (v$data == 1){
      credit_raw1 <- credit_raw[,-1]
      credit_raw1$EDUCATION[credit_raw1$EDUCATION==5 | credit_raw1$EDUCATION == 6] <- 0
      credit_raw1 <- credit_raw1[-which(credit_raw1$EDUCATION == 0),]
      credit_raw1 <- credit_raw1[-which(credit_raw1$MARRIAGE == 0),]
      credit_df <- credit_raw1
      credit_df$SEX <-factor(credit_df$SEX)
      credit_df$EDUCATION <- factor(credit_df$EDUCATION)
      credit_df$MARRIAGE <- factor(credit_df$MARRIAGE)
      credit_df$default.payment.next.month<-factor(credit_df$default.payment.next.month)
      str(credit_df)
      print('Data is cleaned')
    }

  })
  
  output$scale<-renderPrint({
    if (is.null(v$data)) return()
    if (v$data == 2){
      preProcess_model <- preProcess(x = credit_df[, c(1, 5, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22,23)], method = c("center", "scale"))
      credit_df[, c(1, 5, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23 )] <-
        predict(object = preProcess_model, newdata = credit_df[, c(1, 5, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)])
      str(credit_df)
        }
  })
  output$dummyoutput<-renderPrint({
    if (is.null(v$data)) return()
    if (v$data == 3){
      dataset_withdummy <-predict(dummyVars_model, newdata = credit_df[,!(colnames(credit_df) == "default.payment.next.month")])
      dataset_withdummy <-as.data.frame(dataset_withdummy)
      nearZeroVar_columns <- nearZeroVar(dataset_withdummy)
      dataset_withdummy <-dataset_withdummy[,-nearZeroVar_columns]
      dataset_withdummy <-
        cbind(
          dataset_withdummy,
          default.payment.next.month = credit_df$default.payment.next.month
        )
      dataset_withdummy=dataset_withdummy[,-3]
      str(dataset_withdummy)
    }
  })
  output$datasplit<-renderPrint({
    if (is.null(v$data)) return()
    if (v$data == 4){
      index <- sample(x = 3,size = nrow(dataset_withdummy),replace=TRUE,prob = c(input$train,input$validation,input$test))
      training <- dataset_withdummy[index == 1,]
      validation <- dataset_withdummy[index == 2,]
      test <- dataset_withdummy[index == 3,]
      print('Number of Rows in Train Data:')
      print(nrow(training))
      print('Number of Rows in Validation Data:')
      print(nrow(validation))
      print('Number of Rows in Test Data:')
      print(nrow(test))
      
    }

  })
  output$rfoutput <- renderPrint({
    if (is.null(v$data)) return()
    if (v$data == 5){
      index <- sample(x = 3,size = nrow(dataset_withdummy),replace=TRUE,prob = c(input$train,input$validation,input$test))
      training <- dataset_withdummy[index == 1,]
      validation <- dataset_withdummy[index == 2,]
      test <- dataset_withdummy[index == 3,]
      T <- Sys.time()
      mtry <- input$mtry
      ntree <- input$ntree
      # tunegrid <- expand.grid(.mtry=mtry)
      # rf_default <- train(default.payment.next.month~., 
      #                     data=training, 
      #                     method='rf', 
      #                     metric='Accuracy',
      #                     ntree = ntree,
      #                     tuneGrid=tunegrid)
      rf_try1 <- randomForest(default.payment.next.month~.,
                              training,
                              mtry = input$mtry,
                              ntree = input$ntree, importance = TRUE)
      require(pROC)
      print(head(rf_try1$votes))
      print(Sys.time() - T)
      predict <- predict(rf_try1, newdata = validation)
      print(confusionMatrix(predict, validation$default.payment.next.month))
    }
    
  })  
  output$rfplot<-renderPlot({
    if (is.null(v$data)) return()
    if (v$data == 5){
      index <- sample(x = 3,size = nrow(dataset_withdummy),replace=TRUE,prob = c(input$train,input$validation,input$test))
      training <- dataset_withdummy[index == 1,]
      validation <- dataset_withdummy[index == 2,]
      test <- dataset_withdummy[index == 3,]
      T <- Sys.time()
      mtry <- input$mtry
      ntree <- input$ntree
      # tunegrid <- expand.grid(.mtry=mtry)
      # rf_default <- train(default.payment.next.month~., 
      #                     data=training, 
      #                     method='rf', 
      #                     metric='Accuracy',
      #                     ntree = ntree,
      #                     tuneGrid=tunegrid)
      rf_try1 <- randomForest(default.payment.next.month~.,
                              training,
                              mtry = input$mtry,
                              ntree = input$ntree, importance = TRUE)
      rf.roc<-roc(training$default.payment.next.month,rf_try1$votes[,2])
      plot(rf.roc)
      auc(rf.roc)
    }

  })
  
  
  output$svmoutput <- renderPrint({
    if (is.null(v$data)) return()
    if (v$data == 6){
      index <- sample(x = 3,size = nrow(dataset_withdummy),replace=TRUE,prob = c(input$train,input$validation,input$test))
      training <- dataset_withdummy[index == 1,]
      validation <- dataset_withdummy[index == 2,]
      test <- dataset_withdummy[index == 3,]
      C=input$C
      gama=input$gama
      T <- Sys.time()
      svm <- svm(default.payment.next.month ~ ., data=train, kernel="radial", cost=C, gamma=gama)
      summary(svm)
      pred <- predict(svm,validation)
      print(table(pred,validation$default.payment.next.month))
      
      # model <-  train(default.payment.next.month ~ ., data = training, method="svmRadial", 
      #                 trControl=trainControl(method='cv', number=3))
      # # print(confusionMatrix(model))
      # Pred <- predict(model, newdata =validation)
      # print(confusionMatrix(Pred, validation$default.payment.next.month))
      # print(model$bestTune)
      print(Sys.time() - T)
    }
    
  })
  output$svmplot<-renderPlot({
    if (is.null(v$data)) return()
    if (v$data == 6){
      index <- sample(x = 3,size = nrow(dataset_withdummy),replace=TRUE,prob = c(input$train,input$validation,input$test))
      training <- dataset_withdummy[index == 1,]
      validation <- dataset_withdummy[index == 2,]
      test <- dataset_withdummy[index == 3,]
      model <-  train(default.payment.next.month ~ ., data = training, method="svmRadial", 
                      trControl=trainControl(method='cv', number=3))
      # print(confusionMatrix(model))
      Pred <- predict(model, newdata =validation)
      roc_curve <- roc(validation$default.payment.next.month, as.numeric(Pred))
      plot(roc_curve)
     
    }
    
  })
  
  output$nnoutput <- renderPrint({
    if (is.null(v$data)) return()
    if (v$data == 7){
      index <- sample(x = 3,size = nrow(dataset_withdummy),replace=TRUE,prob = c(input$train,input$validation,input$test))
      training <- dataset_withdummy[index == 1,]
      validation <- dataset_withdummy[index == 2,]
      test <- dataset_withdummy[index == 3,]
      trainControl <- trainControl(method = "repeatedcv",#Number of time specified we will repeat the CV for different folds of data.cv:1 Iteration is complete for all folds of data.
                                   number = 3,#Number of folds in which we want our data to be divided.
                                   repeats = 2)#Number of times we want CV to repeat.
      nnetTunegrid <- expand.grid(size = c(input$size),#Started with 3, 5, 7, 10 - then based on results increased and decreased numbers and reached at this value.
                                  decay =c(input$decay))#Started with - c(5 * 10^(-5:-1), 0.25 ) and reached at the value used.
      T <- Sys.time()
      neuralnet_model <-train(
        default.payment.next.month ~ . ,
        data = training,
        method = "nnet",
        trControl = trainControl,
        maxit = 200,#Run for Iterations
        rang = 0.8,#Not tunable - Intial Weights
        tuneGrid= nnetTunegrid         # Alternative tuneLength
      )
      print(Sys.time() - T)
      print("-- Value of Pramaters with best Avg. Accuracy --")
      print(head(neuralnet_model$bestTune))
      class_pred <- predict(neuralnet_model, newdata=validation)
      confusionMatrix(as.factor(class_pred),
                      as.factor(validation$default.payment.next.month),
                      positive = "1", # More interest in its accuracy. Change it to 0!!
                      dnn=c("predictions","actual"),
                      mode="prec_recall")
      plotnet(neuralnet_model)
    }
    
  })
  
  output$nnplot<-renderPlot({
    if (is.null(v$data)) return()
    if (v$data == 7){
      index <- sample(x = 3,size = nrow(dataset_withdummy),replace=TRUE,prob = c(input$train,input$validation,input$test))
      training <- dataset_withdummy[index == 1,]
      validation <- dataset_withdummy[index == 2,]
      test <- dataset_withdummy[index == 3,]
      trainControl <- trainControl(method = "repeatedcv",#Number of time specified we will repeat the CV for different folds of data.cv:1 Iteration is complete for all folds of data.
                                   number = 3,#Number of folds in which we want our data to be divided.
                                   repeats = 2)#Number of times we want CV to repeat.
      nnetTunegrid <- expand.grid(size = c(input$size),#Started with 3, 5, 7, 10 - then based on results increased and decreased numbers and reached at this value.
                                  decay =c(input$decay))#Started with - c(5 * 10^(-5:-1), 0.25 ) and reached at the value used.
      T <- Sys.time()
      neuralnet_model <-train(
        default.payment.next.month ~ . ,
        data = training,
        method = "nnet",
        trControl = trainControl,
        maxit = 500,#Run for Iterations
        rang = 0.8,#Not tunable - Intial Weights
        tuneGrid= nnetTunegrid         # Alternative tuneLength
      )
      print(Sys.time() - T)
      prob_pred <- predict(neuralnet_model, validation,type="prob")
      df_roc<-roc(validation$default.payment.next.month,
                  prob_pred[,2],
                  direction =  "auto")
      plot(df_roc, main="AUC" %s+% df_roc$auc)  # %s+% is from stringi package
      # plotnet(neuralnet_model)
    }
    
  })
  
  output$nnplot2<-renderPlot({
    if (is.null(v$data)) return()
    if (v$data == 7){
      index <- sample(x = 3,size = nrow(dataset_withdummy),replace=TRUE,prob = c(input$train,input$validation,input$test))
      training <- dataset_withdummy[index == 1,]
      validation <- dataset_withdummy[index == 2,]
      test <- dataset_withdummy[index == 3,]
      trainControl <- trainControl(method = "repeatedcv",#Number of time specified we will repeat the CV for different folds of data.cv:1 Iteration is complete for all folds of data.
                                   number = 3,#Number of folds in which we want our data to be divided.
                                   repeats = 2)#Number of times we want CV to repeat.
      nnetTunegrid <- expand.grid(size = c(input$size),#Started with 3, 5, 7, 10 - then based on results increased and decreased numbers and reached at this value.
                                  decay =c(input$decay))#Started with - c(5 * 10^(-5:-1), 0.25 ) and reached at the value used.
      T <- Sys.time()
      neuralnet_model <-train(
        default.payment.next.month ~ . ,
        data = training,
        method = "nnet",
        trControl = trainControl,
        maxit = 500,#Run for Iterations
        rang = 0.8,#Not tunable - Intial Weights
        tuneGrid= nnetTunegrid         # Alternative tuneLength
      )
      print(Sys.time() - T)
      plotnet(neuralnet_model)
    }
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

