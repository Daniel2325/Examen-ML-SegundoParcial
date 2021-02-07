#
# # This is the server logic for a Shiny web application.
#

library(shiny)
library(class)
library(caret)
library(randomForest)
library(e1071)
library(naivebayes)
library(MASS)
library(neuralnet)
library(e1071)
library(caTools)
library(caret)
library(RColorBrewer)
library(readr)
data("iris")
NormIris <- as.data.frame( scale( iris[, 1:4 ], scale = TRUE, center = TRUE ) )
summary( NormIris )
iris.train <- iris[ sample( c( 1:150 ), 100 ), 1:5 ]
iris.test <- iris[ sample( c( 1:150 ), 50 ), 1:5 ]
iris.pred <- knn( train = iris.train[ , 1:4 ], test = iris.test[ , 1:4 ],cl = iris.train[ ,5 ], k = 2 )
con1 = confusionMatrix(iris.pred,iris.test[,5])
con1
accuracy1 = con1$overall[1]
precision1 = mean(con1$byClass[,5])
recall1 = mean(con1$byClass[,6])
f11 = mean(con1$byClass[,7])
## ------------------ RANDOM FOREST IRIS -----------------------
ind <- sample(2,nrow(iris),replace=TRUE,prob=c(0.7,0.3))
trainData <- iris[ind==1,]
testData <- iris[ind==2,]
iris_rf <- randomForest(Species~.,data=trainData,ntree=100,proximity=TRUE)
irisPred<-predict(iris_rf,newdata=testData)
con2 = confusionMatrix(irisPred,testData$Species)
con2
accuracy2 = con2$overall[1]
precision2 = mean(con2$byClass[,5])
recall2 = mean(con2$byClass[,6])
f12 = mean(con2$byClass[,7])

#------- Bayes ---------
classifier<-naiveBayes(iris[,1:4], iris[,5]) 
predicted.classes <- predict(classifier, iris[,-5])
predicted.classes
table(predicted.classes, iris[,5], dnn=list('predicted','actual'))
con3 = confusionMatrix(predicted.classes, iris[,5])
con3
accuracy3 = con3$overall[1]
precision3 = mean(con3$byClass[,5])
recall3 = mean(con3$byClass[,6])
f13 = mean(con3$byClass[,7])

### -------------------- Regresion logistica binaria

modelo<-lda(Species~.,data=iris)
prediccion<-predict(modelo,iris[-5])
m_confusion<-table(iris$Species,prediccion$class,dnn=c("Real","Predicho"))
con4 = confusionMatrix(iris$Species,prediccion$class,dnn=c("Real","Predicho"))
con4
accuracy4 = con4$overall[1]
precision4 = mean(con4$byClass[,5])
recall4 = mean(con4$byClass[,6])
f14 = mean(con4$byClass[,7])

### ----------------------- Redes Neuronales
iris.train <- cbind( iris.train, iris.train$Species == "setosa" )
iris.train <- cbind( iris.train, iris.train$Species == "versicolor" )
iris.train <- cbind( iris.train, iris.train$Species == "virginica" )
names(iris.train)[6]<-"setosa"
names(iris.train)[7]<-"versicolor"
names(iris.train)[8]<-"virginica"
iris.nnt <- neuralnet( setosa + versicolor + virginica ~ Sepal.Length +
                         Sepal.Width +
                         Petal.Length +
                         Petal.Width, 
                       data = iris.train, hidden = c( 3, 2 ) )
# print(iris.nnt) #Ejecutar para ver los resultados
plot( iris.nnt, col.intercept = "blue" , rep="best")
pred.nn <- compute( iris.nnt, iris.test[ 1:4 ] )

resultado <- 0
for ( i in 1:dim( pred.nn$net.result )[1] ){
  resultado[i] <- which.max( pred.nn$net.result[ i, ] )
}
resultado[ resultado == 1 ] <- "setosa"
resultado[ resultado == 2 ] <- "versicolor"
resultado[ resultado == 3 ] <- "virginica"
con5 = confusionMatrix(iris$Species,prediccion$class,dnn=c("Real","Predicho"))
con5
accuracy5 = con5$overall[1]
precision5 = mean(con5$byClass[,5])
recall5 = mean(con5$byClass[,6])
f15 = mean(con5$byClass[,7])

####------------------------------
dataset = iris
### Random Cross Validation
set.seed(2)
split = sample.split(dataset$Species, SplitRatio = .7)
summary(split)

### Training
training_set = subset(dataset, split == TRUE)

### Test
test_set = subset(dataset, split == FALSE)


### Entrenar el Clasificador
# Funcion kernel determina la funcion y va aprendiendo la curva
classifier1 = svm(formula = Species~., data = training_set, 
                  type = 'C-classification', kernel = 'linear')
test_pred1 = predict(classifier1, type = 'response', 
                     newdata = test_set[-5])
con6 = confusionMatrix(test_pred1,test_set[,5])
con6
accuracy6 = con6$overall[1]
precision6 = mean(con6$byClass[,5])
recall6 = mean(con6$byClass[,6])
f16 = mean(con6$byClass[,7])

###========================= Graficas de compraracion de metodos ======================

knnAcurracy = c(accuracy1) 
forestAcurracy = c(accuracy2)
bayesAccuracy = c(accuracy3)
regAccuracy = c(accuracy4)
redesAccuracy = c(accuracy5)
svmAccuracy = c(accuracy6)
accuracyTable = c(knnAcurracy,forestAcurracy,bayesAccuracy,regAccuracy,redesAccuracy,svmAccuracy)
color_d=c("yellow ","red","orange","cyan","red","blue")


presisionTable =c(knnPressision=precision1,forestPressision=precision2,bayesPressision=precision3,regPressision=precision4,redesPressision=precision5,svmPressision=precision6)
recallTable = c(recallKnn=recall1,recallForest=recall2,recallBayes=recall3,recaallRegresion=recall4,recallRedes=recall5,recallSvm=recall6)
measureTable=c(Knn=f11,RandomFores=f12,NaiveBayes=f13,RegresionLog=f14,RedesNeuronales=f15,SVM=f16)

#### ============== Esto es la parte del alfred que no vale ================
wine <- read_csv("./wine.csv", 
                 col_names = FALSE)

NormWine <- as.data.frame( scale( wine[, 2:14 ], scale = TRUE, center = TRUE ) )
summary( NormWine )
wine.train <- wine[ sample( c( 1:178 ), 124), 1:14 ]

wine.test <- wine[ sample( c( 1:178 ), 54 ), 1:14 ]

wine.pred <- knn( train = wine.train[ , 2:14 ], test = wine.test[ , 2:14 ], cl = wine.train$X1, k = 2 )
con_wine_1 = confusionMatrix(wine.pred,factor(wine.test$X1))
accuracy_wine_1 = con_wine_1$overall[1]
precision_wine_1 = mean(con_wine_1$byClass[,5])
recall_wine_1 = mean(con_wine_1$byClass[,6])
f1_wine_1 = mean(con_wine_1$byClass[,7])

ind <- sample(2,nrow(wine),replace=TRUE,prob=c(0.7,0.3))
trainData <- wine[ind==1,]
testData <- wine[ind==2,]
wine_rf <- randomForest(factor(X1)~.,data=trainData,ntree=100,proximity=TRUE)
winePred<-predict(wine_rf,newdata=testData)
table(winePred, testData$X1)
con_wine_2 = confusionMatrix(winePred,factor(testData$X1))
con_wine_2
accuracy_wine_2 = con_wine_2$overall[1]
precision_wine_2 = mean(con_wine_2$byClass[,5])
recall_wine_2 = mean(con_wine_2$byClass[,6])
f1_wine_2 = mean(con_wine_2$byClass[,7])

modelo<-lda(X1~.,data=wine)
prediccion<-predict(modelo,wine[-1])
m_confusion<-table(wine$X1,prediccion$class,dnn=c("Real","Predicho"))
con_wine_4 = confusionMatrix(factor(wine$X1),prediccion$class,dnn=c("Real","Predicho"))
con_wine_4
accuracy_wine_4 = con_wine_4$overall[1]
precision_wine_4 = mean(con_wine_4$byClass[,5])
recall_wine_4 = mean(con_wine_4$byClass[,6])
f1_wine_4 = mean(con_wine_4$byClass[,7])

dataset = wine

### Random Cross Validation
set.seed(2)
split = sample.split(dataset$X1, SplitRatio = .7)
summary(split)

### Training
training_set = subset(dataset, split == TRUE)

### Test
test_set = subset(dataset, split == FALSE)


### Entrenar el Clasificador
# Funcion kernel determina la funcion y va aprendiendo la curva
classifier1 = svm(formula = X1~., data = training_set, 
                  type = 'C-classification', kernel = 'linear')

### Predicciones del clasificador
test_pred1 = predict(classifier1, type = 'response', 
                     newdata = test_set[-1])

### Matriz Confusion
cm1 = table(test_set$X1, test_pred1)
cm1
#plot(classifier1, wine, X1 ~ .,slice = list(Sepal.Width = 1, Sepal.Length = 2))

con_wine_6 = confusionMatrix(test_pred1,factor(test_set$X1))
con_wine_6
accuracy_wine_6 = con_wine_6$overall[1]
precision_wine_6 = mean(con_wine_6$byClass[,5])
recall_wine_6 = mean(con_wine_6$byClass[,6])
f1_wine_6 = mean(con_wine_6$byClass[,7])
knnAcurracy = c(accuracy_wine_1) 
forestAcurracy = c(accuracy_wine_2)
#bayesAccuracy = c(accuracy_wine_3)
regAccuracy = c(accuracy_wine_4)
#redesAccuracy = c(accuracy_wine_5)
svmAccuracy = c(accuracy_wine_6)
accuracyTable2 = c(knnAcurracy,forestAcurracy,regAccuracy,svmAccuracy)
presisionTable2 =c(knnPressision=precision_wine_1,forestPressision=precision_wine_2,regPressision=precision_wine_4,svmPressision=precision6)
recallTable2 = c(recallKnn=recall_wine_1,recallForest=recall_wine_2,recaallRegresion=recall_wine_4,recallSvm=recall_wine_6)
measureTable2=c(Knn=f1_wine_1,RandomFores=f1_wine_2,RegresionLog=f1_wine_4,SVM=f1_wine_6)
shinyServer(function(input, output) {
  output$text1var <- renderText({
    colm = as.numeric(input$var)
    paste("Selected Dataset column is: ", names(iris[colm]))
  })
  output$text2bin <- renderText({
    paste("Number of histogram BINs is: ", input$bin)
  })
  
  output$text3colour <- renderText({
    paste("Colour of histogram is: ", input$radio)
  })
  
  output$distPlot <- renderPlot({
    colm = as.numeric(input$var)
    hist(
      iris[, colm],
      freq = FALSE,
      col = input$radio,
      xlab = names(iris[colm]),
      main = "Colored histogram",
      breaks = seq(0, max(iris[, colm]), l = input$bin + 1)
    )
    
    # add a normal distribution line in histogram
    curve(dnorm(x,
                mean = mean(iris[, colm]),
                sd = sd(iris[, colm])),
          add = TRUE,
          col = "red") #line
  })
  output$accuracy <- renderPlot({
    barplot(accuracyTable,xlab = "Accuracy",ylab = "Puntaje", main = "Accuracy Iris",col=brewer.pal(6, "Set2") ,ylim =c(0,1))
  })
  output$precision <- renderPlot({
    barplot(presisionTable,xlab = "Pressision",ylab = "Puntaje", main = "Precision Iris",col=color_d,ylim =c(0,1))
    
  })
  output$recall <- renderPlot({
    barplot(recallTable,xlab = "Recall",ylab = "Puntaje", main = "Recall Iris",col=brewer.pal(6, "Set1"),ylim =c(0,1))
    
    
  })
  output$measure <- renderPlot({
    barplot(measureTable,xlab = "F1-Measure",ylab = "Puntaje", main = "F1-Measure Iris",col="orange",ylim =c(0,1))
    
  })
  output$accuracy2 <- renderPlot({
    barplot(accuracyTable2,xlab = "Accuracy",ylab = "Puntaje", main = "Accuracy Wine",col=brewer.pal(6, "Set3"),ylim =c(0,1))
    
  })
  output$precision2 <- renderPlot({
    barplot(presisionTable2,xlab = "Pressision",ylab = "Puntaje", main = "Pressision Wine",col=brewer.pal(6, "Set2"),ylim =c(0,1))
    
  })
  output$recall2 <- renderPlot({
    barplot(recallTable2,xlab = "Recall",ylab = "Puntaje", main = "Recall Wine",col=brewer.pal(6, "Set1"),ylim =c(0,1))
  })
  output$measure2 <- renderPlot({
    barplot(measureTable2,xlab = "F1",ylab = "Puntaje", main = "F1 Wine",col=brewer.pal(6, "Set2"),ylim =c(0,1))
  })
  
  
})