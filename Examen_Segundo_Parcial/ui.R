# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(markdown)

shinyUI(navbarPage(
  "ShinyApp - Examen Segundo Parcial",
  tabPanel("Iris",
           # Sidebar with a slider input for number of bins
           sidebarLayout(
             sidebarPanel(
              
               h2("Integrantes:"),
               h4("Alfredo Catucuago"),
               h4("Stalin Chiguano"),
               h4("Daniel Dominguez"),
               h4("Jefferson Estacio"),
               h2("DataSet Iris"),
               h5("El conjunto de datos de Iris consta de 150 instancias en
total, contiene 50 muestras de cada una de tres especies de Iris
(Iris setosa, Iris virginica e Iris versicolor). Se midio cuatro ´
rasgos de cada muestra: el largo y ancho del sepalo y p ´ etalo, ´
en cent´ımetros. Basado en la combinacion de estos cuatro ´
rasgos, Fisher desarrollo un modelo discriminante lineal para ´
distinguir entre una especie y otra."),
               h2("Desarollo"),
               h5("Para la implementacion de los algoritmos, ytomamos en cuenta diferentes medidas para evaluar
                  el rendimiento de cada uno de ellos, tomando en cuenta:KNN, Random Forest, Naive Bayes, Regresión Logística, Red Neuronal y Máquina de Soporte Vectorial, desarollamos cada uno de los modelos y se muestran los resultados comparados entre si.")
             ),
             
             
             # Main Panel - Show a plot of the generated distribution
             mainPanel(
               # textOutput("text0species"),
               plotOutput("accuracy"),
               plotOutput("precision"),
               plotOutput("recall"),
               plotOutput("measure"),
             )
           )),
  tabPanel("Wine",
           sidebarLayout(
             sidebarPanel(
               
               h2("Integrantes:"),
               h4("Alfredo Catucuago"),
               h4("Stalin Chiguano"),
               h4("Daniel Dominguez"),
               h4("Jefferson Estacio"),
               h2("DataSet Iris"),
               h5("El uso del analisis químico determina el origen de los
vinos. Estos datos son el resultado de un analisis químico
de los vinos cultivados en la misma region en Italia pero
derivados de tres cultivares diferentes. El analisis determinón
las cantidades de 13 componentes encontrados en cada uno
de los tres tipos de vinos. En un contexto de clasificacion,
este es un problema bien planteado con estructuras de clase
bien comportadas. Un buen conjunto de datos para la primera
prueba de un nuevo clasificador, pero no muy difícil."),
               h2("Desarollo"),
               h5("Para la implementacion de los algoritmos, ytomamos en cuenta diferentes medidas para evaluar
                  el rendimiento de cada uno de ellos, tomando en cuenta:KNN, Random Forest, Naive Bayes, Regresión Logística, Red Neuronal y Máquina de Soporte Vectorial, desarollamos cada uno de los modelos y se muestran los resultados comparados entre si.")
             ),
             
             
             # Main Panel - Show a plot of the generated distribution
             mainPanel(
               # textOutput("text0species"),
               plotOutput("accuracy2"),
               plotOutput("precision2"),
               plotOutput("recall2"),
               plotOutput("measure2"),
             )
           )),
  tabPanel("About",
           mainPanel(includeMarkdown("about.md")))
))
