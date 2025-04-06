# Installer les packages nécessaires si ce n'est pas encore fait
if (!require("shiny")) install.packages("shiny", dependencies = TRUE)
if (!require("randomForest")) install.packages("randomForest")
if (!require("caret")) install.packages("caret")
if (!require("palmerpenguins")) install.packages("palmerpenguins")

# Charger les bibliothèques
library(shiny)
library(randomForest)
library(caret)
library(palmerpenguins)

# Charger et nettoyer les données penguins
data(penguins)
penguins_clean <- na.omit(penguins[, c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g", "species")])

# Diviser les données en train/test
set.seed(123)
train_index <- createDataPartition(penguins_clean$species, p = 0.8, list = FALSE)
train <- penguins_clean[train_index, ]
test <- penguins_clean[-train_index, ]

# Entraîner un modèle Random Forest
set.seed(123)
rf_model <- randomForest(species ~ ., data = train, ntree = 100, mtry = 2)

# Définir l'interface utilisateur (HTML)
ui <- fluidPage(
  titlePanel("Prédiction de l'espèce d'un manchot 🐧"),
  sidebarLayout(
    sidebarPanel(
      numericInput("bill_length", "Longueur du bec (mm) :", value = 45, min = 30, max = 60),
      numericInput("bill_depth", "Profondeur du bec (mm) :", value = 17, min = 13, max = 21),
      numericInput("flipper_length", "Longueur des nageoires (mm) :", value = 200, min = 170, max = 230),
      numericInput("body_mass", "Masse corporelle (g) :", value = 4000, min = 2700, max = 6300),
      actionButton("predict", "Prédire l'espèce"),
      br(),
      br(),
      textOutput("prediction_result")
    ),
    mainPanel(
      h3("Résultat de la Prédiction :"),
      verbatimTextOutput("prediction_output")
    )
  )
)

# Définir la logique de l'application (Serveur)
server <- function(input, output) {
  observeEvent(input$predict, {
    new_observation <- data.frame(
      bill_length_mm = input$bill_length,
      bill_depth_mm = input$bill_depth,
      flipper_length_mm = input$flipper_length,
      body_mass_g = input$body_mass
    )
    
    prediction <- predict(rf_model, newdata = new_observation, type = "response")
    
    output$prediction_result <- renderText({
      paste("Espèce prédite :", prediction)
    })
    
    output$prediction_output <- renderPrint({
      predict(rf_model, newdata = new_observation, type = "prob")
    })
  })
}

# Lancer l'application
shinyApp(ui = ui, server = server)

# executer le code, il faut ecrire cette commande en ligne de commande
shiny::runApp("app..R")
