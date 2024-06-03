#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(e1071)

ui <- fluidPage(
  
  titlePanel("Predicción de recidiva"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("edad", "Selecciona Edad:", 
                  min = 0, max = 100, value = 50),
      
      selectInput("fenotipo", "Seleccione el Fenotipo:", 
                  choices = c("LumbA", "LumbB", "Basal", "Her2", "Normal")),
      
      selectInput("grado", "Seleccione el Grado:", 
                  choices = c("I", "II", "III")),
      
      selectInput("rest", "Seleccione Receptores de Estrógeno:", 
                  choices = c("P", "N")),
      
      # Botón para hacer la predicción
      actionButton("predict", "Predecir")
    ),
    
    mainPanel(
      # Mostrar el resultado de la predicción
      textOutput("result")
    )
  )
)


server <- function(input, output) {
  # Cargar los datos del CSV al iniciar la aplicación
  datos <- reactive({
    read.table(file = "datos_p1.csv", sep = ";", header = T, dec=",")
  })
  
  # Entrenar el modelo Naive Bayes
  modelo_nb <- reactive({
    df <- datos()
    
    # Eliminación de la variable muestra
    df$Muestra <- NULL
    
    # Eliminación de las filas que contienen un valor nulo en la variable de estudio
    df <- df[!is.na(df$PCR), ]
    
    # Conversión a tipo factor de todas las variables menos edad
    variables_a_cambiar <- c("REst", "RPro", "Her2", "Estadio", "NodAfec", "Grado", "Fenotipo", "PCR")
    df[variables_a_cambiar] <- lapply(df[variables_a_cambiar], factor)
    
    # Redondeo de edad a valores enteros
    df$Edad <- round(df$Edad, digits = 0)
    
    # Recodificación de algunas categorías
    df$Estadio <- dplyr::recode(df$Estadio, 'T0' = "T0_T1")
    df$Estadio <- dplyr::recode(df$Estadio, 'T1' = "T0_T1")
    df$REst <- dplyr::recode(df$REst, 'I' = NA_character_)
    df$RPro <- dplyr::recode(df$RPro, 'I' = NA_character_)
    df$Her2 <- dplyr::recode(df$Her2, 'I' = NA_character_)
    df$Grado <- dplyr::recode(df$Grado, '1' = "I", '2' = "II", '3' = "III")
    
    # Imputación de valores perdidos con la moda
    df <- imputar.con.moda(df, "REst")
    df <- imputar.con.moda(df, "RPro")
    df <- imputar.con.moda(df, "Her2")
    df <- imputar.con.moda(df, "Grado")
    
    rownames(df) <- NULL
    
    df <- df[, c("REst", "Edad", "Fenotipo", "Grado", "PCR")]
    # Suponiendo que la columna objetivo se llama "Recidiva"
    naiveBayes(PCR ~ ., data = df)
  })
  
  # Hacer la predicción al hacer clic en el botón
  observeEvent(input$predict, {
    # Crear un nuevo dataframe con las entradas del usuario
    nuevo_dato <- data.frame(
      Edad = input$edad,
      Fenotipo = input$fenotipo,
      Grado = input$grado,
      Receptores_Estrogeno = input$rest
    )
    
    # Asegurar que las variables categóricas sean factores
    nuevo_dato$Fenotipo <- factor(nuevo_dato$Fenotipo, levels = c("LumbA", "LumbB", "Basal", "Her2", "Normal"))
    nuevo_dato$Grado <- factor(nuevo_dato$Grado, levels = c("I", "II", "III"))
    nuevo_dato$Receptores_Estrogeno <- factor(nuevo_dato$Receptores_Estrogeno, levels = c("P", "N"))
    
    # Hacer la predicción
    prob_clase_positiva <- predict(modelo_nb(), nuevo_dato, type = "raw")[,2]
    
    # Mostrar el resultado
    output$result <- renderText({
      paste("La probabilidad de recidiva es del:", round(prob_clase_positiva * 100, 2), "%")
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
