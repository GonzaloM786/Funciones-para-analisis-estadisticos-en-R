# Devuelve el mejor modelo y la curva ROC usando fuerza bruta para la selección de variables
  # df: dataframe
  # variable_estudio: string que indica la variable de estudio del dataframe
  # tam_entren: proporcion del dataset que se usará tanto para entrenar como validar el modelo
  # umbral: umbral usado para clasificar las probabilidades a la clase negativa o positiva
  # show_metrics: muestra la matriz de confusión y otras métricas (accuracy, precision, especificidad, fallout, recall y f-measure)
holdout <- function(df, variable_estudio, tam_entren = 0.7, umbral = 0.5, show_metrics = TRUE){
  nr <- nrow(df)
  
  # Vector mezclado de índices del tamaño del dataframe
  indices <- sample(1:nr, size = nr, replace = FALSE)
  
  tam_entrenamiento <- floor(tam_entren * nr)
  
  # División los índices en conjuntos de entrenamiento y prueba
  indices_entrenamiento <- indices[1:tam_entrenamiento]
  indices_prueba <- indices[(tam_entrenamiento + 1):(nr)]
  
  # Conjuntos de entrenamiento y prueba utilizando los índices
  datos_entrenamiento <- df[indices_entrenamiento, ]
  datos_prueba <- df[indices_prueba, ]
  
  # Mejor modelo
  mejor_modelo <- list(modelo = NULL, accuracy = -Inf)
  
  # Variables posibles
  variables <- colnames(df)
  variables <-variables[variables != variable_estudio]
  for (j in 1:length(variables)){
    # Combinaciones de variables
    combinaciones <- combn(variables, j)
    for (i in 1:ncol(combinaciones)){
      # Entrenar el modelo la combinacion de variables de la iteracion
      formula <- paste(variable_estudio, "==1 ~", paste(combinaciones[, i], collapse = " + "))
      modelo <- glm(formula, data = datos_entrenamiento, family = binomial("logit"))
      
      # Calcular predicciones sobre los mismos datos usados para el entrenamiento
      predicciones <- predict(modelo, datos_entrenamiento, type = "response")
      clases_predichas <- ifelse(predicciones >= umbral, 1, 0)
      
      VP <- sum(clases_predichas == 1 & datos_entrenamiento[[variable_estudio]] == 1)
      VN <- sum(clases_predichas == 0 & datos_entrenamiento[[variable_estudio]] == 0)
      FP <- sum(clases_predichas == 1 & datos_entrenamiento[[variable_estudio]] == 0)
      FN <- sum(clases_predichas == 0 & datos_entrenamiento[[variable_estudio]] == 1)
      
      # Calcular el accuracy aparente
      accuracy <- (VP+VN)/(VP+VN+FP+FN)
      
      if (accuracy > mejor_modelo$accuracy) {
        mejor_modelo$modelo <- modelo
        mejor_modelo$accuracy <- accuracy
      }
    }
  }
  
  # Obtener las métricas sobre el conjunto de test (datos que no ha visto el modelo)
  predicciones <- predict(mejor_modelo$modelo, datos_prueba, type = "response")
  
  # Mostrar la matriz de confusión y otras métricas (accuracy, precision, especificidad, fallout, recall y f-measure)
  if(show_metrics){
    clases_predichas <- ifelse(predicciones >= umbral, 1, 0)
    
    mc <- matriz.confusion(clases_predichas, datos_prueba[[variable_estudio]])
    calcular.metricas(mc) 
    print(mc)
  } 
  
  # ROC
  roc <- curva_roc(datos_prueba[[variable_estudio]], predicciones)
  
  return(list(modelo = mejor_modelo$modelo, roc = roc))
}

#-------------------------------------------------------------------------------

# Devuelve el mejor modelo y la curva ROC usando fuerza bruta para la selección de variables
  # df: dataframe
  # variable_estudio: string que indica la variable de estudio del dataframe
  # n_iter: número de iteraciones que se repite el hold-out estratificado
  # tam_entren: proporcion del dataset que se usará tanto para entrenar como validar el modelo
  # umbral: umbral usado para clasificar las probabilidades a la clase negativa o positiva
  # show_metrics: muestra la matriz de confusión y otras métricas (accuracy, precision, especificidad, fallout, recall y f-measure)
repHOut <- function(df, variable_estudio, n_iter = 30, umbral = 0.5, tam_entren = 0.7, show_metrics = TRUE) {
  # Mejor modelo
  mejor_modelo <- list(modelo = NULL, accuracy = -Inf)
  
  # Variables posibles
  variables <- colnames(df)
  variables <-variables[variables != variable_estudio]
  
  for(i in 1:n_iter){
    # 'indices' contiene los índices de las filas que pertenecerán al conjunto de entrenamiento
    indices <- createDataPartition(df[[variable_estudio]], p = tam_entren, list = FALSE)
    
    datos_entrenamiento <- df[indices, ]
    datos_prueba <- df[-indices, ]
    
    # Mejor modelo de cada iteracion
    mejor_modelo_iter <- list(modelo = NULL, accuracy = -Inf)
    
    for (j in 1:length(variables)){
      # Combinaciones de variables
      combinaciones <- combn(variables, j)
      for (i in 1:ncol(combinaciones)){
        # Entrenar el modelo la combinacion de variables de la iteracion
        formula <- paste(variable_estudio, "==1 ~", paste(combinaciones[, i], collapse = " + "))
        modelo <- glm(formula, data = datos_entrenamiento, family = binomial("logit"))
        
        # Calcular predicciones sobre los mismos datos usados para el entrenamiento
        predicciones <- predict(modelo, datos_entrenamiento, type = "response")
        clases_predichas <- ifelse(predicciones >= umbral, 1, 0)
        
        VP <- sum(clases_predichas == 1 & datos_entrenamiento[[variable_estudio]] == 1)
        VN <- sum(clases_predichas == 0 & datos_entrenamiento[[variable_estudio]] == 0)
        FP <- sum(clases_predichas == 1 & datos_entrenamiento[[variable_estudio]] == 0)
        FN <- sum(clases_predichas == 0 & datos_entrenamiento[[variable_estudio]] == 1)
        
        # Calcular el accuracy aparente
        accuracy <- (VP+VN)/(VP+VN+FP+FN)
        
        if (accuracy > mejor_modelo_iter$accuracy) {
          mejor_modelo_iter$modelo <- modelo
          mejor_modelo_iter$accuracy <- accuracy
        }
      }
    }
    
    if (mejor_modelo_iter$accuracy > mejor_modelo$accuracy) {
      mejor_modelo$modelo <- mejor_modelo_iter$modelo
      mejor_modelo$accuracy <- mejor_modelo_iter$accuracy
    }
  }
  
  # Obtener las métricas sobre el conjunto de test (datos que no ha visto el modelo)
  predicciones <- predict(mejor_modelo$modelo, datos_prueba, type = "response")
  
  # Mostrar la matriz de confusión y otras métricas (accuracy, precision, especificidad, fallout, recall y f-measure)
  if(show_metrics){
    clases_predichas <- ifelse(predicciones >= umbral, 1, 0)
    
    mc <- matriz.confusion(clases_predichas, datos_prueba[[variable_estudio]])
    calcular.metricas(mc) 
    print(mc)
  } 
  
  # ROC
  roc <- curva_roc(datos_prueba[[variable_estudio]], predicciones)
  
  return(list(modelo = mejor_modelo$modelo, roc = roc))
}

#-------------------------------------------------------------------------------

# Devuelve el mejor modelo y la curva ROC usando fuerza bruta para la selección de variables
  # df: dataframe
  # variable_estudio: string que indica la variable de estudio del dataframe
  # k: número de folds
  # tam_entren: proporcion del dataset que se usará tanto para entrenar como validar el modelo
  # umbral: umbral usado para clasificar las probabilidades a la clase negativa o positiva
  # show_metrics: muestra la matriz de confusión y otras métricas (accuracy, precision, especificidad, fallout, recall y f-measure)
crossVal <- function(df, variable_estudio, k = 10, tam_entren = 0.7, umbral = 0.5, show_metrics = TRUE) {
  indices <- createDataPartition(df[[variable_estudio]], p = tam_entren, list = FALSE)
  
  # 'indices' contiene los índices de las filas que pertenecerán al conjunto de entrenamiento
  set_entrenamiento <- df[indices, ]
  set_prueba <- df[-indices, ]
  
  set.seed(123)
  folds <- createFolds(set_entrenamiento[[variable_estudio]], k = 10)
  
  # Variables posibles
  variables <- colnames(df)
  variables <-variables[variables != variable_estudio]
  
  mejor_modelo <- list(modelo = NULL, accuracy = -Inf)
  
  for(fold in folds){
    datos_entrenamiento <- set_entrenamiento[-fold, ]
    datos_validacion <- set_entrenamiento[fold, ]
    
    mejor_modelo_iter <- list(modelo = NULL, accuracy = -Inf)
    
    for (j in 1:length(variables)){
      # Combinaciones de variables
      combinaciones <- combn(variables, j)
      for (i in 1:ncol(combinaciones)){
        # Entrenar el modelo la combinacion de variables de la iteracion
        formula <- paste(variable_estudio, "==1 ~", paste(combinaciones[, i], collapse = " + "))
        modelo <- glm(formula, data = datos_entrenamiento, family = binomial("logit"))
        
        # Calcular predicciones
        predicciones <- predict(modelo, datos_validacion, type = "response")
        clases_predichas <- ifelse(predicciones >= umbral, 1, 0)
        
        VP <- sum(clases_predichas == 1 & datos_entrenamiento[[variable_estudio]] == 1)
        VN <- sum(clases_predichas == 0 & datos_entrenamiento[[variable_estudio]] == 0)
        FP <- sum(clases_predichas == 1 & datos_entrenamiento[[variable_estudio]] == 0)
        FN <- sum(clases_predichas == 0 & datos_entrenamiento[[variable_estudio]] == 1)
        
        # Calcular el accuracy aparente
        accuracy <- (VP+VN)/(VP+VN+FP+FN)
        
        if (accuracy > mejor_modelo_iter$accuracy) {
          mejor_modelo_iter$modelo <- modelo
          mejor_modelo_iter$accuracy <- accuracy
        }
      }
    }
    
    if (mejor_modelo_iter$accuracy > mejor_modelo$accuracy) {
      mejor_modelo$modelo <- mejor_modelo_iter$modelo
      mejor_modelo$accuracy <- mejor_modelo_iter$accuracy
    }
  }
  
  # Obtener las métricas sobre el conjunto de prueba (datos que no ha visto el modelo)
  predicciones <- predict(mejor_modelo$modelo, set_prueba, type = "response")
  
  # Mostrar la matriz de confusión y otras métricas (accuracy, precision, especificidad, fallout, recall y f-measure)
  if(show_metrics){
    clases_predichas <- ifelse(predicciones >= umbral, 1, 0)
    
    mc <- matriz.confusion(clases_predichas, set_prueba[[variable_estudio]])
    calcular.metricas(mc) 
    print(mc)
  } 
  
  # ROC
  roc <- curva_roc(set_prueba[[variable_estudio]], predicciones)
  
  return(list(modelo = mejor_modelo$modelo, roc = roc))
  
}


#-------------------------------------------------------------------------------

# Función para generar la curva ROC y calcular el AUC
curva_roc <- function(clase_real, probabilidad, umbral_seq = seq(0, 1, 0.01)) {
  # Calcular TPR y FPR para diferentes umbrales de clasificación
  roc_data <- data.frame()
  for (umbral in umbral_seq) {
    clase_predicha <- ifelse(probabilidad >= umbral, 1, 0)
    VP <- sum(clase_predicha == 1 & clase_real == 1)
    FP <- sum(clase_predicha == 1 & clase_real == 0)
    VN <- sum(clase_predicha == 0 & clase_real == 0)
    FN <- sum(clase_predicha == 0 & clase_real == 1)
    TPR <- VP / (VP + FN)
    FPR <- FP / (FP + VN)
    roc_data <- rbind(roc_data, data.frame(Threshold = umbral, TPR = TPR, FPR = FPR))
  }
  
  # Calcular el AUC usando la regla del trapecio
  auc <- 0
  for(i in 2:nrow(roc_data)){
    auc <- auc + abs((roc_data$FPR[i] - roc_data$FPR[i-1])*(roc_data$TPR[i] + roc_data$TPR[i-1])/2)
  }
  
  # Graficar la curva ROC
  roc_plot <- ggplot(roc_data, aes(x = FPR, y = TPR)) +
    geom_line() +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
    labs(x = "Tasa de Falsos Positivos (FPR)", y = "Tasa de Verdaderos Positivos (TPR)", 
         title = "Curva ROC") +
    theme_minimal()
  
  # Devolver la gráfica de la curva ROC, el AUC y la tabla con los valores usados para pintar la curva
  return(list(grafica = roc_plot, auc = auc, data = roc_data))
}

#-------------------------------------------------------------------------------

# Función para graficar varias curvas ROC en una misma gráfica a partir de una lista de curvas ROC
curvas_roc <- function(lista){
  # Curva ROC base
  roc_plot <- ggplot() +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
    labs(x = "Tasa de Falsos Positivos (FPR)", y = "Tasa de Verdaderos Positivos (TPR)", 
         title = "Curva ROC") +
    theme_minimal()
  
  for (roc in lista) {
    roc_data <- roc$data
    
    # Color aleatorio para cada curva
    color <- rgb(runif(1) * 255, runif(1) * 255, runif(1) * 255, maxColorValue = 255)
    roc_plot <- roc_plot + geom_line(data = roc_data, aes(x = FPR, y = TPR), color = color)
  }
  print(roc_plot)
}

#-------------------------------------------------------------------------------

# Genera la matriz de confusión
matriz.confusion <- function(Clases_Predichas, Valores_Reales){
  df_confusion <- data.frame(Positivo = numeric(2), Negativo = numeric(2))
  rownames(df_confusion) <- c("Positivo_Predicho", "Negativo_Predicho")
  colnames(df_confusion) <- c("Positivo_Real", "Negativo_Real")
  
  contador = 1
  while(contador <= length(Valores_Reales)){
    if(Clases_Predichas[contador] == 1 & Valores_Reales[contador] == 1){
      df_confusion["Positivo_Predicho", "Positivo_Real"] = df_confusion["Positivo_Predicho", "Positivo_Real"] + 1
    }else if(Clases_Predichas[contador] == 0 & Valores_Reales[contador] == 0){
      df_confusion["Negativo_Predicho", "Negativo_Real"] = df_confusion["Negativo_Predicho", "Negativo_Real"] + 1
    }else if(Clases_Predichas[contador] == 1 & Valores_Reales[contador] == 0){
      df_confusion["Positivo_Predicho", "Negativo_Real"] = df_confusion["Positivo_Predicho", "Negativo_Real"] + 1
    }else{
      df_confusion["Negativo_Predicho", "Positivo_Real"] = df_confusion["Negativo_Predicho", "Positivo_Real"] + 1
    }
    
    contador <-  contador + 1
  }
  return(df_confusion)
}

#-------------------------------------------------------------------------------

# Muestra las métricas a partir de la matriz de confusión
calcular.metricas <- function(matriz.confusion){
  VP <- matriz.confusion["Positivo_Predicho", "Positivo_Real"]
  VN <- matriz.confusion["Negativo_Predicho", "Negativo_Real"]
  FP <- matriz.confusion["Positivo_Predicho", "Negativo_Real"]
  FN <- matriz.confusion["Negativo_Predicho", "Positivo_Real"]
  accuracy <- (VP+VN)/(VP+VN+FP+FN)
  precision <- (VP)/(VP+FP)
  especificidad <- (VN)/(VN+FP)
  fallout <- (FP)/(FP+VN)
  recall <- (VP)/(VP+FN)
  f_measure <- 2*(precision*recall)/(precision+recall)
  print(paste("Accuracy:", accuracy))
  print(paste("Precisión:", precision))
  print(paste("Especificidad:", especificidad))
  print(paste("Fallout:", fallout))
  print(paste("Recall:", recall))
  print(paste("F-measure:", f_measure))
}





