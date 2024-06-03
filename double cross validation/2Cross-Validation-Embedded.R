# lista_param: list(kernel = c("radial", "polinomical"), C = c(0.1, 1, 10, 100), gamma = c(0.1, 1, 10, 100))

cv5x2_svm_embedded <- function(df_original, variable_estudio, lista_param) {
  semillas <- c(3, 7, 13, 25, 39)
  df <- dummy_escalado(df_original)
  # Creación del meshgrid (combinación de los valores de los parámetros)
  parametros <- expand.grid(lista_param)
  
  FOLD <- c()
  AUC <- c()
  KERNEL <- c()
  GAMMA <- c()
  C <- c()
  VARIABLES <- data.frame(Variables = character(), stringsAsFactors = FALSE)
  
  for (semilla in semillas) {
    set.seed(semilla)
    
    # Crear las cajas de test y entrenamiento para el outer-loop
    folds_outer <- createFolds(df[[variable_estudio]], k = 5)
    
    # Realizar la validación cruzada 5x2
    # Outer-loop
    for (i in 1:5) {
      # Crear las cajas de validación y entrenamiento para el inner-loop
      folds_inner <- folds_outer[-i]
      datos_test <- df[unlist(folds_outer[i]),]
      
      # Inicializar el vector de auc para el inner loop
      auc_inner <- c()
      variables_inner <- c()
      
      # Separar los datos en conjuntos de entrenamiento y validación
      datos_inner <- df[unlist(folds_inner), ]
      mitad <- floor(nrow(datos_inner) / 2)
      datos_train <- datos_inner[1:mitad, ]
      datos_val <- datos_inner[(mitad + 1):nrow(datos_inner), ]
      
      # Inner-loop
      for (j in 1:2) {
        if (j == 2) {
          aux <- datos_train
          datos_train <- datos_val
          datos_val <- aux
        }
        
        variables_seleccionadas <- modelo_lasso(datos_train, variable_estudio)
        
        if (length(variables_seleccionadas) == 0) {
          variables_seleccionadas <- c("Fenotipo")
        }
        
        formula <- paste(variable_estudio, " ~ ", paste(variables_seleccionadas, collapse = " + "))
        
        # Ajustar el modelo SVM con las variables seleccionadas
        for (k in 1:nrow(parametros)) {
          comb <- parametros[k, ]
          
          modelo_svm <- svm(as.formula(formula), data = datos_train, kernel = as.character(comb$kernel), gamma = comb$gamma, cost = comb$C, probability = TRUE)
          
          probabilidades_SVM <- predict(modelo_svm, datos_val, probability = TRUE)
          svm.pred <- attr(probabilidades_SVM, which = "probabilities")[, 2]
          
          auc_modelo <- auc(roc(datos_val[[variable_estudio]], svm.pred))
          auc_inner <- c(auc_inner, auc_modelo)
          variables_inner <- c(variables_inner, paste(variables_seleccionadas, collapse = ", "))
        }
      }
      
      auc_mediado <- c()
      mitad_long <- length(auc_inner) / 2
      for (x in 1:mitad_long) {
        auc_mediado <- c(auc_mediado, (auc_inner[x] + auc_inner[mitad_long + x]) / 2)
      }
      
      posicion_max <- which.max(auc_mediado)
      
      mejores_variables_inner <- unlist(strsplit(variables_inner[posicion_max], ", "))
      formula <- paste(variable_estudio, " ~ ", paste(mejores_variables_inner, collapse = " + "))
      mejor_modelo_inner <- svm(as.formula(formula), data = datos_inner, kernel = as.character(parametros[posicion_max, ]$kernel), gamma = parametros[posicion_max, ]$gamma, cost = parametros[posicion_max, ]$C, probability = TRUE)
      
      probabilidades_SVM_inner <- predict(mejor_modelo_inner, datos_test, probability = TRUE)
      svm.pred_inner <- attr(probabilidades_SVM_inner, which = "probabilities")[, 2]
      
      auc_outer <- auc(roc(datos_test[[variable_estudio]], svm.pred_inner))
      
      FOLD <- c(FOLD, i)
      AUC <- c(AUC, auc_outer)
      KERNEL <- c(KERNEL, as.character(parametros[posicion_max, ]$kernel))
      GAMMA <- c(GAMMA, parametros[posicion_max, ]$gamma)
      C <- c(C, parametros[posicion_max, ]$C)
      VARIABLES <- rbind(VARIABLES, data.frame(Variables = variables_inner[posicion_max], stringsAsFactors = FALSE))
    }
  }
  
  metricas <- data.frame(Semilla = rep(semillas, each = 5), Fold = FOLD, Auc = AUC, Kernel = as.factor(KERNEL), Gamma = GAMMA, Cost = C)
  metricas <- cbind(metricas, VARIABLES)
  return(metricas)
}

#-----------------------------------------------------------------------------------------------------------

cv5x2_ann_embedded <- function(df_original, variable_estudio, lista_param) {
  semillas <- c(3, 7, 13, 25, 39)
  df <- dummy_escalado(df_original)
  
  # Creación del meshgrid (combinación de los valores de los parámetros)
  parametros <- expand.grid(lista_param)
  
  FOLD <- c()
  AUC <- c()
  SIZE <- c()
  DECAY <- c()
  VARIABLES <- data.frame(Variables = character(), stringsAsFactors = FALSE)
  
  for (semilla in semillas) {
    set.seed(semilla)
    
    # Crear las cajas de test y entrenamiento para el outer-loop
    folds_outer <- createFolds(df[[variable_estudio]], k = 5)
    
    # Realizar la validación cruzada 5x2
    # Outer-loop
    for (i in 1:5) {
      # Crear las cajas de validación y entrenamiento para el inner-loop
      folds_inner <- folds_outer[-i]
      datos_test <- df[unlist(folds_outer[i]),]
      
      # Inicializar el vector de auc para el inner loop
      auc_inner <- c()
      variables_inner <- c()
      
      # Separar los datos en conjuntos de entrenamiento y validación
      datos_inner <- df[unlist(folds_inner), ]
      mitad <- floor(nrow(datos_inner) / 2)
      datos_train <- datos_inner[1:mitad, ]
      datos_val <- datos_inner[(mitad + 1):nrow(datos_inner), ]
      
      # Inner-loop
      for (j in 1:2) {
        if (j == 2) {
          aux <- datos_train
          datos_train <- datos_val
          datos_val <- aux
        }
        
        variables_seleccionadas <- modelo_lasso(datos_train, variable_estudio)
        
        if (length(variables_seleccionadas) == 0) {
          variables_seleccionadas <- names(datos_train)[which(names(datos_train) != variable_estudio)][2]
        }
        
        formula <- paste(variable_estudio, " ~ ", paste(variables_seleccionadas, collapse = " + "))
        
        # Ajustar el modelo ANN con las variables seleccionadas
        for (k in 1:nrow(parametros)) {
          comb <- parametros[k, ]
          
          modelo_ann <- nnet(as.formula(formula), data = datos_train, size = comb$size, maxit = 300, decay = comb$decay)
          
          probabilidades_ANN <- predict(modelo_ann, datos_val, type = "raw")
          
          auc_modelo <- auc(roc(datos_val[[variable_estudio]], probabilidades_ANN))
          auc_inner <- c(auc_inner, auc_modelo)
          variables_inner <- c(variables_inner, paste(variables_seleccionadas, collapse = ", "))
        }
      }
      
      auc_mediado <- c()
      mitad_long <- length(auc_inner) / 2
      for (x in 1:mitad_long) {
        auc_mediado <- c(auc_mediado, (auc_inner[x] + auc_inner[mitad_long + x]) / 2)
      }
      
      posicion_max <- which.max(auc_mediado)
      
      mejores_variables_inner <- unlist(strsplit(variables_inner[posicion_max], ", "))
      formula <- paste(variable_estudio, " ~ ", paste(mejores_variables_inner, collapse = " + "))
      mejor_modelo_inner <- nnet(as.formula(formula), data = datos_inner, size = parametros[posicion_max, ]$size, maxit = 300, decay = parametros[posicion_max, ]$decay)
      
      probabilidades_ANN <- predict(mejor_modelo_inner, datos_test, type = "raw")
      
      auc_outer <- auc(roc(datos_test[[variable_estudio]], probabilidades_ANN))
      
      FOLD <- c(FOLD, i)
      AUC <- c(AUC, auc_outer)
      SIZE <- c(SIZE, parametros[posicion_max, ]$size)
      DECAY <- c(DECAY, parametros[posicion_max, ]$decay)
      VARIABLES <- rbind(VARIABLES, data.frame(Variables = variables_inner[posicion_max], stringsAsFactors = FALSE))
    }
  }
  
  metricas <- data.frame(Semilla = rep(semillas, each = 5), Fold = FOLD, Auc = AUC, Size = SIZE, Decay = DECAY)
  metricas <- cbind(metricas, VARIABLES)
  return(metricas)
}

#-----------------------------------------------------------------------------------------------------------


# lista_param: c(0.01, 0.005, 0.001, 0.0005)

cv5x2_dt_embedded <- function(df_original, variable_estudio, lista_param){
  semillas <- c(3, 7, 13, 25, 39)
  df <- dummy_escalado(df_original)
  
  FOLD <- c()
  AUC <- c()
  CP <- c()
  VARIABLES <- data.frame(Variables = character(), stringsAsFactors = FALSE)
  
  for(semilla in semillas){
    set.seed(semilla)
    
    # Crear las cajas de test y entrenamiento para el outer-loop
    folds_outer <- createFolds(df[[variable_estudio]], k = 5)
    
    # Realizar la validación cruzada 5x2
    # Outer-loop
    for(i in 1:5){
      # Crear las cajas de validación y entrenamiento para el inner-loop
      folds_inner <- folds_outer[-i]
      datos_test <- df[unlist(folds_outer[i]),]
      
      # Inicializar el vector de auc para el inner loop
      auc_inner <- c()
      variables_inner <- c()
      
      # Separar los datos en conjuntos de entrenamiento y validación
      datos_inner <- df[unlist(folds_inner), ]
      mitad <- floor(nrow(datos_inner)/2)
      datos_train <- datos_inner[1:mitad, ]
      datos_val <- datos_inner[(mitad+1):nrow(datos_inner), ]
      # Inner-loop
      for(j in 1:2){
        if(j == 2){
          aux = datos_train
          datos_train = datos_val
          datos_val= aux
        }
        
        variables_seleccionadas <- modelo_lasso(datos_train, variable_estudio)
        
        if (length(variables_seleccionadas) == 0) {
          variables_seleccionadas <- names(datos_train)[which(names(datos_train) != variable_estudio)][2]
        }
        
        formula <- paste(variable_estudio, " ~ ", paste(variables_seleccionadas, collapse = " + "))
        
        # Ajustar el modelo para cada combinación de parámetros
        for(n in lista_param){
          
          # Entrenar modelo
          modelo_DT <- rpart(as.formula(formula), data = datos_train, method = "class", cp = n)
          probabilidades_DT <- predict(modelo_DT, datos_val, type="prob")[,2]
          
          auc_modelo <- auc(roc(datos_val[[variable_estudio]], probabilidades_DT))
          auc_inner <- c(auc_inner, auc_modelo)
          variables_inner <- c(variables_inner, paste(variables_seleccionadas, collapse = ", "))
        }
      }
      
      auc_mediado <- c()
      mitad_long <- length(auc_inner)/2
      for (x in 1:mitad_long){
        auc_mediado <- c(auc_mediado, (auc_inner[x]+auc_inner[mitad_long+x])/2)
      }
      
      posicion_max <- which.max(auc_mediado)
      mejores_variables_inner <- unlist(strsplit(variables_inner[posicion_max], ", "))
      formula <- paste(variable_estudio, " ~ ", paste(mejores_variables_inner, collapse = " + "))
      
      mejor_modelo_inner <- rpart(as.formula(formula), data = datos_inner, method = "class", cp = lista_param[posicion_max])
      probabilidades_DT_inner <- predict(mejor_modelo_inner, datos_test, type="prob")[,2]
      
      auc_outer <- auc(roc(datos_test[[variable_estudio]], probabilidades_DT_inner))
      
      FOLD <- c(FOLD, i)
      AUC <- c(AUC, auc_outer)
      CP <- c(CP, lista_param[posicion_max])
      VARIABLES <- rbind(VARIABLES, data.frame(Variables = variables_inner[posicion_max], stringsAsFactors = FALSE))
    }
  }
  metricas <- data.frame(Semilla = semillas, Fold = FOLD, Auc = AUC, cp = CP)
  metricas <- cbind(metricas, VARIABLES)
  return(metricas)
}

#-----------------------------------------------------------------------------------------------------------

# lista_param: k = c(13, 15, 17, 19, 21, 23)

cv5x2_knn_embedded <- function(df_original, variable_estudio, lista_param){
  # Hacer one hot encoding y escalar datos
  df <- dummy_escalado(df_original)
  
  semillas <- c(3, 7, 13, 25, 39)
  
  FOLD <- c()
  AUC <- c()
  K <- c()
  VARIABLES <- data.frame(Variables = character(), stringsAsFactors = FALSE)
  
  for(semilla in semillas){
    set.seed(semilla)
    
    # Crear las cajas de test y entrenamiento para el outer-loop
    folds_outer <- createFolds(df[[variable_estudio]], k = 5)
    
    # Realizar la validación cruzada 5x2
    # Outer-loop
    for(i in 1:5){
      # Crear las cajas de validación y entrenamiento para el inner-loop
      folds_inner <- folds_outer[-i]
      datos_test <- df[unlist(folds_outer[i]),]
      
      # Inicializar el vector de auc para el inner loop
      auc_inner <- c()
      variables_inner <- c()
      # Separar los datos en conjuntos de entrenamiento y validación
      datos_inner <- df[unlist(folds_inner), ]
      mitad <- floor(nrow(datos_inner)/2)
      datos_train <- datos_inner[1:mitad, ]
      datos_val <- datos_inner[(mitad+1):nrow(datos_inner), ]
      # Inner-loop
      for(j in 1:2){
        if(j == 2){
          aux = datos_train
          datos_train = datos_val
          datos_val= aux
        }
        
        variables_seleccionadas <- modelo_lasso(datos_train, variable_estudio)
        
        if (length(variables_seleccionadas) == 0) {
          variables_seleccionadas <- names(datos_train)[which(names(datos_train) != variable_estudio)][1]
        }
        
        datos_train_mod <- datos_train[, variables_seleccionadas]
        datos_val_mod <- datos_val[, variables_seleccionadas]
        
        # Ajustar el modelo para cada combinación de parámetros
        for(n in lista_param){
          
          # Entrenar modelo
          probabilidades_KNN <- knn(train = datos_train_mod, test = datos_val_mod, cl = datos_train[[variable_estudio]], k = n, prob = TRUE)
          
          auc_modelo <- auc(roc(datos_val[[variable_estudio]], attr(probabilidades_KNN, which = "prob")))
          auc_inner <- c(auc_inner, auc_modelo)
          variables_inner <- c(variables_inner, paste(variables_seleccionadas, collapse = ", "))
        }
      }
      
      auc_mediado <- c()
      mitad_long <- length(auc_inner)/2
      for (x in 1:mitad_long){
        auc_mediado <- c(auc_mediado, (auc_inner[x]+auc_inner[mitad_long+x])/2)
      }
      
      posicion_max <- which.max(auc_mediado)
      mejores_variables_inner <- unlist(strsplit(variables_inner[posicion_max], ", "))
      
      datos_inner_mod <- datos_inner[,mejores_variables_inner]
      datos_test_mod <- datos_test[,mejores_variables_inner]
      
      probabilidades_KNN_inner <- knn(train = datos_inner_mod, test = datos_test_mod, cl = datos_inner[[variable_estudio]], k = lista_param[posicion_max], prob = TRUE)
      
      auc_outer <- auc(roc(datos_test[[variable_estudio]], attr(probabilidades_KNN_inner, which = "prob")))
      
      FOLD <- c(FOLD, i)
      AUC <- c(AUC, auc_outer)
      K <- c(K, lista_param[posicion_max])
      VARIABLES <- rbind(VARIABLES, data.frame(Variables = variables_inner[posicion_max], stringsAsFactors = FALSE))
    }
  }
  metricas <- data.frame(Semilla = semillas, Fold = FOLD, Auc = AUC, K = K)
  metricas <- cbind(metricas, VARIABLES)
  return(metricas)
}

#-----------------------------------------------------------------------------------------------------------

cv5x2_nb_embedded <- function(df_original, variable_estudio){
  semillas <- c(3, 7, 13, 25, 39)
  df <- dummy_escalado(df_original)
  
  FOLD <- c()
  AUC <- c()
  VARIABLES <- data.frame(Variables = character(), stringsAsFactors = FALSE)
  
  for(semilla in semillas){
    set.seed(semilla)
    
    # Crear las cajas de test y entrenamiento para el outer-loop
    folds_outer <- createFolds(df[[variable_estudio]], k = 5)
    
    # Realizar la validación cruzada 5x2
    # Outer-loop
    for(i in 1:5){
      # Crear las cajas de validación y entrenamiento para el inner-loop
      folds_inner <- folds_outer[-i]
      datos_test <- df[unlist(folds_outer[i]),]
      
      # Inicializar el vector de auc para el inner loop
      auc_inner <- c()
      variables_inner <- c()
      # Separar los datos en conjuntos de entrenamiento y validación
      datos_inner <- df[unlist(folds_inner), ]
      mitad <- floor(nrow(datos_inner)/2)
      datos_train <- datos_inner[1:mitad, ]
      datos_val <- datos_inner[(mitad+1):nrow(datos_inner), ]
      # Inner-loop
      for(j in 1:2){
        if(j == 2){
          aux = datos_train
          datos_train = datos_val
          datos_val = aux
        }
        
        variables_seleccionadas <- modelo_lasso(datos_train, variable_estudio)
        
        if (length(variables_seleccionadas) == 0) {
          variables_seleccionadas <- names(datos_train)[which(names(datos_train) != variable_estudio)][2]
        }
        
        formula <- paste(variable_estudio, " ~ ", paste(variables_seleccionadas, collapse = " + "))
        
        # Entrenar modelo
        modelo_nb <- naiveBayes(as.formula(formula), data = datos_train)
        
        probabilidades_nb <- predict(modelo_nb, datos_val, type="raw")[,2]
        
        auc_modelo <- auc(roc(datos_val[[variable_estudio]], probabilidades_nb))
        auc_inner <- c(auc_inner, auc_modelo)
        variables_inner <- c(variables_inner, paste(variables_seleccionadas, collapse = ", "))
      }
      
      auc_mediado <- (auc_inner[1]+auc_inner[2])/2
      posicion_max <- which.max(auc_inner)
      
      mejores_variables_inner <- unlist(strsplit(variables_inner[posicion_max], ", "))
      formula <- paste(variable_estudio, " ~ ", paste(mejores_variables_inner, collapse = " + "))
      
      mejor_modelo_inner <- naiveBayes(as.formula(formula), data = datos_inner)
      
      probabilidades_nb <- predict(mejor_modelo_inner, datos_test, type="raw")[,2]
      
      auc_outer <- auc(roc(datos_test[[variable_estudio]], probabilidades_nb))
      
      FOLD <- c(FOLD, i)
      AUC <- c(AUC, auc_outer)
      VARIABLES <- rbind(VARIABLES, data.frame(Variables = variables_inner[posicion_max], stringsAsFactors = FALSE))
    }
  }
  metricas <- data.frame(Semilla = semillas, Fold = FOLD, Auc = AUC)
  metricas <- cbind(metricas, VARIABLES)
  return(metricas)
}

#-----------------------------------------------------------------------------------------------------------

cv5x2_rf_embedded <- function(df_original, variable_estudio, lista_param){
  semillas <- c(3, 7, 13, 25, 39)
  df <- dummy_escalado(df_original)
  
  # Creación del meshgrid (combinación de los valores de los parámetros)
  parametros <- expand.grid(lista_param)
  
  FOLD <- c()
  AUC <- c()
  NTREE <- c()
  MAXNODES <- c()
  VARIABLES <- data.frame(Variables = character(), stringsAsFactors = FALSE)
  
  for(semilla in semillas){
    set.seed(semilla)
    
    # Crear las cajas de test y entrenamiento para el outer-loop
    folds_outer <- createFolds(df[[variable_estudio]], k = 5)
    
    # Realizar la validación cruzada 5x2
    # Outer-loop
    for(i in 1:5){
      # Crear las cajas de validación y entrenamiento para el inner-loop
      folds_inner <- folds_outer[-i]
      datos_test <- df[unlist(folds_outer[i]),]
      
      # Inicializar el vector de auc para el inner loop
      auc_inner <- c()
      variables_inner <- c()
      
      # Separar los datos en conjuntos de entrenamiento y validación
      datos_inner <- df[unlist(folds_inner), ]
      mitad <- floor(nrow(datos_inner)/2)
      datos_train <- datos_inner[1:mitad, ]
      datos_val <- datos_inner[(mitad+1):nrow(datos_inner), ]
      # Inner-loop
      for(j in 1:2){
        if(j == 2){
          aux = datos_train
          datos_train = datos_val
          datos_val = aux
        }
        
        # Ajustar el modelo para cada combinación de parámetros
        for(k in 1:nrow(parametros)){
          comb <- parametros[k, ]
          
          variables_seleccionadas <- modelo_lasso(datos_train, variable_estudio)
          
          if (length(variables_seleccionadas) == 0) {
            variables_seleccionadas <- names(datos_train)[which(names(datos_train) != variable_estudio)][2]
          }
          
          formula <- paste(variable_estudio, " ~ ", paste(variables_seleccionadas, collapse = " + "))
          
          # Entrenar modelo
          modelo_RF <- randomForest(as.formula(formula), data = datos_train, ntree = comb$ntree, maxnodes = comb$maxnodes)
          
          probabilidades_RF <- predict(modelo_RF, datos_val, type="prob")[,2]
          
          auc_modelo <- auc(roc(datos_val[[variable_estudio]], probabilidades_RF))
          auc_inner <- c(auc_inner, auc_modelo)
          variables_inner <- c(variables_inner, paste(variables_seleccionadas, collapse = ", "))
        }
      }
      
      auc_mediado <- c()
      mitad_long <- length(auc_inner)/2
      for (x in 1:mitad_long){
        auc_mediado <- c(auc_mediado, (auc_inner[x]+auc_inner[mitad_long+x])/2)
      }
      
      posicion_max <- which.max(auc_mediado)
      mejores_variables_inner <- unlist(strsplit(variables_inner[posicion_max], ", "))
      formula <- paste(variable_estudio, " ~ ", paste(mejores_variables_inner, collapse = " + "))
      
      mejor_modelo_inner <- randomForest(as.formula(formula), data = datos_inner, ntree = parametros[posicion_max,]$ntree, maxnodes = parametros[posicion_max,]$maxnodes)
      
      probabilidades_RF_inner <- predict(mejor_modelo_inner, datos_test, type="prob")[,2]
      
      auc_outer <- auc(roc(datos_test[[variable_estudio]], probabilidades_RF_inner))
      
      FOLD <- c(FOLD, i)
      AUC <- c(AUC, auc_outer)
      NTREE <- c(NTREE, parametros[posicion_max,]$ntree)
      MAXNODES <- c(MAXNODES, parametros[posicion_max,]$maxnodes)
      VARIABLES <- rbind(VARIABLES, data.frame(Variables = variables_inner[posicion_max], stringsAsFactors = FALSE))
    }
  }
  metricas <- data.frame(Semilla = semillas, Fold = FOLD, Auc = AUC, Ntree = NTREE, Maxnodes = MAXNODES)
  metricas <- cbind(metricas, VARIABLES)
  return(metricas)
}

#-----------------------------------------------------------------------------------------------------------

cv5x2_ab_embedded <- function(df_original, variable_estudio, lista_param){
  semillas <- c(3, 7, 13, 25, 39)
  df <- dummy_escalado(df_original)
  
  FOLD <- c()
  AUC <- c()
  MFINAL <- c()
  VARIABLES <- data.frame(Variables = character(), stringsAsFactors = FALSE)
  
  for(semilla in semillas){
    set.seed(semilla)
    
    # Crear las cajas de test y entrenamiento para el outer-loop
    folds_outer <- createFolds(df[[variable_estudio]], k = 5)
    
    # Realizar la validación cruzada 5x2
    # Outer-loop
    for(i in 1:5){
      # Crear las cajas de validación y entrenamiento para el inner-loop
      folds_inner <- folds_outer[-i]
      datos_test <- df[unlist(folds_outer[i]),]
      
      # Inicializar el vector de auc para el inner loop
      auc_inner <- c()
      variables_inner <- c()
      # Separar los datos en conjuntos de entrenamiento y validación
      datos_inner <- df[unlist(folds_inner), ]
      mitad <- floor(nrow(datos_inner)/2)
      datos_train <- datos_inner[1:mitad, ]
      datos_val <- datos_inner[(mitad+1):nrow(datos_inner), ]
      # Inner-loop
      for(j in 1:2){
        if(j == 2){
          aux = datos_train
          datos_train = datos_val
          datos_val= aux
        }
        
        # Ajustar el modelo para cada combinación de parámetros
        for(n in lista_param){
          
          variables_seleccionadas <- modelo_lasso(datos_train, variable_estudio)
          
          if (length(variables_seleccionadas) == 0) {
            variables_seleccionadas <- names(datos_train)[which(names(datos_train) != variable_estudio)][2]
          }
          
          formula <- paste(variable_estudio, " ~ ", paste(variables_seleccionadas, collapse = " + "))
          
          # Entrenar el modelo AdaBoost
          modelo_adaboost <- boosting(as.formula(formula), data = datos_train, boos = TRUE, mfinal = n)
          predicciones <- predict(modelo_adaboost, newdata = datos_val)
          probabilidades <- (predicciones$prob)[,2]
          
          auc_modelo <- auc(roc(datos_val[[variable_estudio]], probabilidades))
          auc_inner <- c(auc_inner, auc_modelo)
          
        }
      }
      
      auc_mediado <- c()
      mitad_long <- length(auc_inner)/2
      for (x in 1:mitad_long){
        auc_mediado <- c(auc_mediado, (auc_inner[x]+auc_inner[mitad_long+x])/2)
      }
      
      posicion_max <- which.max(auc_mediado)
      mejores_variables_inner <- unlist(strsplit(variables_inner[posicion_max], ", "))
      formula <- paste(variable_estudio, " ~ ", paste(mejores_variables_inner, collapse = " + "))
      
      mejor_modelo_inner <- boosting(as.formula(formula), data = datos_inner, mfinal = lista_param[posicion_max])
      predicciones_ab_inner <- predict(mejor_modelo_inner, newdata = datos_test)
      probabilidades_ab_inner <- (predicciones_ab_inner$prob)[,2]
      
      auc_outer <- auc(roc(datos_test[[variable_estudio]], probabilidades_ab_inner))
      
      FOLD <- c(FOLD, i)
      AUC <- c(AUC, auc_outer)
      MFINAL <- c(MFINAL, lista_param[posicion_max])
      VARIABLES <- rbind(VARIABLES, data.frame(Variables = variables_inner[posicion_max], stringsAsFactors = FALSE))
    }
  }
  metricas <- data.frame(Semilla = semillas, Fold = FOLD, Auc = AUC, Mfinal = MFINAL)
  metricas <- cbind(metricas, VARIABLES)
  return(metricas)
}

modelo_lasso <- function(datos_train, variable_estudio){
  # Ajustar el modelo Lasso y seleccionar variables
  X_train <- as.matrix(datos_train[, -which(names(datos_train) == variable_estudio)])
  y_train <- datos_train[[variable_estudio]]
  cv_lasso <- cv.glmnet(X_train, y_train, alpha = 1, family = "binomial")
  best_lambda <- cv_lasso$lambda.min
  lasso_model <- glmnet(X_train, y_train, alpha = 1, lambda = best_lambda, family = "binomial")
  selected_variables <- rownames(coef(lasso_model))[which(coef(lasso_model) != 0)]
  selected_variables <- selected_variables[selected_variables != "(Intercept)"]
  return(selected_variables)
}
#-----------------------------------------------------------------------------------------------------------

dummy_escalado <- function(df){
  var_categoricas <- c("REst", "RPro", "Grado", "Fenotipo")
  
  df_dummy <- df[c("REst", "RPro", "Grado", "Fenotipo", "PCR")]
  for (var in var_categoricas) {
    # Generar dummies sin omitir la primera categoría
    dummy <- model.matrix(~ . - 1, data = df_dummy[, var, drop = FALSE])
    # Eliminar la columna original
    df_dummy <- df_dummy[, !names(df_dummy) %in% var]
    # Añadir las nuevas columnas dummy
    df_dummy <- cbind(df_dummy, dummy)
  }
  return(df_dummy)
}

