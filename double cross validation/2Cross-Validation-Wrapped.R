cv5x2_svm_wrapped <- function(df, variable_estudio, lista_param) {
  semillas <- c(3, 7, 13, 25, 39)
  
  # Creación del meshgrid (combinación de los valores de los parámetros)
  parametros <- expand.grid(lista_param)
  
  FOLD <- c()
  AUC <- c()
  KERNEL <- c()
  GAMMA <- c()
  C <- c()
  mejores_variables <- data.frame(Variables = character(), stringsAsFactors = FALSE)
  
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
        
        # Ajustar el modelo para cada combinación de parámetros
        for (k in 1:nrow(parametros)) {
          comb <- parametros[k, ]
          
          # Inicializar con todas las características
          features <- names(datos_train)[-which(names(datos_train) == variable_estudio)] # Todas las características excepto la variable objetivo
          
          formula <- paste(variable_estudio, " ~ ", paste(features, collapse = " + "))
          m <- svm(as.formula(formula), data = datos_train, kernel = as.character(comb$kernel), gamma = comb$gamma, cost = comb$C, probability=TRUE)
          prob <- predict(m, datos_val, probability=TRUE)
          prob <- attr(prob, which="probabilities")[,2]
          
          best_auc <- auc(roc(datos_val[[variable_estudio]], prob))
          
          while (length(features) > 1) {
            aucs <- numeric(length(features))
            for (l in 1:length(features)) { # Cambio de índice para evitar conflictos
              # Eliminar una característica y evaluar el modelo
              new_features <- features[-l]
              
              formula <- paste(variable_estudio, " ~ ", paste(new_features, collapse = " + "))
              m <- svm(as.formula(formula), data = datos_train, kernel = as.character(comb$kernel), gamma = comb$gamma, cost = comb$C, probability=TRUE)
              prob <- predict(m, datos_val, probability=TRUE)
              prob <- attr(prob, which="probabilities")[,2]
              
              aucs[l] <- auc(roc(datos_val[[variable_estudio]], prob))
            }
            # Conservar la característica que maximiza el AUC
            best_index <- which.max(aucs)
            if (aucs[best_index] > best_auc) {
              best_auc <- aucs[best_index]
              features <- features[-best_index]
            } else {
              break
            }
          }
          
          variables_inner <- c(variables_inner, paste(features, collapse = ", "))
          auc_inner <- c(auc_inner, best_auc)
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
      
      mejor_modelo_inner <- svm(as.formula(formula), data = datos_inner, kernel = as.character(parametros[posicion_max,]$kernel), gamma = parametros[posicion_max,]$gamma, cost = parametros[posicion_max,]$C, probability=TRUE)
      
      probabilidades_SVM_inner <- predict(mejor_modelo_inner, datos_test, probability=TRUE)
      svm.pred_inner <- attr(probabilidades_SVM_inner, which="probabilities")[,2]
      
      auc_outer <- auc(roc(datos_test[[variable_estudio]], svm.pred_inner))
      
      FOLD <- c(FOLD, i)
      AUC <- c(AUC, auc_outer)
      KERNEL <- c(KERNEL, as.character(parametros[posicion_max,]$kernel))
      GAMMA <- c(GAMMA, parametros[posicion_max,]$gamma)
      C <- c(C, parametros[posicion_max,]$C)
      mejores_variables <- rbind(mejores_variables, data.frame(Variables = variables_inner[posicion_max], stringsAsFactors = FALSE))
    }
  }
  
  metricas <- data.frame(Semilla = rep(semillas, each = 5), Fold = FOLD, Auc = AUC, Kernel = as.factor(KERNEL), Gamma = GAMMA, Cost = C)
  metricas <- cbind(metricas, mejores_variables)
  return(metricas)
}

#-----------------------------------------------------------------------------------

cv5x2_ann_wrapped <- function(df, variable_estudio, lista_param){
  semillas <- c(3, 7, 13, 25, 39)
  
  # Creación del meshgrid (combinación de los valores de los parámetros)
  parametros <- expand.grid(lista_param)
  
  FOLD <- c()
  AUC <- c()
  SIZE <- c()
  DECAY <- c()
  mejores_variables <- data.frame(Variables = character(), stringsAsFactors = FALSE)
  
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
          
          # Inicializar con todas las características
          features <- names(datos_train)[-which(names(datos_train) == variable_estudio)] # Todas las características excepto la variable objetivo
          
          formula <- paste(variable_estudio, " ~ ", paste(features, collapse = " + "))
          m <- nnet(as.formula(formula), data = datos_train, size = comb$size, maxit = 300, decay = comb$decay)
          prob <- predict(m, datos_val, type="raw")
          
          best_auc <- auc(roc(datos_val[[variable_estudio]], prob))
          
          while (length(features) > 1) {
            aucs <- numeric(length(features))
            for (i in 1:length(features)) {
              # Eliminar una característica y evaluar el modelo
              new_features <- features[-i]
              
              formula <- paste(variable_estudio, " ~ ", paste(new_features, collapse = " + "))
              m <- nnet(as.formula(formula), data = datos_train, size = comb$size, maxit = 300, decay = comb$decay)
              prob <- predict(m, datos_val, type="raw")
              
              aucs[i] <- auc(roc(datos_val[[variable_estudio]], prob))
            }
            # Conservar la característica que maximiza el AUC
            best_index <- which.max(aucs)
            if (aucs[best_index] > best_auc) {
              best_auc <- aucs[best_index]
              features <- features[-best_index]
            } else {
              break
            }
          }
          
          variables_inner <- c(variables_inner, paste(features, collapse = ", "))
          auc_inner <- c(auc_inner, best_auc)
          
        }
      }
      
      auc_mediado <- c()
      mitad_long <- length(auc_inner)/2
      for (x in 1:mitad_long){
        auc_mediado <- c(auc_mediado, (auc_inner[x]+auc_inner[mitad_long+x])/2)
      }
      
      posicion_max <- which.max(auc_mediado)
      
      # Crear el modelo con las mejores variables encontradas
      best_features <- strsplit(variables_inner[posicion_max], ", ")[[1]]
      formula <- paste(variable_estudio, " ~ ", paste(best_features, collapse = " + "))
      
      mejor_modelo_inner <- nnet(as.formula(formula), data = datos_inner, size = parametros[posicion_max,]$size, maxit = 300, decay = parametros[posicion_max,]$decay)
      
      probabilidades_ANN <- predict(mejor_modelo_inner, datos_test, type="raw")
      
      auc_outer <- auc(roc(datos_test[[variable_estudio]], probabilidades_ANN))
      
      FOLD <- c(FOLD, i)
      AUC <- c(AUC, auc_outer)
      SIZE <- c(SIZE, parametros[posicion_max,]$size)
      DECAY <- c(DECAY, parametros[posicion_max,]$decay)
      mejores_variables <- rbind(mejores_variables, data.frame(Variables = variables_inner[posicion_max], stringsAsFactors = FALSE))
      
    }
  }
  metricas <- data.frame(Semilla = rep(semillas, each=5), Fold = FOLD, Auc = AUC, Size = SIZE, Decay = DECAY)
  metricas <- cbind(metricas, mejores_variables)
  return(metricas)
}

#-----------------------------------------------------------------------------------

cv5x2_rf_wrapped <- function(df, variable_estudio, lista_param){
  semillas <- c(3, 7, 13, 25, 39)
  
  # Creación del meshgrid (combinación de los valores de los parámetros)
  parametros <- expand.grid(lista_param)
  
  FOLD <- c()
  AUC <- c()
  NTREE <- c()
  MAXNODES <- c()
  mejores_variables <- data.frame(Variables = character(), stringsAsFactors = FALSE)
  
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
          
          # Inicializar con todas las características
          features <- names(datos_train)[-which(names(datos_train) == variable_estudio)] # Todas las características excepto la variable objetivo
          
          formula <- paste(variable_estudio, " ~ ", paste(features, collapse = " + "))
          m <- randomForest(as.formula(formula), data = datos_train, ntree = comb$ntree, maxnodes = comb$maxnodes)
          prob <- predict(m, datos_val, type="prob")[,2]
          
          best_auc <- auc(roc(datos_val[[variable_estudio]], prob))
          
          while (length(features) > 1) {
            aucs <- numeric(length(features))
            for (i in 1:length(features)) {
              # Eliminar una característica y evaluar el modelo
              new_features <- features[-i]
              
              formula <- paste(variable_estudio, " ~ ", paste(new_features, collapse = " + "))
              m <- randomForest(as.formula(formula), data = datos_train, ntree = comb$ntree, maxnodes = comb$maxnodes)
              prob <- predict(m, datos_val, type="prob")[,2]
              
              aucs[i] <- auc(roc(datos_val[[variable_estudio]], prob))
            }
            # Conservar la característica que maximiza el AUC
            best_index <- which.max(aucs)
            if (aucs[best_index] > best_auc) {
              best_auc <- aucs[best_index]
              features <- features[-best_index]
            } else {
              break
            }
          }
          
          variables_inner <- c(variables_inner, paste(features, collapse = ", "))
          auc_inner <- c(auc_inner, best_auc)
          
        }
      }
      
      auc_mediado <- c()
      mitad_long <- length(auc_inner)/2
      for (x in 1:mitad_long){
        auc_mediado <- c(auc_mediado, (auc_inner[x]+auc_inner[mitad_long+x])/2)
      }
      
      posicion_max <- which.max(auc_mediado)
      
      # Crear el modelo con las mejores variables encontradas
      best_features <- strsplit(variables_inner[posicion_max], ", ")[[1]]
      formula <- paste(variable_estudio, " ~ ", paste(best_features, collapse = " + "))
      
      mejor_modelo_inner <- randomForest(as.formula(formula), data = datos_inner, ntree = parametros[posicion_max,]$ntree, maxnodes = parametros[posicion_max,]$maxnodes)
      
      probabilidades_RF_inner <- predict(mejor_modelo_inner, datos_test, type="prob")[,2]
      
      auc_outer <- auc(roc(datos_test[[variable_estudio]], probabilidades_RF_inner))
      
      FOLD <- c(FOLD, i)
      AUC <- c(AUC, auc_outer)
      NTREE <- c(NTREE, parametros[posicion_max,]$ntree)
      MAXNODES <- c(MAXNODES, parametros[posicion_max,]$maxnodes)
      mejores_variables <- rbind(mejores_variables, data.frame(Variables = variables_inner[posicion_max], stringsAsFactors = FALSE))
      
    }
  }
  metricas <- data.frame(Semilla = rep(semillas, each=5), Fold = FOLD, Auc = AUC, Ntree = NTREE, Maxnodes = MAXNODES)
  metricas <- cbind(metricas, mejores_variables)
  return(metricas)
}

#-----------------------------------------------------------------------------------

cv5x2_knn_wrapped <- function(df_original, variable_estudio, lista_param){
  # Hacer one hot encoding y escalar datos
  df <- dummy_escalado(df_original)
  
  semillas <- c(3, 7, 13, 25, 39)
  
  # Creación del meshgrid (combinación de los valores de los parámetros)
  parametros <- lista_param
  
  FOLD <- c()
  AUC <- c()
  K <- c()
  mejores_variables <- data.frame(Variables = character(), stringsAsFactors = FALSE)
  
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
        for(n in parametros){
          
          # Inicializar con todas las características
          features <- names(datos_train)[-which(names(datos_train) == variable_estudio)] # Todas las características excepto la variable objetivo
          
          # Entrenar el modelo con todas las características
          probabilidades_KNN <- knn(train = datos_train[, features], test = datos_val[, features], cl = datos_train[[variable_estudio]], k = n, prob = TRUE)
          auc_modelo <- auc(roc(datos_val[[variable_estudio]], attr(probabilidades_KNN, which = "prob")))
          best_auc <- auc_modelo
          
          while (length(features) > 1) {
            aucs <- numeric(length(features))
            for (i in 1:length(features)) {
              # Eliminar una característica y evaluar el modelo
              new_features <- features[-i]
              
              probabilidades_KNN <- knn(train = datos_train[, new_features], test = datos_val[, new_features], cl = datos_train[[variable_estudio]], k = n, prob = TRUE)
              aucs[i] <- auc(roc(datos_val[[variable_estudio]], attr(probabilidades_KNN, which = "prob")))
            }
            # Conservar la característica que maximiza el AUC
            best_index <- which.max(aucs)
            if (aucs[best_index] > best_auc) {
              best_auc <- aucs[best_index]
              features <- features[-best_index]
            } else {
              break
            }
          }
          
          variables_inner <- c(variables_inner, paste(features, collapse = ", "))
          auc_inner <- c(auc_inner, best_auc)
          
        }
      }
      
      auc_mediado <- c()
      mitad_long <- length(auc_inner)/2
      for (x in 1:mitad_long){
        auc_mediado <- c(auc_mediado, (auc_inner[x] + auc_inner[mitad_long + x]) / 2)
      }
      
      posicion_max <- which.max(auc_mediado)
      
      # Crear el modelo con las mejores variables encontradas
      best_features <- strsplit(variables_inner[posicion_max], ", ")[[1]]
      probabilidades_KNN_inner <- knn(train = datos_inner[, best_features], test = datos_test[, best_features], cl = datos_inner[[variable_estudio]], k = parametros[posicion_max], prob = TRUE)
      
      auc_outer <- auc(roc(datos_test[[variable_estudio]], attr(probabilidades_KNN_inner, which = "prob")))
      
      FOLD <- c(FOLD, i)
      AUC <- c(AUC, auc_outer)
      K <- c(K, parametros[posicion_max])
      mejores_variables <- rbind(mejores_variables, data.frame(Variables = variables_inner[posicion_max], stringsAsFactors = FALSE))
      
    }
  }
  metricas <- data.frame(Semilla = rep(semillas, each = 5), Fold = FOLD, Auc = AUC, K = K)
  metricas <- cbind(metricas, mejores_variables)
  return(metricas)
}
#-----------------------------------------------------------------------------------

cv5x2_ab_wrapped <- function(df, variable_estudio, lista_param){
  semillas <- c(3, 7, 13, 25, 39)
  
  # Creación del meshgrid (combinación de los valores de los parámetros)
  parametros <- lista_param
  
  FOLD <- c()
  AUC <- c()
  MFINAL <- c()
  mejores_variables <- data.frame(Variables = character(), stringsAsFactors = FALSE)
  
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
        for(n in parametros){
          
          # Inicializar con todas las características
          features <- names(datos_train)[-which(names(datos_train) == variable_estudio)] # Todas las características excepto la variable objetivo
          
          # Entrenar el modelo con todas las características
          formula <- paste(variable_estudio, " ~ ", paste(features, collapse = " + "))
          modelo_adaboost <- boosting(as.formula(formula), data = datos_train, boos = TRUE, mfinal = n)
          predicciones <- predict(modelo_adaboost, newdata = datos_val)
          probabilidades <- predicciones$prob[,2]
          auc_modelo <- auc(roc(datos_val[[variable_estudio]], probabilidades))
          best_auc <- auc_modelo
          
          while (length(features) > 1) {
            aucs <- numeric(length(features))
            for (k in 1:length(features)) {
              # Eliminar una característica y evaluar el modelo
              new_features <- features[-k]
              formula <- paste(variable_estudio, " ~ ", paste(new_features, collapse = " + "))
              modelo_adaboost <- boosting(as.formula(formula), data = datos_train, boos = TRUE, mfinal = n)
              predicciones <- predict(modelo_adaboost, newdata = datos_val)
              probabilidades <- predicciones$prob[,2]
              aucs[k] <- auc(roc(datos_val[[variable_estudio]], probabilidades))
            }
            # Conservar la característica que maximiza el AUC
            best_index <- which.max(aucs)
            if (aucs[best_index] > best_auc) {
              best_auc <- aucs[best_index]
              features <- features[-best_index]
            } else {
              break
            }
          }
          
          variables_inner <- c(variables_inner, paste(features, collapse = ", "))
          auc_inner <- c(auc_inner, best_auc)
          
        }
      }
      
      auc_mediado <- c()
      mitad_long <- length(auc_inner) / 2
      for (x in 1:mitad_long){
        auc_mediado <- c(auc_mediado, (auc_inner[x] + auc_inner[mitad_long + x]) / 2)
      }
      
      posicion_max <- which.max(auc_mediado)
      
      # Crear el modelo con las mejores variables encontradas
      best_features <- strsplit(variables_inner[posicion_max], ", ")[[1]]
      formula <- paste(variable_estudio, " ~ ", paste(best_features, collapse = " + "))
      mejor_modelo_inner <- boosting(as.formula(formula), data = datos_inner, boos = TRUE, mfinal = parametros[posicion_max])
      predicciones_ab_inner <- predict(mejor_modelo_inner, newdata = datos_test)
      probabilidades_ab_inner <- predicciones_ab_inner$prob[,2]
      
      auc_outer <- auc(roc(datos_test[[variable_estudio]], probabilidades_ab_inner))
      
      FOLD <- c(FOLD, i)
      AUC <- c(AUC, auc_outer)
      MFINAL <- c(MFINAL, parametros[posicion_max])
      mejores_variables <- rbind(mejores_variables, data.frame(Variables = variables_inner[posicion_max], stringsAsFactors = FALSE))
      
    }
  }
  metricas <- data.frame(Semilla = rep(semillas, each = 5), Fold = FOLD, Auc = AUC, mfinal = MFINAL)
  metricas <- cbind(metricas, mejores_variables)
  return(metricas)
}
#-----------------------------------------------------------------------------------

cv5x2_dt_wrapped <- function(df, variable_estudio, lista_param){
  semillas <- c(3, 7, 13, 25, 39)
  
  # Creación del meshgrid (combinación de los valores de los parámetros)
  parametros <- lista_param
  
  FOLD <- c()
  AUC <- c()
  CP <- c()
  mejores_variables <- data.frame(Variables = character(), stringsAsFactors = FALSE)
  
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
        for(n in parametros){
          
          # Inicializar con todas las características
          features <- names(datos_train)[-which(names(datos_train) == variable_estudio)] # Todas las características excepto la variable objetivo
          
          # Entrenar el modelo con todas las características
          formula <- paste(variable_estudio, " ~ ", paste(features, collapse = " + "))
          modelo_DT <- rpart(as.formula(formula), data = datos_train, method = "class", cp = n)
          probabilidades_DT <- predict(modelo_DT, datos_val, type = "prob")[, 2]
          auc_modelo <- auc(roc(datos_val[[variable_estudio]], probabilidades_DT))
          best_auc <- auc_modelo
          
          while (length(features) > 1) {
            aucs <- numeric(length(features))
            for (i in 1:length(features)) {
              # Eliminar una característica y evaluar el modelo
              new_features <- features[-i]
              formula <- paste(variable_estudio, " ~ ", paste(new_features, collapse = " + "))
              modelo_DT <- rpart(as.formula(formula), data = datos_train, method = "class", cp = n)
              probabilidades_DT <- predict(modelo_DT, datos_val, type = "prob")[, 2]
              aucs[i] <- auc(roc(datos_val[[variable_estudio]], probabilidades_DT))
            }
            # Conservar la característica que maximiza el AUC
            best_index <- which.max(aucs)
            if (aucs[best_index] > best_auc) {
              best_auc <- aucs[best_index]
              features <- features[-best_index]
            } else {
              break
            }
          }
          
          variables_inner <- c(variables_inner, paste(features, collapse = ", "))
          auc_inner <- c(auc_inner, best_auc)
          
        }
      }
      
      auc_mediado <- c()
      mitad_long <- length(auc_inner) / 2
      for (x in 1:mitad_long){
        auc_mediado <- c(auc_mediado, (auc_inner[x] + auc_inner[mitad_long + x]) / 2)
      }
      
      posicion_max <- which.max(auc_mediado)
      
      # Crear el modelo con las mejores variables encontradas
      best_features <- strsplit(variables_inner[posicion_max], ", ")[[1]]
      formula <- paste(variable_estudio, " ~ ", paste(best_features, collapse = " + "))
      mejor_modelo_inner <- rpart(as.formula(formula), data = datos_inner, method = "class", cp = parametros[posicion_max])
      probabilidades_DT_inner <- predict(mejor_modelo_inner, datos_test, type = "prob")[, 2]
      
      auc_outer <- auc(roc(datos_test[[variable_estudio]], probabilidades_DT_inner))
      
      FOLD <- c(FOLD, i)
      AUC <- c(AUC, auc_outer)
      CP <- c(CP, parametros[posicion_max])
      mejores_variables <- rbind(mejores_variables, data.frame(Variables = variables_inner[posicion_max], stringsAsFactors = FALSE))
      
    }
  }
  metricas <- data.frame(Semilla = rep(semillas, each = 5), Fold = FOLD, Auc = AUC, cp = CP)
  metricas <- cbind(metricas, mejores_variables)
  return(metricas)
}

#-----------------------------------------------------------------------------------

cv5x2_nb_wrapped <- function(df, variable_estudio) {
  semillas <- c(3, 7, 13, 25, 39)
  
  FOLD <- c()
  AUC <- c()
  mejores_variables <- data.frame(Variables = character(), stringsAsFactors = FALSE)
  
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
        
        # Inicializar con todas las características
        features <- names(datos_train)[-which(names(datos_train) == variable_estudio)] # Todas las características excepto la variable objetivo
        formula <- paste(variable_estudio, " ~ ", paste(features, collapse = " + "))
        modelo_nb <- naiveBayes(as.formula(formula), data = datos_train)
        prob <- predict(modelo_nb, datos_val, type = "raw")[,2]
        best_auc <- auc(roc(datos_val[[variable_estudio]], prob))
        
        while (length(features) > 1) {
          aucs <- numeric(length(features))
          for (k in 1:length(features)) {
            # Eliminar una característica y evaluar el modelo
            new_features <- features[-k]
            formula <- paste(variable_estudio, " ~ ", paste(new_features, collapse = " + "))
            modelo_nb <- naiveBayes(as.formula(formula), data = datos_train)
            prob <- predict(modelo_nb, datos_val, type = "raw")[,2]
            aucs[k] <- auc(roc(datos_val[[variable_estudio]], prob))
          }
          
          # Conservar la característica que maximiza el AUC
          best_index <- which.max(aucs)
          if (aucs[best_index] > best_auc) {
            best_auc <- aucs[best_index]
            features <- features[-best_index]
          } else {
            break
          }
        }
        
        variables_inner <- c(variables_inner, paste(features, collapse = ", "))
        auc_inner <- c(auc_inner, best_auc)
      }
      
      auc_mediado <- c()
      mitad_long <- length(auc_inner) / 2
      for (x in 1:mitad_long) {
        auc_mediado <- c(auc_mediado, (auc_inner[x] + auc_inner[mitad_long + x]) / 2)
      }
      
      posicion_max <- which.max(auc_mediado)
      
      # Crear el modelo con las mejores variables encontradas
      best_features <- strsplit(variables_inner[posicion_max], ", ")[[1]]
      formula <- paste(variable_estudio, " ~ ", paste(best_features, collapse = " + "))
      mejor_modelo_inner <- naiveBayes(as.formula(formula), data = datos_inner)
      
      probabilidades_nb <- predict(mejor_modelo_inner, datos_test, type = "raw")[,2]
      auc_outer <- auc(roc(datos_test[[variable_estudio]], probabilidades_nb))
      
      FOLD <- c(FOLD, i)
      AUC <- c(AUC, auc_outer)
      mejores_variables <- rbind(mejores_variables, data.frame(Variables = variables_inner[posicion_max], stringsAsFactors = FALSE))
    }
  }
  
  metricas <- data.frame(Semilla = rep(semillas, each = 5), Fold = FOLD, Auc = AUC)
  metricas <- cbind(metricas, mejores_variables)
  return(metricas)
}


#-----------------------------------------------------------------------------------

backward_selection <- function(data, variable_estudio) {
  # Inicializar con todas las características
  features <- names(data)[-which(names(data) == variable_estudio)] # Todas las características excepto la variable objetivo
  best_auc <- evaluar_modelo(features, data)
  
  while (length(features) > 1) {
    aucs <- numeric(length(features))
    for (i in 1:length(features)) {
      # Eliminar una característica y evaluar el modelo
      new_features <- features[-i]
      aucs[i] <- evaluar_modelo(new_features, data)
    }
    # Conservar la característica que maximiza el AUC
    best_index <- which.max(aucs)
    if (aucs[best_index] > best_auc) {
      best_auc <- aucs[best_index]
      features <- features[-best_index]
    } else {
      break
    }
  }
  
  return(features)
}




