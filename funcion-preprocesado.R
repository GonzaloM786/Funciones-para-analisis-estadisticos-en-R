# - Variable categórica - categórica
#   Chi-cuadrado
#   Si tiene alguna instancia menor que 5 / muestra pequeña -> Fisher
#
# - Variable categórica - numérica
#   - Dos categorías:
#       Distribución normal: t-test
#       Distribución diferente: wilcox test
#   - Más de dos categorías:
#       Distribución normal: análisis de la varianza
#       Distribución diferente: Kruskal test
#
# - Variable numérica - numérica
#   Distribución normal: correlación de Pearson
#   Distribución diferente: correlación de Spearman

# Funcionamiento
# estadistica.bivariable(datos, var1, var2) = devuelve el p-valor ($resultado) y el test ($method) entre dos variables de un df
# estadistica.general(datos) = devuelve un df con todas las combinaciones de variables, el p-valor y el test
# estadistica.general2(datos) = devuelve lo mismo pero en forma de matriz más visual
# visualizar.estadistica(datos) = genera una gráfica para ver el resultado
# variables.menor.umbral(datos) = devuelve los pares de variables cuyo test entre ellas tiene un p-valor menor a 0.05
# variables.estudio.menor.umbral(datos, variable_de_estudio) = devuelve el par variable-variable de estudio cuyo test da un p-valor menor a 0.05

library(ggplot2)

estadistica.bivariable <- function(datos, var1, var2){
  if(var1 == var2){
    return(list(resultado = 0, method = "no-test"))
  }
  else if(!is.numeric(datos[[var1]]) && !is.numeric(datos[[var2]])){ # Chi o fisher
    tab.contingencia <- xtabs(as.formula(paste("~", var1, "+", var2)), data=datos)
    if(any(tab.contingencia < 5) || nrow(datos)<100){ # Fisher
      return(list(resultado = fisher.test(tab.contingencia, simulate.p.value=TRUE)$p.value, method = "fisher"))
    }else{ # Chi
      return(list(resultado = chisq.test(tab.contingencia)$p.value, method = "chi"))
    }
  }else if(is.numeric(datos[[var1]]) && is.numeric(datos[[var2]])){
     if(shapiro.test(datos[[var1]])$p.value > 0.05 && shapiro.test(datos[[var2]])$p.value > 0.05){ # Pearson
       return(list(resultado = cor.test(datos[[var1]], datos[[var2]], method = "pearson")$p.value, method = "pearson"))
     }else{ # Spearman
       # El metodo de kendall es igual que el de spearman pero resuelve los empates
       return(list(resultado = cor.test(datos[[var1]], datos[[var2]], method = "kendall")$p.value, method = "spearman"))
     }
  }else{
    # Calculamos el numero de categorias
    if(is.numeric(datos[[var1]])){
      num.categorias <- nlevels(factor(datos[[var2]]))
    }else{
      num.categorias <- nlevels(factor(datos[[var1]]))
    }
    # Calculamos si la distribucion es normal 
    if(is.numeric(datos[[var1]])){
      if(shapiro.test(datos[[var1]])$p.value > 0.05){
        es.normal <- TRUE
      }else{
        es.normal <- FALSE
      }
    }else{
      if(shapiro.test(datos[[var2]])$p.value > 0.05){
        es.normal <- TRUE
      }else{
        es.normal <- FALSE
      }
    }
    
    # Aplicamos test
    if(num.categorias == 2){ # Dos categorias
      if(es.normal){
        #print(paste(var1, var2))
        if(is.numeric(datos[[var1]])){
          return(list(resultado=t.test(as.formula(paste(var1, "~", var2)), data = datos)$p.value, method="t-test"))
        }else{
          return(list(resultado=t.test(as.formula(paste(var2, "~", var1)), data = datos)$p.value, method="t-test"))
        }
        
      }else{
        if(is.numeric(datos[[var1]])){
          return(list(resultado=wilcox.test(as.formula(paste(var1, "~", var2)), data = datos)$p.value, method="wilcox"))
        }else{
          return(list(resultado=wilcox.test(as.formula(paste(var2, "~", var1)), data = datos)$p.value, method="wilcox"))
        }
        
      
      }
    }else{ # Mas de dos categorias
      if(es.normal){
        return(list(resultado= summary(aov(as.formula(paste(var1, "~", var2)), data = datos))[[1]][["Pr(>F)"]][1], method="aov"))
        
      }else{
        return(list(resultado=kruskal.test(as.formula(paste(var1, "~", var2)), data = datos)$p.value, method="kruksal"))
      }
    }
  }
}




 estadistica.general2 <- function(datos){
   variables <- colnames(datos) # Vector con el nombre de las variables
   df <- data.frame(matrix(ncol=length(variables), nrow=length(variables))) # Df vacio
   rownames(df) <- variables
   colnames(df) <- variables
   
   for(var1 in variables){
     for(var2 in variables){
       # Estadistica bivariable
       res <- estadistica.bivariable(datos, var1, var2)$resultado
       df[var1, var2] <- res
     }
   }  
   return(df)
 }

 estadistica.general <- function(datos){
   variables <- colnames(datos) # Vector con el nombre de las variables
   
   resultado <- numeric(0)
   var1 <- character(0)
   var2 <- character(0)
   test <- character(0)
   
   for(x in 1:(length(variables))){
     for(y in (1):length(variables)){
       
       variable_1 <- variables[x]
       variable_2 <- variables[y]
       
       res <- estadistica.bivariable(datos, variable_1, variable_2)
       
       var1 <- c(var1, variable_1)
       var2 <- c(var2, variable_2)
       resultado <- c(resultado, res$resultado)
       test <- c(test, res$method)
     }
   }
   
   df <- data.frame(Var_1 = var1, Var_2 = var2, Valor = resultado, Test = test)
   df$Umbral <- ifelse(df$Valor > 0.05, "Mayor", "Menor")
   return(df)
 }
 
visualizar.estadistica <- function(datos){
 
  dfFinal <- estadistica.general(datos)
  
  ggplot(dfFinal, aes(x = Var_1, y = Var_2, fill = Umbral)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.2g", Valor)), size = 1.5, color = "black") +
    scale_fill_manual(values = c("Menor" = "green", "Mayor" = "red")) +
    labs(title = "P-valores de la estadística bivariable", x = "", y = "") +
    theme_minimal() +
    guides(fill = FALSE)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

variables.menor.umbral <- function(datos){
  df <- estadistica.general2(datos)
  var1 <- character(0)
  var2 <- character(0)
  
  variables <- colnames(df)
  for(x in variables){
    for(y in variables){
      if(x != y && df[x, y] < 0.05){
        var1 <- c(var1, x)
        var2 <- c(var2, y)
      }
    }
  }
  
  res <- data.frame(Variable_X = var1, Variable_Y = var2)
  return(res)
}


variables.estudio.menor.umbral <- function(datos, var_estudio){
  df <- estadistica.general2(datos)
  var1 <- character(0)
  var2 <- character(0)
  valor <- double(0)
  
  variables <- colnames(df)
  for(x in variables){
      if(x != var_estudio && df[x, var_estudio] < 0.05){
        var1 <- c(var1, x)
        var2 <- c(var2, var_estudio)
        valor <- c(valor, df[x, var_estudio])
      }
  }
  
  res <- data.frame(Variable_X = var1, Variable_Y = var2, P_valor = valor)
  return(res)
}

