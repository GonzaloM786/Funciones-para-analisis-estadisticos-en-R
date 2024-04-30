# Funciones para análisis estadísticos en R

Se incluyen funciones útiles para la obtención de datos en análisis estadísticos en R. A continuación se ofrce una descripción de las mismas:

<details open>
  <summary>Funciones para la estadística descriptiva</summary>
  
## Descripción 

* __estadistica.bivariable(datos, var1, var2)__: devuelve el p-valor ($resultado) y el test ($method) entre dos variables de un dataframe.

  Las condiciones bajo las que se ejecuta cada test son las siguientes:
  
  > Variable categórica - categórica  
  >> Chi-cuadrado
  >
  >> Si tiene alguna instancia menor que 5 / muestra pequeña -> Fisher
  >
  > Variable categórica - numérica
  >> Dos categorías:
  >>> Distribución normal: t-test
  >> 
  >>> Distribución diferente: wilcox test
  >>
  >> Más de dos categorías:
  >>> Distribución normal: análisis de la varianza
  >>
  >>> Distribución diferente: Kruskal test
  >
  > Variable numérica - numérica
  >> Distribución normal: correlación de Pearson
  >
  >> Distribución diferente: correlación de Spearman
  
* __estadistica.general(datos)__: devuelve un dataframe con todas las combinaciones de variables, el p-valor y el test.
* __estadistica.general2(datos)__: devuelve un dataframe con tantas filas y columnas como variables y el p-valor en el valor de las celdas.
* __visualizar.estadistica(datos)__: genera una gráfica para ver el resultado.

  Ejemplo de gráfica:
  
  ![image](https://github.com/GonzaloM786/Funciones-para-analisis-estadisticos-en-R/assets/149195278/2fcdfc92-b760-410c-b11c-01f4ba30941e)

* __variables.menor.umbral(datos)__: devuelve los pares de variables cuyo test entre ellas tiene un p-valor menor a 0.05.
* __variables.estudio.menor.umbral(datos, variable_de_estudio)__: devuelve el par variable-variable de estudio cuyo test da un p-valor menor a 0.05.

</details>

<details open>
  <summary>Funciones para la validación interna, curva ROC y métricas de rendimiento</summary>

## Descripción

* __holdout(df, variable_estudio, tam_entren = 0.7, umbral = 0.5, show_metrics = TRUE)__: devuelve el mejor modelo usando la fuerza bruta para la selección de variables y su curva ROC asociada.

El conjunto de datos se divide en entrenamiento y test. El set de entrenamiento se usa para entrenar y validar el modelo (métricas aparentes). Una vez obtenido el mejor modelo, se vuelven a calcular las métricas sobre el conjunto de test y la curva ROC, para ver la predicción a futuro.

* __repHOut(df, variable_estudio, n_iter = 30, umbral = 0.5, tam_entren = 0.7, show_metrics = TRUE)__: devuelve el mejor modelo usando la fuerza bruta para la selección de variables y su curva ROC asociada.

El conjunto de datos se divide en entrenamiento y test, de manera estratificada, es decir, respetando las proporciones de la variable de estudio en las divisiones. El set de entrenamiento se usa para entrenar y validar el modelo (métricas aparentes). Este proceso se repite tantas veces como se le indique. Una vez obtenido el mejor modelo, se vuelven a calcular las métricas sobre el conjunto de test y la curva ROC, para ver la predicción a futuro.

* __crossVal(df, variable_estudio, k = 10, tam_entren = 0.7, umbral = 0.5, show_metrics = TRUE)__: devuelve el mejor modelo usando la fuerza bruta para la selección de variables y su curva ROC asociada.

Se aparta un conjunto de test para poder validar el modelo al final. El resto del conjunto de datos se divide en folds de igual tamaño. En cada iteración se usa uno de los folds para validar el modelo y el resto para entrenarlo. De esta manera todas las filas son usadas una y solo una vez para validar el modelo. Cuando se obtiene el mejor modelo, se vuelven a calcular las métricas sobre el conjunto de test y la curva ROC, para ver la predicción a futuro.

* __curva_roc(clase_real, probabilidad, umbral_seq = seq(0, 1, 0.01))__: devuelve la curva ROC, el AUC y el dataframe con los datos usados para dibujar la curva, a partir del conjunto de valores reales y el conjunto con las probabilidades de los valores predicho.

![image](https://github.com/GonzaloM786/Funciones-para-analisis-estadisticos-en-R/assets/149195278/34d8ac41-9c6e-4186-ae50-2766a49446fd)

* __curvas_roc(lista)__: grafica varias curvas ROC en una misma gráfica a partir de una lista de curvas ROC, las devueltas por la función anterior.

![image](https://github.com/GonzaloM786/Funciones-para-analisis-estadisticos-en-R/assets/149195278/3d9416a5-a7b6-4a6b-bdb5-053f3f731214)
  
* __matriz.confusion(Clases_Predichas, Valores_Reales)__: devuelve la matriz de confusión.

* __calcular.metricas(matriz.confusion)__: imprime las métricas asociadas a la matriz de confusión (accuracy, precision, especificidad, fallout, recall y f-measure).

</details>
