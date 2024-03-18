# Funciones para análisis estadísticos en R

Se incluyen funciones útiles para la obtención de datos en análisis estadísticos en R. A continuación se ofrce una descripción de las mismas:

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
