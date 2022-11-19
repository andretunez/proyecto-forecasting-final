

library(readxl)
setwd("/Users/andretunez/Downloads/proyecto fore")

pronostico <- read_excel("bddproyecto2.xlsx", 
                          sheet = "PRONOSTICO MES 4")
View(pronostico)

library(zoo)
library(lmtest)

modelo1 = lm(formula = pronostico$`Monto de Ingresos Total Por Mes` ~ pronostico$Mes, 
             data = pronostico)
summary(modelo1)

#A través del resumen de los datos estadísticos, podemos observar que el 
#valor del coeficiente de determinación es R2: 0.05489, por lo que los 
#datos representados en el modelo no explica el la variabilidad de los
#datos en entorno a su media. 

#Validación de la significancia 

#Ho:β1=0
#Ha:β1≠0

#Debido a que el valor−p ≤ α, podemos decir que no se rechaza la hipotesis nula, lo cual nos permite afirmar que β1 no es diferente del valor cero, 
#por lo que la variable “Mes” no es significativa para el modelo.

#Validación de los supuestos 

plot(modelo1$residuals)

#En la gráfica del modelo podemos observar el comportamiento de los puntos residuales. 
#Si nos imaginamos una franja líneal trazeado en la gráfica de dispersón, se puede observar que los puntos resiaules se encuentran dentro de la pendiente, por lo que, el modelo presenta una varianza constante. 

  dwtest(modelo1)

# H0:ρ=0Residuales Independientes
# Ha:ρ≠0Residuales Dependientes

# Con un valor de significancia (alpha) del 0.05, a través de una prueba Durbin Watson, podemos observar que el 
# valorp=  0.02309 es menor al valor de alpha, por lo que SI habría evidencia para rechazar Ho. 
  #Se puede definir que los residuales no son independientes entre si. 
  
qqnorm(modelo1$residuals)

#A través de una gráfica QQ-Cuantil, podemos verificar la distribucción de los puntos residuales. 
#Claramente se pueden ver como los puntos residuales tienen una distribución de normalidad ya que dibujan una linea recta. 

shapiro.test(modelo1$residuals)

# Ho = Los residuales se distribuyen normalmente
# Ha = Los residuales NO se distribuyen normalmente

#Con la prueba Shapiro podemos concluir si la distribución de los residuales represeta una normalidad. 
#Con el resultado obtenido de valor−p= 0.9496 podemos deducir que los residuales si presentan un comportamiento de normalidad ya que el valor p es mayor al valor alpha
#por lo que no es posible rechazar Ho. 

#Predicción de ingresos en el siguiente mes de Abril 
 
x = 4
ingresos = 48175091 + 217931*4
ingresos #Total mes de Abril 2015
