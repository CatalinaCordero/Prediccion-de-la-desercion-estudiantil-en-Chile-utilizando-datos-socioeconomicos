#Predicción de la deserción estudiantil en Chile utilizando datos socioeconómicos de la encuesta CASEN 2022
#Por: Catalina Andrea Cordero Zurita 21057497-2, Sofía Patricia Espinoza Gajardo 21193437-9 y Arantxa Tamara Lezaeta Mac-Lean 21194726-8.

#Durante décadas, estudios e investigaciones han demostrado que la deserción escolar es
#un problema crítico y multifactorial que tiene un impacto negativo en el futuro de los
#jóvenes, principalmente porque disminuyen sus oportunidades laborales y aumenta el riesgo
#de caer y persistir en la pobreza. Además, en países con tasas de deserción más bajas, 
#y donde esta se concentra principalmente en el nivel secundario, aumentar dos años la 
#escolaridad (aproximadamente, completar la enseñanza secundaria) genera incrementos en 
#el ingreso laboral en zonas urbanas (Santos, 2009), lo cual es solo un ejemplo de que
#esto puede tener consecuencias en el desarrollo social y económico del país. Hoy en día, 
#nos damos cuenta que la deserción escolar puede abordarse eficazmente mediante la ciencia
#de datos y algoritmos predictivos.

#Con base en una revisión bibliográfica y en ejemplos internacionales, aspiramos a que 
#esta herramienta contribuya significativamente a nuestro país, marcando el inicio de
#intervenciones o políticas que aborden estos desafíos de manera integral.

#Utilizamos la Encuesta de Caracterización Socioeconómica Nacional (CASEN) 2022, una 
#iniciativa del Ministerio de Desarrollo Social y Familia (anteriormente MIDEPLAN). 
#Esta encuesta, que reúne datos de forma anónima sobre más de 200.000 personas, tiene
#como propósito fundamental ofrecer un panorama detallado de la situación socioeconómica
#de los hogares en Chile. Entre sus objetivos se encuentran la estimación de la tasa de
#pobreza, la evaluación de la cobertura y distribución del gasto fiscal en programas sociales,
#y la identificación de cómo estos recursos se focalizan en los diferentes estratos de la
#población (Observatorio Social - Ministerio de Desarrollo Social y Familia, n.d.). Elegimos
#la CASEN como nuestra base de datos debido a su riqueza y amplitud, ya que ofrece un conjunto
#integral de variables clave relacionadas con la situación económica, la educación, la salud y
#las condiciones de vida de la población chilena, que son esenciales para nuestro análisis.

#En base a esto, partimos con una revisión exhaustiva de la bibliografía relacionada para
#identificar las variables más relevantes. Posteriormente, analizamos en su totalidad el 
#cuestionario CASEN 2022 y lo contrastamos con su respectiva base de datos para comprender e
#interpretar los resultados. A partir de este análisis, seleccionamos las preguntas que podrían
#estar vinculadas a las variables identificadas en un principio. 

#A continuación, se expone el paso a paso del código del proyecto, explicado a detalle,
#en función de cumplir con nuestro objetivo expuesto en la propuesta de proyecto: 
# Crear una interfaz de usuario intuitiva y accesible.

# Partimos por cargar los paquetes necesarios:
install.packages("dplyr")
library(dplyr)
install.packages("haven")
library(haven)
install.packages("ggplot2")
library(ggplot2)
install.packages("gridExtra")
library(gridExtra)
install.packages("tzdb")
library(tzdb)

#En primer lugar, comenzamos realizando una limpieza de la base de datos en RStudio.
#Decidimos eliminar las columnas que contenían datos de tipo texto libre, así como aquellas que tenían
#todas sus observaciones como `NA`. Finalmente, seleccionamos las columnas que contenían las
#variables que habíamos identificado según la literatura y creamos nuestro vector con estas columnas.
#Todo esto de la siguiente manera:

# Leer el archivo Stata
datos <- read_dta("/Users/sofiaespinoza/Documents/2S2024/DATA SCIENCE/Proyecto Semestral/bdproyecto.dta")

# Eliminar columnas que contienen datos de tipo texto libre
datos_sin_strings <- datos %>% select_if(~ !any(is.character(.)))

# Mostrar el resultado
print(datos_sin_strings)

# Eliminar columnas con todos los valores NA
datos_limpios<- datos_sin_strings[, colSums(is.na(datos_sin_strings)) < nrow(datos_sin_strings)]
head(datos_limpios)

# Crear un vector con los nombres de las columnas deseadas
columnas <- c('id_vivienda', 'hh_d_rez', 'hh_d_entorno', 'pobreza_multi_5d', 'nse', 'ytoth', 's5', 'e5a', 
              's7', 'e10', 'p3', 'p4', 'area', 'region', 'tipohogar', 
              'depen_grado', 'pobreza','desercion')

# Seleccionar las columnas especificadas en el vector y crear una nueva base de datos
datos_seleccionados <- datos_limpios %>% select(all_of(columnas))

table(datos_seleccionados$desercion)
datos_seleccionados

#A continuación, procedimos a investigar cómo tratar las variables categóricas en una
#regresión logística, ya que la mayoría de nuestras variables predictoras seleccionadas eran de tipo
#categórico, con varias categorías posibles en sus respuestas. Después de revisar la literatura y las
#mejores prácticas, decidimos transformar estas variables en factores etiquetados.

#----------------------------------------------------------------------------------------------------------------------------
#TRABAJANDO VARIABLES CATEGÓRICAS

#Para ello, comenzamos con la variable “nse” (nivel socioeconómico), asignando etiquetas a
#sus valores numéricos mediante la siguiente transformación:

# Asegurarse de que las variables estén en formato de factor con niveles definidos
datos_seleccionados$nse <- factor(datos_seleccionados$nse, 
                                   levels = c(1, 2, 3, 4, 5, 6, 7),
                                   labels = c("Bajo", "Medio", "Alto", 
                                              "Bajo-medio", "Bajo-alto", 
                                              "Bajo-medio-alto", 
                                              "Medio-alto"))
table(datos_seleccionados$nse)

#En este caso, asignamos un nombre significativo a cada nivel de la variable “nse”, lo cual
#facilitó su interpretación en el modelo. Posteriormente, verificamos la distribución de los niveles
#mediante la función “table()”.

#De manera similar, realizamos el mismo proceso con las siguientes variables,
#transformándolas en un factor con etiquetas correspondientes a los distintos valores de la variable. En
#todos los casos, este enfoque de etiquetar las variables categóricas como factores nos permitió
#prepararlas adecuadamente para el análisis en la regresión logística, facilitando su inclusión como
#variables predictoras en el modelo

datos_seleccionados$s5 <- factor(datos_seleccionados$s5, 
                                  levels = c(-88, 0, 1, 2, 3, 4),
                                  labels = c("No sabe", "0", "1", 
                                             "2", "3", "4"))
table(datos_seleccionados$s5)

datos_seleccionados$e5a <- factor(datos_seleccionados$e5a, 
                                  levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 
                                             11, 12, 13, 14, 15, 16, -88, -99),
                                  labels = c("Ayuda en la casa o quehaceres del hogar",
                                             "Ayuda o se dedica al cuidado de alguien",
                                             "Embarazo, maternidad o paternidad",
                                             "Tiene una discapacidad o requiere establecimiento de educación especial",
                                             "Tiene una enfermedad o condición de salud que lo(a) inhabilita",
                                             "Problemas familiares",
                                             "No le interesa o no conoce la manera para completar sus estudios",
                                             "Terminó de estudiar",
                                             "Está asistiendo a un preuniversitario",
                                             "Se encuentra preparando la Prueba de Acceso a la Educación Superior (PAES) o Prueba de Transición (PDT) de Invierno por su cuenta",
                                             "Dificultad económica",
                                             "Trabaja o busca trabajo",
                                             "Problemas de rendimiento o cancelación de matrícula",
                                             "Dificultad de acceso o movilización",
                                             "Por la pandemia COVID-19",
                                             "Otra razón. Especifique",
                                             "No sabe",
                                             "No responde"))
table(datos_seleccionados$e5a)

datos_seleccionados$s7 <- factor(datos_seleccionados$s7, 
                                 levels = c(1, 2, 3, 4),
                                 labels = c("Embarazada", "Amamantando", "Embarazada y amamantando", "No"))
table(datos_seleccionados$s7)

datos_seleccionados$e10 <- factor(datos_seleccionados$e10, 
                                  levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 
                                             10, 11, 12, 13, 14, -88),
                                  labels = c("Municipal o Servicio Local de Educación",
                                             "Particular Subvencionada",
                                             "Corporación de Administración Delegada",
                                             "Particular no Subvencionada",
                                             "JUNJI",
                                             "INTEGRA",
                                             "Jardín infantil o sala cuna del trabajo de la madre o del padre",
                                             "Centro de Formación Técnica",
                                             "Instituto Profesional",
                                             "Universidad Privada no perteneciente al Consejo de Rectores (CRUCH)",
                                             "Universidad Privada perteneciente al Consejo de Rectores (CRUCH)",
                                             "Universidad Estatal",
                                             "Establecimiento de Educación Superior de las Fuerzas Armadas y del Orden",
                                             "Establecimiento fuera de Chile",
                                             "No sabe"))
table(datos_seleccionados$e10)

datos_seleccionados$p3 <- factor(datos_seleccionados$p3, 
                                 levels = c(1, 2, 3, 4),
                                 labels = c("Mucho (observa basura en varios lugares)",
                                            "Más o menos (observa basura en 3 o 4 lugares)",
                                            "Poco (si observa cuidadosamente alrededor, verá uno o dos lugares con basura)",
                                            "Nada (no hay basura visible)"))
table(datos_seleccionados$p3)

datos_seleccionados$p4 <- factor(datos_seleccionados$p4, 
                                 levels = c(1, 2, 3, 4),
                                 labels = c("Mucho (observa 5 o más áreas con grafitis o daño deliberado)",
                                            "Más o menos (observa 3 o 4 áreas con grafitis o daño deliberado)",
                                            "Poco (observa una o dos áreas con grafitis o daño deliberado)",
                                            "Nada (no hay señales de grafitis o daño deliberado visibles)"))
table(datos_seleccionados$p4)

datos_seleccionados$depen_grado <- factor(datos_seleccionados$depen_grado,
                                          levels = c(0, 1, 2, 3),
                                          labels = c("No dependiente", "Dependencia severa", "Dependencia moderada", "Dependencia leve"))
table(datos_seleccionados$depen_grado)

datos_seleccionados$area <- factor(datos_seleccionados$area,
                                   levels = c(1, 2),
                                   labels = c("Urbano", "Rural"))
table(datos_seleccionados$area)

datos_seleccionados$region <- factor(datos_seleccionados$region,
                                     levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16),
                                     labels = c("Región de Tarapacá", "Región de Antofagasta", "Región de Atacama", 
                                                "Región de Coquimbo", "Región de Valparaíso", "Región del Libertador Gral. Bernardo O'Higgins",
                                                "Región del Maule", "Región del Biobío", "Región de La Araucanía", "Región de Los Lagos", 
                                                "Región de Aysén del Gral. Carlos Ibáñez del Campo", "Región de Magallanes y de la Antártica Chilena", 
                                                "Región Metropolitana de Santiago", "Región de Los Ríos", "Región de Arica y Parinacota", "Región de Ñuble"))
table(datos_seleccionados$region)

datos_seleccionados$tipohogar <- factor(datos_seleccionados$tipohogar,
                                        levels = c(1, 2, 3, 4, 5, 6),
                                        labels = c("Unipersonal", "Nuclear Monoparental", "Nuclear Biparental", 
                                                   "Extenso Monoparental", "Extenso Biparental", "Censal"))
table(datos_seleccionados$tipohogar)

datos_seleccionados$pobreza <- factor(datos_seleccionados$pobreza,
                                      levels = c(1, 2, 3),
                                      labels = c("Pobreza extrema", "Pobreza no extrema", "No pobreza"))
table(datos_seleccionados$pobreza)

#Dado que queríamos que la deserción fuera nuestra variable a predecir (dependiente) y, por lo
#tanto, para incluirla en nuestra regresión logística esta debía ser binaria, decidimos
#transformar la variable “desercion”, que originalmente contenía múltiples categorías, en variables
#binarias para poder incluirla en el modelo.
#En este paso, lo que hicimos fue crear nuevas variables binarias basadas en los valores de la
#columna “desercion”. Usamos la función “mutate()” del paquete “dplyr” para modificar el data frame
#“datos_seleccionados” y generar nuevas columnas, donde cada columna representó una categoría de
#la variable original como una variable binaria (0 ó 1) (Bosco Mendoza, 2020). El código que
#ejecutamos fue el siguiente:

# Modificar el data frame para obtener nuestra variable predictora binaria
datos_seleccionados <- datos_seleccionados %>%
  # Crear variables binarias para cada categoría
  mutate(
    no_sabe = ifelse(desercion == -88, 1, 0),
    abandono = ifelse(desercion == 1, 1, 0),
    desercion = ifelse(desercion == 2, 1, 0),
    nunca_asistio = ifelse(desercion == 3, 1, 0)
  )
print(head(datos_seleccionados))

#El resultado es que la variable “desercion”, que originalmente tenía varias categorías, fue
#convertida en varias variables binarias. Cada una de estas nuevas variables indica si una persona
#pertenece o no a una categoría específica relacionada con la deserción escolar. Este paso es importante
#porque la regresión logística solo puede manejar variables dependientes binarias, por lo que
#transformamos esta variable en un formato adecuado para el análisis.

#Finalmente, utilizando la función Generalized Linear Model (glm()) en R, junto con el link
#especificado, se puede aplicar el modelo de regresión logística a las variables del conjunto de datos
#seleccionados y examinar los resultados de la siguiente manera:

# Crear el modelo de regresión logística usando las variables predictoras
modelo_logistico <- glm(desercion ~ hh_d_rez + hh_d_entorno + pobreza_multi_5d + nse + ytoth + s5 + e5a + e10 + s7  + p3 + 
                          p4 + depen_grado + area + region + tipohogar + pobreza,
                        data = datos_seleccionados, 
                        family = binomial(link="logit"))
summary(modelo_logistico)

#Sin embargo, cuando corrimos ese modelo, ocurrió un error. Para mejorar la calidad de nuestro modelo y garantizar que cumpla con los objetivos
#planteados en la entrega 1, detectamos que la variable e10 estaba generando el error mencionado.
#Después de revisar el comportamiento de la variable, decidimos eliminarla de nuestro modelo. Esta
#acción fue tomada con el fin de evitar problemas que pudieran comprometer la calidad y fiabilidad de
#los resultados, ajustando así el modelo a las necesidades del análisis y las variables más relevantes
#para la predicción de la deserción.

#Nuevo modelo sin e10 porque generaba error
modelo_logistico1 <- glm(desercion ~ hh_d_rez + hh_d_entorno + pobreza_multi_5d + nse + ytoth + s5 + e5a + s7  + p3 + 
                           p4 + depen_grado + area + region + tipohogar + pobreza,
                         data = datos_seleccionados, 
                         family = binomial(link="logit"))
summary(modelo_logistico1)


#Siguiendo la lógica presentada en el texto "Data Science and Big Data Analytics", tras realizar
#múltiples ejecuciones del modelo y evaluar la significancia estadística de cada una de las variables,
#logramos obtener la evidencia suficiente para eliminar las siguientes variables, ya que sus p-values
#eran demasiado altos y no aportan información relevante para la predicción de la deserción. A continuación 
#se muestra el paso a paso de cada modelo que íbamos corriendo tras eliminar cada variable.

#Nuevo modelo sin hh_d_entorno porque tenia pvalue muy alto
modelo_logistico2 <- glm(formula = desercion ~ hh_d_rez + pobreza_multi_5d + 
                           nse + ytoth + s5 + e5a + s7 + p3 + p4 + depen_grado + area + 
                           region + tipohogar + pobreza, family = binomial(link = "logit"), 
                         data = datos_seleccionados)
summary(modelo_logistico2)


#Nuevo modelo sin tipohogar porque tenia pvalue muy alto en todas sus categorías
modelo_logistico3 <- glm(formula = desercion ~ hh_d_rez + pobreza_multi_5d + 
                           nse + ytoth + s5 + e5a + s7 + p3 + p4 + depen_grado + area + 
                           region + pobreza, family = binomial(link = "logit"), 
                         data = datos_seleccionados)
summary(modelo_logistico3)

#Nuevo modelo sin pobreza porque tenia pvalue muy alto en todas sus categorías
modelo_logistico4 <- glm(formula = desercion ~ hh_d_rez + pobreza_multi_5d + 
                           nse + ytoth + s5 + e5a + s7 + p3 + p4 + depen_grado + area + 
                           region, family = binomial(link = "logit"), 
                         data = datos_seleccionados)
summary(modelo_logistico4)


#Nuevo modelo sin depen_grado porque tenia pvalue muy alto en todas sus categorías
modelo_logistico5 <- glm(formula = desercion ~ hh_d_rez + pobreza_multi_5d + 
                           nse + ytoth + s5 + e5a + s7 + p3 + p4 + area + 
                           region, family = binomial(link = "logit"), 
                         data = datos_seleccionados)
summary(modelo_logistico5)


#Nuevo modelo sin p4 porque tenia pvalue muy alto en todas sus categorías
modelo_logistico6 <- glm(formula = desercion ~ hh_d_rez + pobreza_multi_5d + 
                           nse + ytoth + s5 + e5a + s7 + p3 + area + 
                           region, family = binomial(link = "logit"), 
                         data = datos_seleccionados)
summary(modelo_logistico6)


#Nuevo modelo sin p3 porque tenia pvalue muy alto en todas sus categorías
modelo_logistico7 <- glm(formula = desercion ~ hh_d_rez + pobreza_multi_5d + 
                           nse + ytoth + s5 + e5a + s7 + area + 
                           region, family = binomial(link = "logit"), 
                         data = datos_seleccionados)
summary(modelo_logistico7)

#Estas variables, al tener p-values superiores a los umbrales comúnmente aceptados (por
#ejemplo, 0.05 o 0.1), fueron descartadas para optimizar el modelo y centrarse en aquellas variables
#que tienen un mayor impacto en la variable de interés, en este caso, la deserción. Finalmente, el
#modelo queda como:

#Nuevo modelo sin hh_d_rez porque tenia pvalue muy alto
modelo_logistico8 <- glm(formula = desercion ~ pobreza_multi_5d + 
                           nse + ytoth + s5 + e5a + s7 + area + 
                           region, family = binomial(link = "logit"), 
                         data = datos_seleccionados)
summary(modelo_logistico8)

#Se decidió pausar el proceso de eliminación de variables en este momento, dado que, tras
#revisar el modelo y los resultados obtenidos, no existían más variables que carecieran de al menos una
#categoría significativa para el modelo. En este punto, todas las variables que permanecen en el modelo
#aportan información relevante, por lo que consideramos que continuar con la eliminación no mejoraría
#sustancialmente los resultados.

#Respecto a los resultados del modelo, algunas conclusiones destacables se presentan a
#continuación:
  #Con respecto a la variable pobreza_multi_5d, el coeficiente es 0.1702 y el p-value es
#0.017459, lo que sugiere que esta variable tiene un impacto positivo en la probabilidad de deserción.
#Un aumento en la pobreza se asocia con una mayor probabilidad de deserción.

  #Por otro lado, con respecto a la variable nse (Nivel Socioeconómico), se tiene que:
# -nseMedio: No significativo (p = 0.144349).
# -nseAlto: Significativo (p = 0.002181), con un coeficiente negativo de -0.3954, indicando que
#un nivel socioeconómico alto reduce la probabilidad de deserción.
# -nseBajo-medio: No significativo (p = 0.692527).
# -nseBajo-alto: Significativo (p = 0.044781), con un coeficiente negativo de -1.758, sugiriendo
#que un nivel socioeconómico bajo-alto disminuye la probabilidad de deserción.

  #Siguiendo con la variable ytoth (Ingreso total), el coeficiente es -6.366e-08 con un valor p de
#0.045535, lo que indica que el ingreso total tiene una relación inversa débil y significativa con la
#deserción (cada unidad de ingreso adicional disminuye la probabilidad de deserción en una cantidad
#pequeña).

  #En relación con las variables que van de s5 a s7 (Preguntas relacionadas con el entorno),
#varios de los coeficientes de las preguntas (s5, s51, s52, etc.) tienen valores p significativos. En
#particular:
# -s52 (valor de 1.538) tiene un valor p de 0.016875, lo que sugiere que una respuesta a esta
#pregunta está asociada con una mayor probabilidad de deserción.
# -s50 no es significativo (p = 0.950910), lo que indica que no tiene un efecto significativo en la
#probabilidad de deserción.

  #Por último, conforme a la variable area (Tipo de área), Rural tiene un coeficiente positivo de
#0.1856 y es significativo (p = 0.037791), lo que sugiere que vivir en un área rural está asociado con
#una mayor probabilidad de deserción.

#--------------------------------------------------------------------------------------------------------------------------------------------
#COMPROBACIÓN VALIDEZ DEL MODELO 
#VALIDACIÓN CRUZADA

#Una vez obtenido el modelo de regresión lineal, se procedió a comprobar su validez mediante
#una "validación cruzada", la cual se utilizó principalmente para evaluar el rendimiento del modelo
#implementado. Para llevar a cabo este proceso, fue necesario instalar varias librerías, entre ellas
#caret", "tidyverse" e "ISLR".
library(caret)   # Para dividir los datos y entrenar el modelo
install.packages("tidyverse")
library(tidyverse)
library(ISLR)

#Validación simple

#Decidimos utilizar el método de validación simple, en donde el primer paso fue utilizar la
#función “set.seed(1)” para establecer una semilla para el generador de números aleatorios, con esto
#aseguramos que se producirán los mismos números aleatorios cada vez que se ejecute el código. El
#segundo paso fue dividir aleatoriamente los datos en dos conjuntos: uno de entrenamiento y otro de
#testeo. En este caso, decidimos contar con un conjunto de entrenamiento del 80% de los datos
#iniciales, y el otro 20% será el conjunto de testeo. Lo anterior, se realizó con el siguiente código:

set.seed(1)
# Índices aleatorios para el set de entrenamiento 
train_index <- sample(x = nrow(datos_seleccionados), size = 0.8*(nrow(datos_seleccionados)), 
                        replace = FALSE)
# Subgrupos de entrenamiento y test
train_data <- datos_seleccionados[train_index, ]
test_data <- datos_seleccionados[-train_index, ]

#A continuación, se buscó corroborar que las distribuciones de ambos conjuntos, de
#entrenamiento y de testeo sean parecidas. En este caso ambas distribuciones son similares, en primer
#lugar, se observa que el conjunto de “desertores”, es decir, cuando el valor es 1, tiene una proporción
#de aproximadamente 65% y en segundo lugar, el conjunto de “no desertores”, cuando el valor es 0,
#tiene una proporción del alrededor de 35%. Con esto se puede seguir con el ajuste del modelo de
#regresión logística. El código utilizado y resultado se expone a continuación:

#Corroborar distribuciones sean parecidas de los subgrupos de entrenamiento y testeo
prop.table(table(train_data$desercion)) %>% round(digits = 3)
prop.table(table(test_data$desercion)) %>% round(digits = 3)

#Seguimos con el ajuste del modelo, pero esta vez específicamente con el conjunto de datos de
#entrenamiento. Esto con el fin de evitar un sobreajuste (overfitting) en el modelo realizado.

# Ajuste del modelo logístico con los datos de entrenamiento
modelo_logistico_prueba <- glm(desercion ~ pobreza_multi_5d + 
                                 nse + ytoth + s5 + e5a + s7 + area + 
                                 region, data = datos_seleccionados, 
                                 family = binomial(link = "logit"), 
                               subset = train_index)
summary(modelo_logistico_prueba)

#Al tener el modelo de regresión logística ya ajustado, calculamos la probabilidad de las
#predicciones del modelo, a través del conjunto de datos de testeo. Para esto, fue necesario transformar
#las probabilidades que eran mayores a 0.5 en 1 y 0 en caso contrario para así tener una clasificación
#binaria.
#Luego procedimos a crear la Matriz de Confusión, la cual “es una representación gráfica que nos permite ver el grado de acierto
#de nuestro modelo. El gráfico tiene cuatro divisiones: Verdaderos Positivos (VP), Falsos Positivos
#(FP), Falsos Negativos (FN) y Verdaderos Negativos (VN). Siendo los datos verdaderos los que nos
#interesa maximizar (valores de la diagonal).” (Regresión logística, s/f).

#Se presenta el código, junto con el resultado de la matriz de confusión:

# Cálculo de la probabilidad predicha por el modelo con los datos de test
prob.modelo <- predict(object = modelo_logistico_prueba, newdata = test_data, 
                       type = "response")
# Vector de caracteres “0”
pred.modelo <- rep("0", length(prob.modelo))
# Sustitución de “0” por “1” si la probabilidad a posteriori > 0,5
pred.modelo[prob.modelo > 0.5] <- "1"
# Matriz de confusión
table(pred.modelo, test_data$desercion)

#La matriz de confusión nos entrega las predicciones del modelo, junto con los datos reales que
#se encuentran en el conjunto de datos de testeo. Con el resultado de la matriz de confusión podemos
#llegar a las siguientes conclusiones:
# ● Verdaderos Negativos (TN): hay 544 casos donde la clase real es 0 y el modelo también
#predijo 0.
# ● Falsos Positivos (FP): 926 casos donde la clase real es 0, pero el modelo predijo 1.
# ● Falsos Negativos (FN): existen 254 casos donde la clase real es 1, pero el modelo predijo 0.
# ● Verdaderos Positivos (TP): en 607 casos la clase real es 1 y el modelo también predijo 1.

#Continuamos calculando la estimación “test error rate”, lo cual en primera instancia nos dio
#como resultado NA, si se corre esto código:

# Test error rate (resultado=NA)
mean(pred.modelo != test_data$desercion)

#Esto es debido a la cantidad de filas vacías que se encuentran en el conjunto de datos.
#Para solucionarlo, decidimos calcular nuevamente el “test error rate” pero únicamente con las celdas
#que eran distintas de NA. Esto se realizó con el siguiente código:

#Test error rate sin considerar las filas que arrojaron predicciones NA
# Filtrar las filas que no tengan NA en ninguna de las dos columnas
filas_completas <- !is.na(pred.modelo) & !is.na(test_data$desercion)
# Calcular el error solo en las filas completas
error <- mean(pred.modelo[filas_completas] != test_data$desercion[filas_completas])
error

#El resultado que obtuvimos fue de 0.5062205, lo cual quiere decir que la estimación del test
#error rate del modelo mediante validación simple es del 50,62%. Indicando que el modelo de
#regresión logística implementado acierta con sus predicciones en solo un 1 – 0,5062 = 49,38% de los
#casos. Este mismo resultado se puede obtener a través de la siguiente línea de código:

mean(pred.modelo != test_data$desercion, na.rm = TRUE)

#Sin embargo, la estimación del test error rate mediante validación simple es propensa a 
#sufrir alta variabilidad (depende de cómo se hayan distribuido las observaciones en los 
#grupos de entrenamiento y test). A continuación, se muestra el mismo proceso llevado 
#a cabo anteriormente, repitiéndolo 100 veces (en cada iteración los datos se van a repartir
#de manera distinta en entrenamiento y test).

# Vector donde se almacenarán los 100 test error estimados
vector_errores <- rep(NA, 500)
for (i in 1:500){
  # importante la creación de nuevos índices para cada iteración, de lo contrario, 
  #el test error obtenido siempre sería el mismo
  train_index <- sample(x = nrow(datos_seleccionados), size = 0.8*(nrow(datos_seleccionados)), 
                           replace = FALSE)
  train_data <- datos_seleccionados[train_index, ]
  test_data <- datos_seleccionados[-train_index, ]
  modelo_logistico_prueba <- glm(desercion ~ pobreza_multi_5d + 
                                   nse + ytoth + s5 + e5a + s7 + area + 
                                   region, data = datos_seleccionados, 
                                 family = binomial(link = "logit"), 
                                 subset = train_index)
  prob.modelo <- predict(object = modelo_logistico_prueba, newdata = test_data, 
                         type = "response")
  pred.modelo <- rep("0", length(prob.modelo))
  pred.modelo[prob.modelo > 0.5] <- "1"
  vector_errores[i] <- mean(pred.modelo != test_data$desercion, na.rm = TRUE)
}
summary(vector_errores)

#Con esto hemos comprobamos que el error disminuyó, concluyendo que la estimación 
#del test error rate del modelo mediante la iteración de 100 validaciones
#es del 50,81%, lo que indicaría que el modelo acierta con sus predicciones en 
#solo un 1 – 0,5081 = 49,19% de los casos.

#Teniendo en cuenta los errores del modelo, decidimos graficar la distribución de estas 100
#iteraciones que se realizaron previamente, a través de dos tipos de gráficos. En primer lugar con
#boxplot, el cual es una herramienta útil para observar los sesgos de estos errores junto con posibles
#valores atípicos (outliers) que se pueden encontrar. Y en segundo lugar, se realizó un histograma, para
#visualizar la frecuencia de los errores y obtener posibles sesgos o bien, simetría. Al visualizar ambos
#gráficos, podemos concluir que los errores se encuentran concentrados en su mediana, pero también
#existen valores ligeramente dispersos.

#GRAFICANDO LA DISTRIBUCIÓN DE LOS ERRORES
boxplot <- ggplot(data = data.frame(vector_errores = vector_errores), aes(x = 1, 
                                                              y = vector_errores)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(colour = c("orangered2"), width = 0.1) +
  theme(axis.title.x = element_blank(),axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())
boxplot

histograma <- ggplot(data = data.frame(vector_errores = vector_errores), aes(x = vector_errores)) +
  geom_histogram(color = "grey", fill = "orangered2")
histograma

grid.arrange(boxplot, histograma, ncol = 2)

#Para que el modelo predictivo se considere útil, debe acertar con las 
#predicciones en un porcentaje superior a lo esperado por azar o respecto 
#al nivel basal (el que se obtiene si se asignaran a la clase mayoritaria), 
#que en el caso de este set de datos corresponde a “1”. Para
#esto, se utilizó el siguiente código:

prop.table(table(datos_seleccionados$desercion)) 

#Si se predijera deserción = "1" en todos los casos, el modelo alcanzaría aproximadamente un
#65% de aciertos, lo que representa el mínimo porcentaje que debería superar para considerarse útil.
#Sin embargo, en este caso, el modelo no logra superar ese umbral.

#-----------------------------------------------------------------------------------------------------------------------
#PARA PRESENTAR LOS RESULTADOS

#Al tener el modelo de regresión logística completo, comenzamos a trabajar en la forma que
#representaremos los datos. La idea es mostrar tanto los valores reales como los predichos de nuestra
#variable, que en este caso es “desercion”, junto con sus probabilidades. Para esto comenzamos
#creando un nuevo data frame, el cual contiene el id de vivienda del hogar junto con la probabilidad de
#deserción del mismo, lo cual fue realizado a través del siguiente código:

y_test <- test_data$desercion
# Crear un data frame con el código del hogar y la probabilidad de deserción
  resultados_probabilidades <- data.frame(
  id_vivienda = test_data$id_vivienda,
  probabilidad_desercion = prob.modelo
  )
View(resultados_probabilidades[complete.cases(resultados_probabilidades), ])

#Continuamos creando un nuevo data frame con el objetivo de generar una tabla comparativa
#que incluya los valores reales de la variable de interés ("deserción"), las predicciones obtenidas del
#modelo y las probabilidades asociadas a cada predicción. A continuación se muestra la línea de código 
#que arroja el resultado descrito:

#Crear tabla comparativa con los valores reales (y_test) y predichos (pred.modelo)
tabla_comparativa <- data.frame(
  `ID_vivienda` = resultados_probabilidades$id_vivienda,
  `Valor Real (y_test)` = y_test,  # los valores reales de la variable de interés
  `Predicción (y_pred)` = pred.modelo,  # las predicciones convertidas a binario (0 o 1)
  probabilidad_desercion = prob.modelo
)
View(tabla_comparativa[complete.cases(tabla_comparativa), ])

#En la tabla comparativa generada previamente, decidimos agregar una nueva columna
#llamada 'Es correcto', que indicará si la predicción realizada por el modelo de regresión logística fue
#correcta. Esta columna mostrará 'TRUE' si la predicción fue acertada y 'FALSE' en caso contrario. A
#continuación se muestra la línea de código que al ser ejecutada entrega el resultado descrito:

# Añadir una columna que indique si la predicción fue correcta
tabla_comparativa$`Es_correcto` <- tabla_comparativa$`Valor.Real..y_test.` == tabla_comparativa$`Predicción..y_pred.`
View(tabla_comparativa[complete.cases(tabla_comparativa), ])

#Finalmente, decidimos entregar una tabla en la que solo se muestran los resultados en que el
#modelo predice que habrá deserción. A continuación se muestra la línea de código junto con una
#porción del resultado:
#TABLA QUE MUESTRA LOS QUE EL MODELO PREDICE QUE VAN A DESERTAR
View(tabla_comparativa[tabla_comparativa$Predicción..y_pred.=="1", ])

#-----------------------------------------------------------------------------------------------------------------------
#RECOMENDACIONES
#Como equipo, deseamos proponer una serie de recomendaciones derivadas de los hallazgos
#obtenidos en el estudio. Estas sugerencias están enfocadas en maximizar el impacto y la efectividad de
#las acciones basadas en los resultados del análisis. Cada recomendación está formulada para abordar
#aspectos específicos identificados durante el proceso de investigación y para proporcionar orientación
#en la implementación de mejoras estratégicas.

#A continuación, presentamos las recomendaciones clave, cada una respaldada por la evidencia
#y el análisis detallado realizado, con el fin de contribuir de manera significativa a los objetivos
#planteados en este estudio.

#1. Probar el modelo predictivo con otras encuestas CASEN de años anteriores, esto permitirá
#evaluar cómo ha evolucionado la deserción escolar a lo largo del tiempo. Además, sería
#interesante analizar el impacto de la pandemia de COVID-19 en las tasas de deserción escolar
#en Chile.
#2. Probar el modelo predictivo con bases de datos adicionales como lo es la encuesta INJUV
#(Instituto Nacional de la Juventud), esta podría complementar la información sobre factores
#que influyen en la deserción escolar en jóvenes, de esta forma se enriquecería el análisis y se
#mejoraría la capacidad predictiva del modelo, logrando identificar el perfil de los desertores
#en Chile en base a las Encuestas CASEN e INJUV.
#3. Cruzar las bases de datos de distintos años de encuestas CASEN aumentaría el tamaño de la
#muestra y se obtendría una mayor robustez en las predicciones del modelo. De esta manera se
#identificarían tendencias y patrones más precisos sobre la deserción escolar en Chile,
#mejorando la fiabilidad de los resultados.

#-----------------------------------------------------------------------------------------------------------------------
#REFLEXIONES

#La incertidumbre fue un factor presente desde el comienzo del proyecto, especialmente
#cuando no teníamos una visión completamente clara de lo que queríamos lograr. A lo largo del
#proceso, tomamos diversas decisiones, algunas de las cuales tuvieron que ser reformuladas o
#ajustadas. Un ejemplo de esto fue la selección de variables, ya que inicialmente, planeábamos usar
#variables elegidas por nosotras, pero finalmente optamos por aquellas que contaban con respaldo en la
#literatura, lo cual consideramos fundamental.

#Al principio, la falta de experiencia en trabajar con una base de datos tan grande nos generó
#dudas sobre cómo proceder. Al no saber cómo cargarla correctamente, intentamos pasarla de STATA a
#Excel para trabajarla en Python, lo que resultó ser un proceso innecesariamente complicado. Más
#adelante, nos dimos cuenta de que trabajar directamente en R habría sido mucho más eficiente, ya que
#nos habría permitido manejar el archivo sin la necesidad de convertirlo a diferentes formatos como
#Excel o CSV. En esta entrega específica, una de las decisiones más significativas fue migrar de un
#programa a otro, lo que decidimos en conjunto, motivadas por la necesidad de optimizar el proceso y
#cumplir con los plazos. Esta situación nos impulsó a replantear la estrategia y a tomar decisiones
#rápidas, pero siempre bien fundamentadas.

#Cada decisión estuvo basada en los conocimientos adquiridos a lo largo del semestre,
#enriquecidos por la lectura, investigación e implementación. La orientación y el material
#proporcionado por los profesores del curso fueron esenciales, brindándonos el apoyo necesario en
#momentos de incertidumbre. Además, cabe destacar el apoyo e interés constante por nuestros avances
#de alumnos, alumnas y profesoras de la facultad quienes estuvieron siempre pendientes y dispuestos a
#brindarnos consejos y recomendaciones.

#De esta experiencia, aprendimos la importancia de la adaptabilidad y de respaldar nuestras
#decisiones en fuentes confiables, lo cual nos permitió afrontar la incertidumbre con mayor confianza y
#avanzar hacia nuestros objetivos.

#-----------------------------------------------------------------------------------------------------------------------
#AGRADECIMIENTOS
#El agradecimiento de este proyecto va dirijido a los profesores Diego Martinez, Javier Maturana y Claudia Toledo,
#que estuvieron pendientes de nuestros avances e interesados en que siempre fuésemos mas allá.
#Además, agradecemos en demasía a los alumnos de magister de la escuela de Ingeniería Industrial PUCV Luz Céspedes
#y Lukas Chahuán, quienes estuvieron siempre dispuestos a echarnos una mano en el proceso. 
#Por último, agradecernos a nosotras mismas, por aceptar el desafío y buscar siempre la mejor solución
#a los problemas que se nos atravesaban en el camino, trabajando siempre desde la toma de decisiones 
#fundamentadas en base al conocimiento y si no existía claridad de este, se obtenía con perseverancia,
#esfuerzo y compañerismo.
