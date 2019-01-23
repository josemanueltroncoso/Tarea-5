### borrado de variables rm(list = ls())  

##########################################################
######### Iniciando la extracciÃ³n de informaciÃ³n #########
##########################################################

# Usando la librerÃ�a rvest
library('rvest')

# inicializando la variable archivo con el nombre de mi pÃ¡gina
archivo <- 'Tarea5.html'

# Leyendo el HTML del archivo
webpage <- read_html(archivo)

##########################################################
############# ExtracciÃ³n del texto noticia ###############
##########################################################

# Extrayendo contenido en la clase justificado
contenidoWebNoticia <- html_nodes(webpage,'p')

# Pasando la info a texto
textoNoticia <- html_text(contenidoWebNoticia)

# Viendo a priori la info en la variable textoNoticia
print(textoNoticia)

# Pregunta: Â¿QuÃ© representa el \n?

# Eliminando los \n,comillas("),puntos(.) y comas(,) del texto
textoNoticia <- gsub("\n","",textoNoticia)
textoNoticia <- gsub("\"","",textoNoticia)
textoNoticia <- gsub("[.]","",textoNoticia)
textoNoticia <- gsub(",","",textoNoticia)
textoNoticia <- gsub("\t","",textoNoticia)

# Viendo a priori la info en la variable textoNoticia
print(textoNoticia)

# Separando las palabras por espacio
splitEspacioNoticia <- strsplit(textoNoticia," ")[[1]]

# Pasando todas las palabras a minÃºsculas
splitEspacioNoticia <- tolower(splitEspacioNoticia)

# Contando palabras
unlistNoticias<-unlist(splitEspacioNoticia)
tablaPalabras<-table(unlistNoticias)

# Pasando la informaciÃ³n a un data frame
dfPalabrasNoticia <- as.data.frame(tablaPalabras)

# Almacenando la informaciÃ³n en CSV
write.csv(dfPalabrasNoticia, file="PalabrasNoticia.csv")

# o en un txt
write.table(dfPalabrasNoticia, file="PalabrasNoticia.txt")

##########################################################
############ Extraccion informaciÃ³n tabla ################
##########################################################

# Extrayendo los elementos que contienen las tablas
tablaProductos <- html_nodes(webpage, "Deportes Melipilla")

# Extraccio de el contenido de las tablas usando el tag table
contenedorDeTablas <- html_nodes(tablaProductos, "table")

# ExtracciÃ³n informaciÃ³n tabla 1
tabla1 <- html_table(contenedorDeTablas[1][[1]])

# Viendo el contenido de la posiciÃ³n 1,2 de la tabla1
print(tabla1[1,2])

# ExtracciÃ³n informaciÃ³n tabla 2
tabla2 <- html_table(contenedorDeTablas[2][[1]])

# Viendo el contenido de la posiciÃ³n 1,2 de la tabla2
print(tabla2[1,2])

# Limpiando $ comas y cambios de puntos por coma
tabla1$Valor <- gsub("\\$","",tabla1$Valor)
tabla1$Valor <- gsub("[.]","",tabla1$Valor)
tabla1$Valor <- as.numeric(gsub(",",".",tabla1$Valor))

tabla2$Valor <- gsub("\\$","",tabla2$Valor)
tabla2$Valor <- gsub("[.]","",tabla2$Valor)
tabla2$Valor <- as.numeric(gsub(",",".",tabla2$Valor))

# Combinando los dos data frames y creando un tercer data frame
tablaMerge <- rbind(tabla1,tabla2)

# Realizando una busqueda en el dataframe
elementosEncontrados <- tablaMerge[which(tablaMerge$Supermercado == "Unimarc"), ]

# Creando una tercera columna "ProductoSupermercado" con la 
# intenciÃ³n de generando nombres Ãºnicos, esto es para
# graficar el valor de cada producto en cada supermercado
tablaMerge$ProductoSupermercado <- paste(tablaMerge$Producto," ",tablaMerge$Supermercado) 

################### Graficando los productos
library('ggplot2')

# GrÃ¡fico Barra por producto concatenado con supermercado,
# respecto al costo
tablaMerge %>%
  ggplot() +
  aes(x = ProductoSupermercado, y = Valor) +
  geom_bar(stat="identity")

# GrÃ¡fico boxplot diferenciado por producto
tablaMerge %>%
  ggplot() +
  geom_boxplot(aes(x = Producto, y = Valor)) +
  theme_bw()

# Ejercicio guarde la tabla de productos en un CSV o txt
# cuidado con sobreescribir el archivo anterior
