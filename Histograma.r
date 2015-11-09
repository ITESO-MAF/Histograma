# ---------------------------------------------------------------------------------------------------- #
# ITESO  - Ingeniería Financiera                                                                       #
# Código - Gráfica tipo Histograma                                                                     #
# Versión inicial - FranciscoME                                                                        #
# ---------------------------------------------------------------------------------------------------- #

# Cargar librerías a utilizar

library  (ggplot2)
library  (gridExtra)
library  (plyr)
library  (moments)

# Etiquetas y colores genéricos

color1 <- "white"
color2 <- "light gray"
color3 <- "#0066cc"
color4 <- "#333333"
color5 <- "steel blue"
color6 <- "turquoise"
color7 <- "#cc0000"
color8 <- "dark grey"
color9 <- "dark blue"
colorA <- "#000066"
colorB <- "steel blue"
titulo <- "Normalidad de Errores"
titulo <- "Título de gráfica"
ejex   <- "Eje x"
ejey   <- "Eje y"
linea  <- "longdash"

# Generación de datos aleatorios para muestra y cálculo de estadísticas básicas

dfDatos <- data.frame(rnorm(1000,0,1))
mediaDatos  <- signif(mean(dfDatos[,1]),2)
dsDatos     <- signif(sd(dfDatos[,1]),2)
mediaDatos  <- signif(mean(dfDatos[,1]),2)
sesDatos    <- signif(skewness(dfDatos[,1]),2)
curtDatos   <- signif(kurtosis(dfDatos[,1]),2)

# Tamaños de elementos

tamEjex  <- 12                                               # Tamaño de valores/letras en Eje X
tamEjey  <- 12                                               # Tamaño de valores/letras en Eje Y
tamEtiq  <- 5                                                # Tamaño de etiquetas horizontales  
anchoClase <- 0.25                                           # Valor de ancho de clase para histograma 
tamTitulo  <- 14                                             # Tamaño de letra del título del gráfico
tamTituloX <- 14                                             # Tamaño de letra del título del eje X
tamTituloY <- 14                                             # Tamaño de letra del título del eje Y
anchoLinea <- .75                                            # Ancho de las líneas verticales


# Inicia código para histograma 
# Cargar datos, declarar que será histograma, colocar títulos de ejes y de gráfico.

ggHistograma  <- ggplot(dfDatos,aes(dfDatos[,1])) +                        # Datos de entrada y eje variable
                 geom_histogram(aes(fill = ..count..,y = ..density..),     # Relleno según cantidad de datos
                 binwidth=0.25) + labs(title = titulo,y = ejey,x = ejex) + # Ancho de clase y títulos
                 scale_fill_continuous(low = colorA, high = colorB, guide = "none")

# Tema para el fondo, colores de escalas, borde de gráfica.

ggHistograma1 <- ggHistograma +  theme(panel.background = element_rect(fill = color1),
                 panel.grid.minor.y = element_line(size = .2, color = color2),
                 panel.grid.major.y = element_line(size = .2, color = color2),
                 panel.grid.minor.x = element_line(size = .2, color = color2),
                 panel.grid.major.x = element_line(size = .2, color = color2),
                 axis.text.x  = element_text(colour = color4,size = tamEjex, hjust =.5,vjust = 0),
                 axis.text.y  = element_text(colour = color4,size = tamEjey, hjust =.5,vjust = 0),
                 axis.title.x = element_text(colour = color4,size = tamTituloX,hjust =.5,vjust = 0),
                 axis.title.y = element_text(colour = color4,size = tamTituloY,hjust =.5,vjust = 1),
                 title=element_text(colour = colorB, size = tamTitulo, hjust = 1, vjust = 0.8),
                 panel.border = element_rect(linetype = 1, colour = color8, fill = NA))

# Función de densidad de probabilidad normal con media y desviación estándar igual a la de los datos

ggHistograma2 <- ggHistograma1  + stat_function(fun=dnorm,args=list(mediaDatos, dsDatos),
                  geom = "area", alpha = 0.35, color = "grey", fill = "black")

# Extraer del objeto construido el valor máximo de probabilidad 

ggInfoHist  <- ggplot_build(ggHistograma)$data[[1]]
yMax        <- (max(ggInfoHist$density))*1.1
  
# Incluir las líneas verticales que indican las desviaciones estándar para los datos.

ggHistograma3 <- ggHistograma2 +
geom_segment(aes(x=mediaDatos,xend=mediaDatos,yend=yMax,y=0),colour=color8,linetype=linea,size=anchoLinea) +
geom_segment(aes(x=-6*dsDatos,xend=-6*dsDatos,yend=yMax,y=0),colour=colorB,linetype=linea,size=anchoLinea) +
geom_segment(aes(x=+6*dsDatos,xend=+6*dsDatos,yend=yMax,y=0),colour=colorB,linetype=linea,size=anchoLinea) +
geom_segment(aes(x=-5*dsDatos,xend=-5*dsDatos,yend=yMax,y=0),colour=colorB,linetype=linea,size=anchoLinea) +
geom_segment(aes(x=+5*dsDatos,xend=+5*dsDatos,yend=yMax,y=0),colour=colorB,linetype=linea,size=anchoLinea) +
geom_segment(aes(x=-4*dsDatos,xend=-4*dsDatos,yend=yMax,y=0),colour=colorB,linetype=linea,size=anchoLinea) +
geom_segment(aes(x=+4*dsDatos,xend=+4*dsDatos,yend=yMax,y=0),colour=colorB,linetype=linea,size=anchoLinea) +
geom_segment(aes(x=-3*dsDatos,xend=-3*dsDatos,yend=yMax,y=0),colour=colorB,linetype=linea,size=anchoLinea) +
geom_segment(aes(x=+3*dsDatos,xend=+3*dsDatos,yend=yMax,y=0),colour=colorB,linetype=linea,size=anchoLinea) +
geom_segment(aes(x=-2*dsDatos,xend=-2*dsDatos,yend=yMax,y=0),colour=colorB,linetype=linea,size=anchoLinea) +
geom_segment(aes(x=+2*dsDatos,xend=+2*dsDatos,yend=yMax,y=0),colour=colorB,linetype=linea,size=anchoLinea) + 
geom_segment(aes(x=-1*dsDatos,xend=-1*dsDatos,yend=yMax,y=0),colour=colorB,linetype=linea,size=anchoLinea) + 
geom_segment(aes(x=+1*dsDatos,xend=+1*dsDatos,yend=yMax,y=0),colour=colorB,linetype=linea,size=anchoLinea)

# Incluir las etiquetas de las líneas verticales y ajustar la escala vertical y horizontal

ggHistograma4 <- ggHistograma3 +
annotate("text",x=mediaDatos,y=-.015,parse=TRUE,label="-mu",     colour=colorA,size=tamEtiq) +
annotate("text",x=-1*dsDatos,y=-.015,parse=TRUE,label="-sigma",  colour=colorA,size=tamEtiq) +
annotate("text",x=+1*dsDatos,y=-.015,parse=TRUE,label="sigma",   colour=colorA,size=tamEtiq) +
annotate("text",x=-2*dsDatos,y=-.015,parse=TRUE,label="-2*sigma",colour=colorA,size=tamEtiq) +
annotate("text",x=+2*dsDatos,y=-.015,parse=TRUE,label="2*sigma", colour=colorA,size=tamEtiq) +
annotate("text",x=-3*dsDatos,y=-.015,parse=TRUE,label="-3*sigma",colour=colorA,size=tamEtiq) +
annotate("text",x=+3*dsDatos,y=-.015,parse=TRUE,label="3*sigma", colour=colorA,size=tamEtiq) +
annotate("text",x=-4*dsDatos,y=-.015,parse=TRUE,label="-4*sigma",colour=colorA,size=tamEtiq) +
annotate("text",x=+4*dsDatos,y=-.015,parse=TRUE,label="4*sigma", colour=colorA,size=tamEtiq) +
annotate("text",x=-5*dsDatos,y=-.015,parse=TRUE,label="-5*sigma",colour=colorA,size=tamEtiq) +
annotate("text",x=+5*dsDatos,y=-.015,parse=TRUE,label="5*sigma", colour=colorA,size=tamEtiq) +
annotate("text",x=-6*dsDatos,y=-.015,parse=TRUE,label="-6*sigma",colour=colorA,size=tamEtiq) +
annotate("text",x=+6*dsDatos,y=-.015,parse=TRUE,label="6*sigma", colour=colorA,size=tamEtiq) +
scale_y_continuous(breaks=seq(0,yMax,round(yMax/11,2)))                                      +  
scale_x_continuous(breaks=round(seq(-6*dsDatos,6*dsDatos,dsDatos),2))                        

# Cálculo de momentos estadísticos y almacenamiento de estos en un data.frame

sesDatos  <- signif(skewness(dfDatos[,1]),2)
curtDatos <- signif(kurtosis(dfDatos[,1]),2)
Medidas   <- c("Media","DesvEst","Sesgo","Kurtosis")
Valor     <- c(mediaDatos, dsDatos, sesDatos, curtDatos)
df.est    <- data.frame(Medidas,Valor)

# Indicación de coordenadas y colocación del cuadro con momentos estadísticos

cajaMaxY <- yMax
cajaMinY <- signif((cajaMaxY*.60),2)
cajaMinX <- signif(3*dsDatos,2)
cajaMaxX <- Inf
ggHistograma5 <- ggHistograma4 + annotation_custom (grob = tableGrob((df.est[,1:2]),
show.rownames = FALSE), xmin = cajaMinX, xmax = cajaMaxX,ymin = cajaMinY, ymax = cajaMaxY)

ggHistograma5
