# R TRADER 

## PROYECTO 1. COMPARA INDICES

Este proyecto descarga los datos de una serie de indices y pinta una gráfica conjunta con la serie acumulada de datos desde una fecha concreta.
Este grafico nos permite ver la evoluciçon relativa de los valores desde una determinada fecha.

Lo pensé para ver la evolución de los indices desde el BREXIT.

## USO
necesitamos poner en un fichero llamado valores.txt la serie de valores, indices o tickers que vamos a pintar.

```
# Ejemplo de valores.txtx

^GSPC,S&P 500
^DJI,Dow Jones Industrial Average
^IXIC,NASDAQ Composite
^NYA,NYSE COMPOSITE (DJ)
^XAX,NYSE AMEX COMPOSITE INDEX
^RUT,Russell 2000
^FTSE,FTSE 100
^GDAXI,DAX
^FCHI,CAC 40

```
hecho esto ejecutamos el fichero de codigo R leedatos.r que continene lo siguiente:
Esto lo podemos hacer con :

source("leedatos.r")

```{r}
Sys.Date()
# Establece el directorio de trabajo
setwd("C:/R/ibex/");

#librerias necesarias
#library("ggplot2", lib.loc="~/R/win-library/3.1");
library("tseries");
library("timeSeries");
library("timeDate");
library("dygraphs", lib.loc="~/R/win-library/3.1")
library("zoo", lib.loc="~/R/win-library/3.3")
library("ggplot2", lib.loc="~/R/win-library/3.3") 

#lee tickers de un fichero y lo guarda en un data frame.
#valoresIbex<-read.table("ibex35.txt",header=TRUE,sep=",")
valores<-read.table("valores.txt",header=FALSE,sep=",");
n <-nrow(valores)
valores;

#funcion para leer los datos de la web de yahoo según el ticker
yahoo.read <- function(ticker,anos,compresion){
  #anos<- 1;años o días de datos desde el día de hoy. si no se multiplica por 365 son dias
  #compresion= puede ser: d=dias w=semanas m=meses
  acc <- get.hist.quote(instrument= ticker, start= Sys.Date() - anos,end= Sys.Date(), quote="AdjClose",provider="yahoo", origin="1970-01-01", compression=as.character(compresion), retclass="zoo");
  acc <- acc[!is.na(acc)]; 
  acc.title = ticker
  #Calculamos la diferencia entre los valores de hoy y de ayer
  difacc <- diff(acc);
  #Calculamos los porcentajes de la diferencia
  dif100 <-100*difacc/acc;
  #calculo la serie acumulada
  acumdif<-cumsum(dif100);
  return(acumdif)
}

#------
anos<-20;  #*365 dias
compre<-as.character("d");

df<- yahoo.read(valores[1,1],anos,compre);
for (i in 2:n) {
  acc<- yahoo.read(valores[i,1],anos,compre);
  names(acc)<-as.character(valores[i,1]);
  df<-merge(df,acc);
}

for (i in 1:n) {
  colnames(df)[i]=as.character(valores[i,1]);
}

#Uso autoplot() de zoo para pintar las grafias en lugar de ggplot2():
#PERO HAY QUE CARGAR LA LIBRERÍA ggplot2
autoplot(df, facets = NULL);

#uso de dygraph para pintar las graficas
dygraph(df, main = "evolución desde el Brexit") %>%
  dyLegend(show = "follow", hideOnMouseOut = TRUE);

#ponemos un listado de los resultados
x<-df[nrow(df),];
x=t(x);
#x[order(x[,1]),];
y<-x[order(x[,1]),];
#t(y);
criterio<- (y>0);
y[criterio];
criterio<- (y<0);
y[criterio];
autoplot(df, facets = NULL)
```
##
