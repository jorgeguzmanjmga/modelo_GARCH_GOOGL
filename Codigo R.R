library(fGarch)
library(rugarch)
library(forecast)
library(TSA)
library(MTS)
library(fDMA) 


#Importar base
base<-read.csv("GOOGL.csv")
base<-base[3171:4431,] #información de los ultimos 5 años
base$Date<-as.Date(base$Date)
str(base)
summary(base)


#Se utilizaran los precios de cierre para la serie de tiempo

datos.ts<-ts(base$Close, end=c(2022, 3, 24), frequency = 252)
plot(datos.ts, ylab="Precios de cierre", xlab="Fecha")



par(mfrow=c(1,1))
logdatos<-log(datos.ts)
rend<-diff(logdatos)
plot(rend)


#### Hechos estilizados que podemos observar ####

#No es estacionaria
plot(datos.ts,type="l", col="black", xlab="Fecha", ylab="Precio de cierre",
     main="Precios de cierre acciones de Google") 

#Volatilidad
plot(rend, xlab="Fecha", ylab="Rendimiento", main="Volatilidad")

#Correlación de los rendimentos es pequeña
par(mfrow = c(1,2))
acf(rend, main="ACF Rendimientos",xlab="Retardo")
pacf(rend, main="PACF Rendimientos", xlab="Retardo")

#Los rendimientos cuadrados parecen estar correlacionados
par(mfrow = c(1,1))
DifCuadradas<-rend^2
acf(as.vector(DifCuadradas), main="ACF Cuadrados de Rendimientos", xlab="Retardo")

archtest(as.vector(rend), lag=5) # se rechaza entonces sí hay algún modelo

#### Modelo ####
#Determinamos modelos arma para la serie original (rendimientos)

#Componente arma
par(mfrow=c(1,2))
acf(as.vector(rend),main="ACF  de rendimientos"); 
pacf(as.vector(rend),main="PACF de rendimientos") 
eacf(rend) 
#parece tener el comportamiento de un AR(1) o MA(1)

#Ajustamos el modelo ARMA
modelo<-arima(rend, order=c(0,0,1))
modelo

#Con los residuales tenemos que determinar la componente GARCH
datos.garch<-residuals(modelo)
archtest(as.vector(datos.garch), lag=5) # se rechaza nuevamente

#Determinamos el modelo ARMA(r,q) para los cuadrados de los residuos
par(mfrow=c(1,2))
acf(as.vector(datos.garch^2),main="ACF de residuos al cuadrado"); 
pacf(as.vector(datos.garch^2),main="PACF de residuos al cuadrado") 
eacf(datos.garch^2) 

#Determinamos GARCH(p,q) compatibles con los r,p del ARMA

modelogarch <- ugarchspec(variance.model=list(model = "sGARCH",  garchOrder = c(3, 1), 
                                              submodel = NULL,  external.regressors = NULL,    variance.targeting = FALSE), 
                          mean.model=list(armaOrder = c(0, 1),  external.regressors = NULL),  distribution.model = "norm")
modelogarch

garchfit = ugarchfit(spec = modelogarch, out.sample=5, data = rend)
print(garchfit)


modelogarch <- ugarchspec(variance.model=list(model = "sGARCH",  garchOrder = c(1, 1), 
                                              submodel = NULL,  external.regressors = NULL,    variance.targeting = FALSE), 
                          mean.model=list(armaOrder = c(0, 1),  external.regressors = NULL),  distribution.model = "norm")
modelogarch

garchfit = ugarchfit(spec = modelogarch, out.sample=0, data = rend)
print(garchfit)

#### Pronosticos ####
par(mfrow=c(1,1))
pred<-ugarchforecast(garchfit, n.ahead=5 , n.roll=0, out.sample=0)
#el n.ahead tiene que ser coherente con el out.sample del ajuste del modelo
pred
plot(pred)

head(sigma(pred))
head(fitted(pred))

