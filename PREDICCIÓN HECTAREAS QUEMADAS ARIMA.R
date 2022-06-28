rm(list = ls())

dato_hctas_quemadas <- read.csv2(file.choose("Datos_hectareas_quemadas.csv"))

hctas_quemadas <- ts(as.numeric(dato_hctas_quemadas[1:62,1]),1961,2021)

# install.packages(c("fpp2", "goftest", "lmtest", "tseries", "TSstudio"))


library(fpp2)
library(tseries)
library(lmtest)
library(TSstudio)


##### FASE 1: Especificacion de la forma funcional ######

auto.arima(hctas_quemadas)

## Primero miramos el gráfico de la serie temporal y los correlogramas simple y parcial

ts_plot(hctas_quemadas, 
        title = "Hectáreas quemadas en España 1961-2021",
        Xtitle = "Año",
        Ytitle = "Hectáreas quemadas",
        width = 2,
        line.mode =  "lines+markers")


ggAcf(hctas_quemadas)

## Test de Dickey Fuller
adf.test(hctas_quemadas)
# No rechaza la hipótesis nula de no estacionariedad


## Test de Philips - Perron
pp.test(hctas_quemadas)
# Rechaza la hipótesis nula de no estacionariedad


## Test de KPSS
kpss.test(hctas_quemadas)
# No rechaza la hipótesis nula de estacionariedad



# Según PP y KPSS parece que la serie temporal está generada por un proceso estocástico estacionario,
# al menos en media aunque no para el ADF. Sin embargo, a simple vista parece que puede ser no estacionario en varianza,
# por lo que procedemos a transformar la serie tomando logaritmos, y repetimos el proceso.

log_hctas_quemadas = log(hctas_quemadas)


## Primero miramos el gráfico de la serie temporal y los correlogramas simple y parcial

ts_plot(log_hctas_quemadas, 
        title = "Logaritmos de Hectáreas quemadas en España 1961-2021",
        Xtitle = "Año",
        Ytitle = "Log de Hectáreas quemadas",
        width = 2,
        line.mode =  "lines+markers")

ggAcf(log_hctas_quemadas)


# El gráfico de la serie temporal muestra ahora una variación más regular. Sin embargo
# la media parece ahora algo cambiante, conque procedemos a realizar de nuevo los contrastes
# de estacionariedad en media:

## Test de Dickey Fuller
adf.test(log_hctas_quemadas)
# No rechaza la hipótesis nula de no estacionariedad


## Test de Philips - Perron
pp.test(log_hctas_quemadas)
# Rechaza la hipótesis nula de no estacionariedad


## Test de KPSS
kpss.test(log_hctas_quemadas)
# No rechaza la hipótesis nula de estacionariedad



auto.arima(log_hctas_quemadas)

# Las conclusiones de los tests son las mismas de antes.
# Como la función auto.arima nos dice que deberíamos tomar diferencias (ARIMA(0,1,1)), y el ADF test
# no nos da evidencias de estacionariedad, probamos a tomar diferencias y repetir el proceso:


diff_log_hctas_quemadas = diff(log(hctas_quemadas))


## Primero miramos el gráfico de la serie temporal y los correlogramas simple y parcial

ts_plot(diff_log_hctas_quemadas, 
        title = "Diferencias de los Logaritmos de Hectáreas quemadas en España 1961-2021",
        Xtitle = "Año",
        Ytitle = "Difs Log de Hectáreas quemadas",
        width = 2,
        line.mode =  "lines+markers")

ggAcf(diff_log_hctas_quemadas)


# El gráfico de la serie temporal muestra ahora una variación regular, así como una media 
# que parece constante a simple vista.

# Por otro lado, a diferencia de las series temporales anteriores, el gráfico de 
# la FAS no parece en ningún momento mostrar valores positivos decrecientes, lo cual
# según el procedimiento de Box Y jenkins podía ser signo de no estacionariedad.

## Test de Dickey Fuller
adf.test(diff_log_hctas_quemadas)
# Rechaza la hipótesis nula de no estacionariedad


## Test de Philips - Perron
pp.test(diff_log_hctas_quemadas)
# Rechaza la hipótesis nula de no estacionariedad


## Test de KPSS
kpss.test(diff_log_hctas_quemadas)
# No rechaza la hipótesis nula de estacionariedad


# Ahora todos los tests parecen concluir que esta serie temporal es estacionaria 
# en media

# A partir de los reultados de los 3 tests, podemos concluir que la serie temporal
# del logaritmo las diferencias está generada por un proceso estocástico estacionario


# Especificación de las estructurar AR y MA:

ggAcf(diff_log_hctas_quemadas)
ggPacf(diff_log_hctas_quemadas)



##### FASE 2A: Estimación de los parámetros. #####

## ESTIMACIÓN ARIMA

# Como formas funcionales tentativas del proceso generador vamos a escoger, según los correlogramas:


fit1 = Arima(log_hctas_quemadas, c(0,1,0))
fit2 = Arima(log_hctas_quemadas, c(1,1,0))
fit3 = Arima(log_hctas_quemadas, c(0,1,1))
fit4 = Arima(log_hctas_quemadas, c(1,1,1))
fit5 = Arima(log_hctas_quemadas, c(2,1,0))
fit6 = Arima(log_hctas_quemadas, c(2,1,1))

summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)
summary(fit5)
summary(fit6)

# Mirando los resultados de las estimaciones, los criterios AIC, BIC y RMSE nos
# indican que el mejor modelo es el ARIMA(0,1,1) sobre la serie temporal de los logaritmos



modelo = Arima(log_hctas_quemadas, c(0,1,1))
summary(modelo)


# Comprobamos si fuera necesaria una constante con su contraste de significatividad:

modelo_const = Arima(log_hctas_quemadas, c(0,1,1), include.constant = TRUE)
summary(modelo_const)
mean_const = modelo_const$coef[2]
sd_const = sqrt(modelo_const$var.coef[2,2])

qnorm(0.025,0,sd_const)
qnorm(0.975,0,sd_const)

# area de aceptacion : (-0.04419994 , +0.04419994)
# el valor estimado de la constante (que es 0.009115365) está dentro de la region de aceptacion,
# con lo cual no podemos rechazar la hipotesis nula de que la constante es 0.
# Además, los criterios de selección no mejoran con la constante.


# Conclusion: no necesitamos constante y el mejor modelo seria el ARIMA(0,1,1) ya que es el que
# nos saca mejores criterios de validacion en computo global. Con lo cual escogemos esta especificacion
# y nos quedamos con sus parametros estimados







##### FASE 2B: Validación, Significatividad y Diagnosis. #####

#IMPORTANTE: estos pasos no siguen este orden necesariamente.


## Validacion:

# hecho en el apartado anterior con RMSE, AIC y BIC.



# Significatividad:

#Por el TCL:

coeftest(modelo)
# Rechazo la hipotesis nula de que el coeficiente theta es 0, es decir, se concluye que
# el parametro theta es significativo.

# Nota: nos podremos fiar de este test si la diagnosis nos ha salido bien.



## Diagnosis:

res = resid(modelo)
checkresiduals(modelo) #Visual y Correlacion



# Autocorrelacion

# Test de Ljung-Box (Hipotesis nula: incorrelación de los residuos)
library(stats)
Box.test(res, lag = 10, type = "Ljung-Box")
# No se rechaza la hipótesis nula de NO autocorrelación de los residuos.




# Normalidad

library("goftest") # goftest de de "goodness of fit"
cvm.test(res,"pnorm",mean(res),sd(res), estimated = TRUE)
ad.test(res,"pnorm",mean(res),sd(res), estimated = TRUE)
# Ninguno de los tests rechaza la hipotesis nula de que los residuos siguen una distribucion Normal




# Media = 0

# Por el TCL:
qnorm(0.975,0,sd(res)/sqrt(length(res)))
qnorm(0.025,0,sd(res)/sqrt(length(res)))
mean(res)
# NO rechazamos la hipotesis nula de que la media del residuo sea 0.

# Análisis de datos atípicos:

summary(res)
sd(res)
2*sd(res)
3*sd(res)

plot(res, ylim = c(-2,2))
lines(x = c(1961,2021), y = c(2*sd(res),2*sd(res)), col = "blue")
lines(x = c(1961,2021), y = c(-2*sd(res),-2*sd(res)), col = "blue")
lines(x = c(1961,2021), y = c(3*sd(res),3*sd(res)), col = "red")
lines(x = c(1961,2021), y = c(-3*sd(res),-3*sd(res)), col = "red")

print((atipicos_2sd = res[res<(-2*sd(res)) | res>(2*sd(res))]))
print((atipicos_3sd = res[res<(-3*sd(res)) | res>(3*sd(res))]))


# Hay tan solo 2 valores que sobresalen por encima del valor absoluto 
# del doble de la desviación típica de los residuos, pero ninguno del triple



# Estabilidad de la varianza (Homocedasticidad/Heterocedasticidad)

fitaux = lm(res^2 ~ 0 + lag(res,1))
k = 1
df = length(res) - k
(bp.statistic <- (length(res) - k) * summary(fitaux)$r.squared)
(bp.pvalue <- pchisq(bp.statistic, k))
# No se rechaza la hipótesis nula de homocedasticidad.


# Datos los resultados de la fase de diagnosis, en principio podemos
# concluir que los residuos son ruido blanco




##### FASE 3: Prediccion. #####

N = 100000000

##ARIMA(0,1,1) (mejores criterios de validación)

# Como nuestra fase de diagnosis ha ido bien, podemos sacar la distribucion asintotica.

res_hat = modelo$residuals
t = length(log_hctas_quemadas)
theta_hat = modelo$coef[1]

# Distribucion en t+1 asintotica:
var_res_hat = modelo$sigma2
sd_res_hat = sqrt(var_res_hat)
mean_log_hctas_quemadas_pred = (log_hctas_quemadas[t] + res_hat[length(res_hat)]*theta_hat)


escenarios_asint = exp(rnorm(N, mean_log_hctas_quemadas_pred, sd_res_hat))



dx_t1 = density(escenarios_asint)
plot(dx_t1, lwd = 2, col = "blue", main = "Predicción de hectáreas quemadas - 2022")

hctas_quemadas[length(hctas_quemadas)]
mean(escenarios_asint)
sd(escenarios_asint)


# Distribucion en t+1 Bootstrap teniendo en cuenta la incertidumbre de estimacion:

Boot=3500
escenarios = c()
bar <- txtProgressBar(0,Boot,style=3)
for (b in 1:Boot){
  res_boot = sample(res_hat, length(res_hat), replace = TRUE)
  
  y_boot = c()
  y_boot[1] = diff_log_hctas_quemadas[1]
  for (i in 2:t){
    y_boot[i] = res_boot[i-1]*theta_hat + res_boot[i]
  }
  
  estim_boot = Arima(y_boot[-1] ,c(0,0,1))
  
  escenarios[b] = exp(log_hctas_quemadas[t] + res_hat[length(res_hat)]*(estim_boot$coef[1]) + 
                        res_boot[length(res_boot)])
  setTxtProgressBar(bar, b)
}
hist(escenarios)


mean(escenarios)
sd(escenarios)


# Comparacion de la asintotica con la bootstrap:
# La línea de función de densidad es la distribucion asintotica y las barras son la distribucion bootstrap.


col2 = rgb(0,0,1, alpha = 0.3)
hist(escenarios, 
     freq = F, 
     main= "Predicción de hectáreas quemadas - 2022", 
     lwd=2, col=0,
     ylim = c(min(density(escenarios_asint)$y), c(max(density(escenarios_asint)$y))))
polygon(density(escenarios_asint), col = col2)


# Escenarios Asintótico:

# hectareas quemadas esperadas
mean(escenarios_asint)

# VaR 99.5%
(VaR_t1 = quantile(escenarios_asint,0.995))

# TVaR 99.5%
(TVaR_t1 = mean(escenarios_asint[escenarios_asint>VaR_t1]))

# IC al 95%
quantile(escenarios_asint,0.975)
quantile(escenarios_asint,0.025)


# Escenarios Bootstrap:

# hectareas quemadas esperadas
mean(escenarios)

# VaR 99.5%
(VaR_t1_B = quantile(escenarios,0.995))

# TVaR 99.5%
(TVaR_t1_B = mean(escenarios[escenarios>VaR_t1_B]))

# IC al 95%
quantile(escenarios,0.975)
quantile(escenarios,0.025)




### Distribucion en t+2 asintotica:

log_hctas_quemadas_2 = c(log_hctas_quemadas, mean_log_hctas_quemadas_pred)
log_hctas_quemadas_t2 = ts(as.numeric(log_hctas_quemadas_2),1961,2022)
diff_log_hctas_quemadas_t2 = diff(log_hctas_quemadas_t2)

modelo_t2 = Arima(log_hctas_quemadas_t2, c(0,1,1))
summary(modelo_t2)

res_hat_t2 = modelo_t2$residuals
t2 = length(log_hctas_quemadas_t2)
theta_hat_t2 = modelo_t2$coef[1]


var_res_hat_2 = var_res_hat*(1+theta_hat^2+theta_hat_t2^2)
sd_res_hat_2 = sqrt(var_res_hat_2)
mean_log_hctas_quemadas_pred_t2 = (log_hctas_quemadas_t2[t2] + res_hat_t2[length(res_hat_t2)]*theta_hat_t2)

escenarios_asint_t2 = exp(rnorm(N, mean_log_hctas_quemadas_pred_t2, sd_res_hat_2))

dx_t2 = density(escenarios_asint_t2)
plot(dx_t2, lwd = 2, col = "red", main = "Predicción de hectáreas quemadas - 2023")


# hectareas quemadas esperadas
mean(escenarios_asint_t2)

# VaR 99.5%
(VaR_t2 = quantile(escenarios_asint_t2,0.995))

# TVaR 99.5%
(TVaR_t2 = mean(escenarios_asint_t2[escenarios_asint_t2>VaR_t2]))

# IC al 95%
quantile(escenarios_asint_t2,0.975)
quantile(escenarios_asint_t2,0.025)


### Distribucion en t+3 asintotica:

log_hctas_quemadas_3 = c(log_hctas_quemadas_2, mean_log_hctas_quemadas_pred_t2)
log_hctas_quemadas_t3 = ts(as.numeric(log_hctas_quemadas_3),1961,2023)
diff_log_hctas_quemadas_t3= diff(log_hctas_quemadas_t3)

modelo_t3 = Arima(log_hctas_quemadas_t3, c(0,1,1))
summary(modelo_t3)

res_hat_t3 = modelo_t3$residuals
t3 = length(log_hctas_quemadas_t3)
theta_hat_t3 = modelo_t3$coef[1]


var_res_hat_3 = var_res_hat*(1+theta_hat^2+theta_hat_t2^2+theta_hat_t3^2)
sd_res_hat_3 = sqrt(var_res_hat_3)
mean_log_hctas_quemadas_pred_t3 = (log_hctas_quemadas_t3[t3] + res_hat_t3[length(res_hat_t3)]*theta_hat_t3)

escenarios_asint_t3 = exp(rnorm(N, mean_log_hctas_quemadas_pred_t3, sd_res_hat_3))

dx_t3 = density(escenarios_asint_t3)
plot(dx_t3, lwd = 2, col = "green", main = "Predicción de hectáreas quemadas - 2024")


# hectareas quemadas esperadas
mean(escenarios_asint_t3)

# VaR 99.5%
(VaR_t3 = quantile(escenarios_asint_t3,0.995))

# TVaR 99.5%
(TVaR_t3 = mean(escenarios_asint_t3[escenarios_asint_t3>VaR_t3]))

# IC al 95%
quantile(escenarios_asint_t3,0.975)
quantile(escenarios_asint_t3,0.025)


# Comparación de las distribuciones de los 3 periodos:

plot(dx_t1, main= "Predicción de hectáreas quemadas", 
     lwd=2, 
     xlim = c(min(dx_t1$x, dx_t2$x, dx_t3$x), c(max(dx_t1$x))),  # Mínimo y máximo limites eje X
     ylim = c(min(dx_t1$y, dx_t2$y, dx_t3$y), c(max(dx_t1$y, dx_t2$y, dx_t3$y))), 
     col = "blue")
lines(dx_t2, lwd = 2, col = "red")
lines(dx_t3, lwd = 2, col = "green")
legend("topright", c("2022", "2023", "2024"), fill = c("blue", "red", "green"))