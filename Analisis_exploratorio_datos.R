# ANALISIS EXPLORATORIO DE LAS 4 ESTACIONES

# Ruta de los datasets
ruta <- "C:/Users/MAFER/Downloads/Analisis Datasets/Datasets/"

# 1. Cargar los cuatro archivos CSV
est1 <- read.csv(paste0(ruta, "DATA_1_final_annotated.CSV"))
est2 <- read.csv(paste0(ruta, "DATA_2_final_annotated.CSV"))
est3 <- read.csv(paste0(ruta, "DATA_3_final_annotated.CSV"))
est4 <- read.csv(paste0(ruta, "DATA_4_final_annotated.CSV"))

estaciones <- list(est1, est2, est3, est4)

# Carpeta donde guardaremos los resultados
dir.create("resultados_EDA", showWarnings = FALSE)

# 2. RESUMEN GENERAL

resumen <- data.frame(
  Estacion = 1:4,
  Registros = sapply(estaciones, nrow),
  Columnas = sapply(estaciones, ncol),
  Faltantes = sapply(estaciones, function(x) sum(is.na(x))),
  Duplicados = sapply(estaciones, function(x) sum(duplicated(x))),
  Timestamps_repetidos = sapply(estaciones, function(x) {
    sum(duplicated(x$Timestamp))
  })
)

print(resumen)
write.csv(resumen, "resultados_EDA/01_resumen.csv",
          row.names = FALSE)

# 3. FECHAS E INTERVALOS

fechas <- data.frame()

for (i in 1:4) {
  tiempo <- sort(estaciones[[i]]$Timestamp)
  intervalos <- diff(tiempo)
  
  fila <- data.frame(
    Estacion = i,
    Inicio = as.character(as.POSIXct(min(tiempo),
                                     origin = "1970-01-01", tz = "UTC")),
    Fin = as.character(as.POSIXct(max(tiempo),
                                  origin = "1970-01-01", tz = "UTC")),
    Intervalo_mediano = median(intervalos),
    Intervalos_mayores_360s = sum(intervalos > 360)
  )
  
  fechas <- rbind(fechas, fila)
}

print(fechas)
write.csv(fechas, "resultados_EDA/02_fechas.csv",
          row.names = FALSE)

# 4. ESTADISTICAS DESCRIPTIVAS

estadisticas <- data.frame()

for (i in 1:4) {
  for (variable in c("Temperature.C.", "Humidity...")) {
    
    x <- estaciones[[i]][[variable]]
    
    fila <- data.frame(
      Estacion = i,
      Variable = variable,
      Minimo = min(x, na.rm = TRUE),
      Maximo = max(x, na.rm = TRUE),
      Media = mean(x, na.rm = TRUE),
      Mediana = median(x, na.rm = TRUE),
      Desviacion = sd(x, na.rm = TRUE)
    )
    
    estadisticas <- rbind(estadisticas, fila)
  }
}

print(estadisticas)
write.csv(estadisticas, "resultados_EDA/03_estadisticas.csv",
          row.names = FALSE)

# 5. OUTLIERS DE LLUVIA (IQR)

outliers <- data.frame()

for (i in 1:4) {
  x <- estaciones[[i]]$Rain_raw
  
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  rango <- q3 - q1
  
  cantidad <- sum(x < q1 - 1.5 * rango |
                    x > q3 + 1.5 * rango, na.rm = TRUE)
  
  outliers <- rbind(outliers, data.frame(
    Estacion = i,
    Cantidad = cantidad,
    Porcentaje = round(100 * cantidad / sum(!is.na(x)), 2)
  ))
}

print(outliers)
write.csv(outliers, "resultados_EDA/04_outliers.csv",
          row.names = FALSE)

# 6. GRAFICAS

png("resultados_EDA/05_temperatura.png")
boxplot(lapply(estaciones, function(x) x$Temperature.C.),
        names = paste("Estacion", 1:4),
        main = "Temperatura por estacion",
        ylab = "Temperatura (C)",
        col = "lightblue")
dev.off()

png("resultados_EDA/06_humedad.png")
boxplot(lapply(estaciones, function(x) x$Humidity...),
        names = paste("Estacion", 1:4),
        main = "Humedad relativa por estacion",
        ylab = "Humedad (%)",
        col = "lightgreen")
dev.off()

print("Analisis general terminado")
