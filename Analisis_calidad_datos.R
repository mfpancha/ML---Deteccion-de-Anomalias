# ANALISIS DE CALIDAD DE LAS 4 ESTACIONES

# Ruta de los datasets
ruta <- "C:/Users/MAFER/Downloads/Analisis Datasets/Datasets/"

# 1. Cargar los cuatro archivos CSV
est1 <- read.csv(paste0(ruta, "DATA_1_final_annotated.CSV"))
est2 <- read.csv(paste0(ruta, "DATA_2_final_annotated.CSV"))
est3 <- read.csv(paste0(ruta, "DATA_3_final_annotated.CSV"))
est4 <- read.csv(paste0(ruta, "DATA_4_final_annotated.CSV"))

estaciones <- list(est1, est2, est3, est4)

dir.create("resultados_EDA", showWarnings = FALSE)

# Variables ambientales que comparten las estaciones
variables <- c("Temperature.C.", "Humidity...",
               "Rain_raw", "Light_raw", "UV_raw",
               "soil_moist_raw")

# 2. OUTLIERS POR VARIABLE

resultados <- data.frame()

for (i in 1:4) {
  for (variable in variables) {
    
    x <- estaciones[[i]][[variable]]
    
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    q3 <- quantile(x, 0.75, na.rm = TRUE)
    rango <- q3 - q1
    
    cantidad <- sum(x < q1 - 1.5 * rango |
                      x > q3 + 1.5 * rango, na.rm = TRUE)
    
    resultados <- rbind(resultados, data.frame(
      Estacion = i,
      Variable = variable,
      Outliers = cantidad
    ))
  }
}

print(resultados)
write.csv(resultados, "resultados_EDA/07_outliers_variables.csv",
          row.names = FALSE)

# 3. SECUENCIAS CONSTANTES
# Buscamos secuencias de 10 o mas lecturas iguales

constantes <- data.frame()

for (i in 1:4) {
  for (variable in variables) {
    
    x <- estaciones[[i]][[variable]]
    secuencias <- rle(x)$lengths
    
    constantes <- rbind(constantes, data.frame(
      Estacion = i,
      Variable = variable,
      Secuencias_10 = sum(secuencias >= 10),
      Maxima_secuencia = max(secuencias)
    ))
  }
}

print(constantes)
write.csv(constantes, "resultados_EDA/08_constantes.csv",
          row.names = FALSE)

# 4. CAMBIOS ABRUPTOS
# Revisamos diferencias superiores al percentil 99

cambios <- data.frame()

for (i in 1:4) {
  for (variable in variables) {
    
    datos <- estaciones[[i]]
    datos <- datos[order(datos$Timestamp), ]
    
    diferencias <- abs(diff(datos[[variable]]))
    limite <- quantile(diferencias, 0.99, na.rm = TRUE)
    
    cambios <- rbind(cambios, data.frame(
      Estacion = i,
      Variable = variable,
      Limite = as.numeric(limite),
      Cambios_grandes = sum(diferencias > limite,
                            na.rm = TRUE)
    ))
  }
}

print(cambios)
write.csv(cambios, "resultados_EDA/09_cambios.csv",
          row.names = FALSE)

# 5. VALORES FUERA DE 0 A 100 %

humedad_suelo <- est1$soil_moist_percent...

fuera_rango <- sum(humedad_suelo < 0 |
                     humedad_suelo > 100, na.rm = TRUE)

print(paste("Humedad de suelo fuera de rango:",
            fuera_rango))

write.csv(
  data.frame(Estacion = 1, Fuera_rango = fuera_rango),
  "resultados_EDA/10_rangos.csv",
  row.names = FALSE
)

# 6. COMPARAR EL MISMO PERIODO

inicio <- max(sapply(estaciones, function(x) min(x$Timestamp)))
fin <- min(sapply(estaciones, function(x) max(x$Timestamp)))

comunes <- lapply(estaciones, function(x) {
  x[x$Timestamp >= inicio & x$Timestamp <= fin, ]
})

png("resultados_EDA/11_temperatura_comun.png")
boxplot(lapply(comunes, function(x) x$Temperature.C.),
        names = paste("Estacion", 1:4),
        main = "Temperatura en periodo comun",
        ylab = "Temperatura (C)",
        col = "lightblue")
dev.off()

png("resultados_EDA/12_humedad_comun.png")
boxplot(lapply(comunes, function(x) x$Humidity...),
        names = paste("Estacion", 1:4),
        main = "Humedad en periodo comun",
        ylab = "Humedad (%)",
        col = "lightgreen")
dev.off()

print("Analisis complementario terminado")