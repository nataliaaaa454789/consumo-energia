
# Proyecto energia - Analisis consumo y costo

# Paso 1: Crear vectores
energia <- c(rep("Renovable", 10), rep("No Renovable", 10))
consumo <- c(45, 50, 48, NA, 53, 46, 49, NA, 51, 47,
             60, 65, NA, 70, 68, 63, 66, 64, NA, 67)
costo_kwh <- c(rep(0.15, 10), rep(0.25, 10))

# Paso 2: reemplazar NA por la mediana segun tipo energia
for (tipo in unique(energia)) {
  pos <- which(energia == tipo)
  sub_consumo <- consumo[pos]
  med <- median(sub_consumo, na.rm=TRUE)
  consumo[pos][is.na(sub_consumo)] <- med
  consumo[pos] <- consumo[pos]  # se reasigna por si acaso
}

# Paso 3: Dataframe
df_consumo <- data.frame(energia, consumo, costo_kwh)

# Paso 4: Calculos
df_consumo$costo_total <- df_consumo$consumo * df_consumo$costo_kwh

# suma total por tipo
total_consumo <- tapply(df_consumo$consumo, df_consumo$energia, sum)
total_costo <- tapply(df_consumo$costo_total, df_consumo$energia, sum)
media_consumo <- tapply(df_consumo$consumo, df_consumo$energia, mean)

# simular ganancia con 10% mas
df_consumo$ganancia <- df_consumo$costo_total * 1.1

# Paso 5: resumen
df_ordenado <- df_consumo[order(df_consumo$costo_total, decreasing=TRUE), ]
top_3_costos <- df_ordenado[1:3, ]

resumen_energia <- list(
  ordenado = df_ordenado,
  consumo_total = total_consumo,
  costos_totales = total_costo,
  top3 = top_3_costos
)

# print para ver el resumen
print(resumen_energia)
