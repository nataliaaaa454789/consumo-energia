
# verificar_existencia-numeros.R

# Función que lee los números desde un archivo de texto
leer_numeros <- function(nombre_archivo) {
  if (!file.exists(nombre_archivo)) {
    cat("El archivo no existe.\n")
    stop("No se puede continuar sin el archivo.")
  }

  # Leemos los números y los convertimos en enteros
  numeros <- as.integer(readLines(nombre_archivo))
  return(numeros)
}

# Llamamos a la función para leer el archivo
numeros <- leer_numeros("numeros.txt")

# Cálculo de estadísticas básicas
media_val <- mean(numeros)
mediana_val <- median(numeros)
desv <- sd(numeros)

# Mostramos los resultados por consola
cat("Media:", media_val, "\n")
cat("Mediana:", mediana_val, "\n")
cat("Desviación estándar:", desv, "\n")

# Mensaje si la desviación estándar es muy alta
if (desv > 10) {
  cat("Hay mucha variabilidad en los datos.\n")
}

# Calculamos el cuadrado de cada número con sapply()
cuadrados <- sapply(numeros, function(n) n^2)

# Guardamos todo en un archivo resultados.txt
salida <- "resultados.txt"
con <- file(salida, "w")

writeLines("# Estadísticas calculadas del archivo numeros.txt", con)
writeLines(paste("Media:", media_val), con)
writeLines(paste("Mediana:", mediana_val), con)
writeLines(paste("Desviación estándar:", desv), con)

if (desv > 10) {
  writeLines("Comentario: La desviación estándar es alta", con)
}

writeLines("\n# Cuadrados de los números:", con)
for (i in seq_along(numeros)) {
  linea <- paste(numeros[i], "al cuadrado es", cuadrados[i])
  writeLines(linea, con)
}

close(con)

cat("Archivo 'resultados.txt' creado correctamente.\n")
