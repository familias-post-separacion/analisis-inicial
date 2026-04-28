library(word2vec)

# Tu vector de 66 páginas
# mi_lista_66 <- c("texto página 1...", "texto página 2...", ...)

x <- texto  # cada elemento = texto completo de una página

# Con páginas completas ya tienes suficiente texto para parámetros más robustos
model <- word2vec(
  x         = x,
  dim       = 100,       # 100 es razonable para un documento de 66 páginas
  iter      = 30,        # suficiente para este tamaño
  min_count = 2,         # palabras que aparecen al menos 2 veces en todo el doc
  threads   = 4
)

# Guardar
write.word2vec(model, "modelo_documento.bin")

# Matriz de embeddings
embedding <- as.matrix(model)

# Verificar vocabulario resultante
cat("Palabras en vocabulario:", nrow(embedding), "\n")


# Ver qué palabras quedaron en el vocabulario
vocab <- rownames(embedding)
head(vocab, 40)  # revisa esto primero para elegir palabras relevantes

# Buscar palabras cercanas con términos de TU documento
lookslike <- predict(
  model,
  newdata = c("infidelidad", "matrimonio", "familia", "cónyuge"),  # reemplaza con términos reales
  type    = "nearest",
  top_n   = 5
)

lookslike



# ✅ Generar nombres de dimensiones manualmente
var_names <- paste0("V", 1:ncol(embedding))  # genera "V1", "V2", ..., "V100"

vectores_paginas <- map(texto, function(pagina) {
  palabras <- unlist(strsplit(tolower(pagina), "\\W+"))
  palabras <- palabras[nchar(palabras) > 0]
  
  vecs <- map(palabras, function(p) {
    tryCatch(predict(model, p, type = "embedding"), error = function(e) NULL)
  }) %>%
    discard(is.null) %>%
    discard(function(x) {
      v <- as.numeric(x)                    # convertir matriz → vector numérico
      any(is.na(v)) || sum(v) == 0          # descartar si tiene NA o es vector cero
    }) %>%
    map(as.numeric)                         # asegurar que cada elemento sea vector
  
  if (length(vecs) == 0) return(rep(0, 100))
  
  # Promedio de vectores = representación de la página
  Reduce("+", vecs) / length(vecs)
})

encontrar_paginas <- function(consulta, n = 3) {
  vec_consulta <- tryCatch(
    predict(model, unlist(strsplit(tolower(consulta), "\\W+")), type = "embedding"),
    error = function(e) NULL
  )
  if (is.null(vec_consulta)) {
    message("Ninguna palabra de la consulta está en el vocabulario")
    return(NULL)
  }
  vec_consulta <- colMeans(vec_consulta)
  
  similitudes <- map_dbl(vectores_paginas, ~coop::cosine(vec_consulta, .x))
  top <- order(similitudes, decreasing = TRUE)[1:n]
  
  # Mostrar número de página y similitud
  cat("Páginas más relevantes:\n")
  for (i in seq_along(top)) {
    cat(sprintf("  Página %d — similitud: %.3f\n", top[i], similitudes[top[i]]))
  }
  
  return(invisible(texto[top]))
}

#
# Buscar las 3 páginas más relevantes para una consulta
encontrar_paginas("infidelidad", n = 3)
