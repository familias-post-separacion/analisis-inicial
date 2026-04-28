# PIPELINE USANDO LDA


library(tidytext)
library(topicmodels)
library(ldatuning)
library(dplyr)
library(tidyr)
library(purrr)

# Dividir cada página en frases
frases <- map2_dfr(texto, seq_along(texto), function(pagina, num_pagina) {
  fs <- unlist(strsplit(pagina, "(?<=[.!?])\\s+", perl = TRUE))
  fs <- fs[nchar(trimws(fs)) > 20]  # descartar frases muy cortas
  tibble(pagina = num_pagina, frase_id = seq_along(fs), texto = fs)
})

cat("Total de frases extraídas:", nrow(frases), "\n")


# Tokenizar y eliminar stopwords en español
data("stop_words")  # stopwords en inglés — complementar con español abajo

stopwords_es <- c("de", "la", "el", "en", "y", "a", "los", "las", "un", "una",
                  "que", "se", "del", "por", "con", "no", "es", "su", "al",
                  "lo", "como", "más", "pero", "sus", "le", "ya", "o", "fue",
                  "este", "ha", "si", "porque", "esta", "son", "entre", "cuando",
                  "muy", "sin", "sobre", "también", "me", "hasta", "hay", "donde",
                  "quien", "desde", "todo", "nos", "durante", "uno", "ni", "contra")

dtm <- frases %>%
  unnest_tokens(palabra, texto) %>%
  filter(!palabra %in% stopwords_es) %>%
  filter(nchar(palabra) > 3) %>%          # descartar palabras muy cortas
  count(frase_id, palabra) %>%
  cast_dtm(frase_id, palabra, n)

cat("Dimensiones DTM:", dim(dtm), "\n")  # frases x palabras


library(topicmodels)

# Probar distintos valores de k
valores_k <- 2:10
perplejidad <- map_dbl(valores_k, function(k) {
  modelo <- LDA(dtm, k = k, method = "Gibbs",
                control = list(seed = 42, iter = 500, burnin = 100))
  perplexity(modelo, newdata = dtm)  # <-- agregar newdata = dtm
})

# Ver resultados
resultados_k <- tibble(k = valores_k, perplejidad = perplejidad)
print(resultados_k)

# Graficar
plot(valores_k, perplejidad, type = "b", pch = 19,
     xlab = "Número de tópicos (k)",
     ylab = "Perplejidad",
     main = "Selección de k óptimo")

# Graficar — busca el "codo" donde la perplejidad deja de bajar mucho
plot(valores_k, perplejidad, type = "b", pch = 19,
     xlab = "Número de tópicos (k)",
     ylab = "Perplejidad",
     main = "Selección de k óptimo")


# Reemplaza k con el número que sugirió el gráfico
k <- 5  # <-- ajusta según el gráfico

modelo_lda <- LDA(
  dtm,
  k       = k,
  method  = "Gibbs",
  control = list(seed = 42, iter = 1000, burnin = 100)
)


# Top 10 palabras por tópico
palabras_topicos <- tidy(modelo_lda, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Imprimir
palabras_topicos %>%
  group_by(topic) %>%
  summarise(palabras = paste(term, collapse = ", ")) %>%
  print()


# Tópico dominante por frase
topicos_frases <- tidy(modelo_lda, matrix = "gamma") %>%
  group_by(document) %>%
  slice_max(gamma, n = 1) %>%
  ungroup() %>%
  mutate(frase_id = as.integer(document)) %>%
  left_join(frases, by = "frase_id") %>%
  select(pagina, frase_id, topico = topic, probabilidad = gamma, texto)

# Tópico dominante por página
topicos_paginas <- topicos_frases %>%
  group_by(pagina, topico) %>%
  summarise(n_frases = n(), .groups = "drop") %>%
  group_by(pagina) %>%
  slice_max(n_frases, n = 1) %>%
  ungroup()

print(topicos_paginas)
# GRAFIO

library(ggplot2)

# Top 10 palabras por tópico ordenadas por probabilidad
palabras_topicos %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(x = beta, y = term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y", labeller = label_both) +
  scale_y_reordered() +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title    = "Palabras más relevantes por tópico",
    subtitle = "Ordenadas por probabilidad beta",
    x        = "Probabilidad (beta)",
    y        = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )


## GRAFICO 2 ---------------------------------
library(ggplot2)
library(ggrepel)  # install.packages("ggrepel") si no lo tienes
library(tibble)
# Paso 1: extraer matriz beta (palabras x tópicos)
beta_matrix <- modelo_lda %>%
  tidy(matrix = "beta") %>%
  pivot_wider(names_from = topic, values_from = beta, values_fill = 0) %>%
  column_to_rownames("term")

# Paso 2: reducir dimensiones con PCA
pca <- prcomp(beta_matrix, scale. = TRUE)
coords_palabras <- as.data.frame(pca$x[, 1:2])
coords_palabras$palabra <- rownames(coords_palabras)

# Paso 3: coordenadas de los tópicos (centroide de sus top palabras)
top_palabras <- tidy(modelo_lda, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  pull(term)

coords_topicos <- coords_palabras %>%
  filter(palabra %in% top_palabras) %>%
  left_join(
    tidy(modelo_lda, matrix = "beta") %>%
      group_by(topic) %>%
      slice_max(beta, n = 10) %>%
      select(palabra = term, topic),
    by = "palabra"
  ) %>%
  group_by(topic) %>%
  summarise(PC1 = mean(PC1), PC2 = mean(PC2)) %>%
  mutate(label = paste0("Tópico ", topic))

# Paso 4: graficar
ggplot() +
  # palabras como puntos pequeños
  geom_point(data = coords_palabras %>% filter(palabra %in% top_palabras),
             aes(x = PC1, y = PC2), color = "gray70", size = 1.5) +
  geom_text_repel(data = coords_palabras %>% filter(palabra %in% top_palabras),
                  aes(x = PC1, y = PC2, label = palabra),
                  size = 3, color = "gray40", max.overlaps = 20) +
  # tópicos como puntos grandes
  geom_point(data = coords_topicos,
             aes(x = PC1, y = PC2, color = factor(topic)), size = 8) +
  geom_text(data = coords_topicos,
            aes(x = PC1, y = PC2, label = label),
            size = 3.5, fontface = "bold", color = "white") +
  scale_color_brewer(palette = "Set2") +
  labs(
    title    = "Mapa de tópicos y palabras",
    subtitle = "Reducción PCA — tópicos como centroides de sus palabras clave",
    x        = "Dimensión 1",
    y        = "Dimensión 2",
    color    = "Tópico"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")
