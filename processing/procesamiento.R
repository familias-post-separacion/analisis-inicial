


# 0. cargar librerias ------------------------------------------------------------
install.packages("pdftools")
pacman::p_load(pdftools, stringr, quanteda, quanteda.textstats, quanteda.textplots, dplyr)

# 1. cargar datos ----------------------------------------------------------------
texto <- pdf_text("input/sentencia.pdf") 
# se tiene un string de 66 elementos en que:
# cada elemento corresponde a una pagina del documento

# 2. revisar páginas (indices) ---------------------------------------------------
texto[12]

# 3. procesar texto --------------------------------------------------------------

## 3.1 pasar a minusculas ---------------------------------------------------------
texto <- str_to_lower(texto)

## 3.2 extraer ciertas palabras ----------------------------------------------------
texto_infidelidad <- str_extract(texto, pattern = "infidelidad")

## 3.3 tokenizar ------------------------------------------------------------------ 

### 3.3.1 creamos el corpus ---------------------------------------------------------
corpus_sentecia <- corpus(unlist(texto))

### 3.3.2 generamos tokens ----------------------------------------------------------
toks_sentencia <-  corpus_sentecia %>% 
  tokens()

# generamos un objeto que contenga las stopwords
stopwords_personalizadas <- c("con", 
                              "de", 
                              "del", 
                              "aquel", 
                              "la", 
                              "el", 
                              "por", 
                              "que", 
                              "su", 
                              "una", 
                              "un",
                              "y",
                              "los", 
                              "a",
                              "se",
                              "(",
                              ")",
                              ".",
                              ",", 
                              ";",
                              ":",
                              "/")

# matriz DFM 

# generamos matriz DFM para los tokens de las canciones de 1960
dfm_sentencia <- toks_sentencia %>%
  tokens_select(pattern = stopwords_personalizadas, selection = "remove") %>% # remover stopwords
  dfm()  


dfm_sentencia <- toks_sentencia %>%
  dfm()  

tfidf_sentencia <- dfm_sentencia %>% 
  dfm_tfidf()

#similitud coseno
distancia_sentencia <- textstat_simil(tfidf_sentencia, method = "cosine")
# se calcua la similitud coseno de las tres palabra más similares a la fila 7
sort(distancia_sentencia[7, ], decreasing = T)[2:4]

# 4. analisis -----------------------------------------------------------------
# 7 palabras anterioroes y posteriores de infidelidad
#t <- toks_sentencia %>% kwic(pattern = "infidelidad", window = 7)


## 4.1 explorar palabras ------------------------------------------------------

# se almacena en el objeto keywords las palabras que nos interesen
keywords <- c("amor", "quiero", "infidelidad")

# inicializamos la lista para 1960
contexto_sentencia <- list()


# iteramos el cálculo de las 7 palabras anteriores y las 7 palabras posteriores a cada tópico de 1960
for (i in keywords) {
  contexto_sentencia[[i]] <- toks_sentencia %>% kwic(pattern = i, window = 7)
}


# se genera la funcion generar tabla
generar_tabla <- function(contexto_sentencia_list, i) { 
  
  # extraimos el tópico correspondiente 
  letra <- contexto_sentencia_list[[i]] 
  
  # se convierte en data frame y se seleccionan las tres columnas a visualizar en la tabla
  letra <- as.data.frame(letra) %>% select(pre, keyword, post)
  
  # creamos objeto tabla con su formato personalizado 
  tabla <- letra %>% knitr::kable(format = "html",
                                  aling = "c",
                                  col.names = c("Oración anterior", "Palabra", "Oración posterior")) %>% # agregamos título de columnas 
    kableExtra::kable_minimal() %>% 
    kableExtra::row_spec(0, background = "lightpink") %>%
    kableExtra::column_spec(1, background = "mistyrose")  %>%
    kableExtra::column_spec(2, background = "mistyrose", bold = TRUE )  %>%
    kableExtra::column_spec(3, background = "mistyrose")
  
  
  # retorna la tabla 
  return(tabla)
}

generar_tabla(contexto_sentencia, "infidelidad")
