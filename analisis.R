# Importar paquetes
if (!require(tidytext)) {
  install.packages(tidytext)
  library(tidytext)
}
if (!require(tidyverse)) {
  install.packages(tidyverse)
  library(tidyverse)
}
if (!require(patchwork)) {
  install.packages('patchwork')
  library(patchwork)
}
if (!require(wordcloud)) {
  install.packages('wordcloud')
  library(wordcloud)
}

# Leer archivos
temp_df <- data.frame(matrix(nrow=1, ncol=2))
colnames(temp_df) <- c("nombre", "contenido")
archivos <- list.files(path='discursos', pattern="\\.txt$", full.names = T)
cuantos <- length(archivos)
for (i in 1:cuantos) {
  contenido_arch <- read_file(archivos[i])
  nombre_arch <- substr(archivos[i], 11, 15)
  temp_df <- rbind(temp_df, c(nombre_arch, contenido_arch))
}
temp_df <- temp_df[-1, ]

# Discursos de ambos, juntos
petro <- paste(temp_df[temp_df$nombre=="petro",2], collapse=" ")
duque <- paste(temp_df[temp_df$nombre=="duque",2], collapse=" ")

# Stopwords en español
custom_stop_words <- data.frame(palabra=readLines("stopwords-es.txt", warn=F), lexicon="SMART")

# Convertir en tokens todo
df_petro <- tibble(contenido=petro) %>% 
  unnest_tokens(output=palabra, input=contenido) %>% 
  anti_join(custom_stop_words)
df_duque <- tibble(contenido=duque) %>% 
  unnest_tokens(output=palabra, input=contenido) %>% 
  anti_join(custom_stop_words)

# Conteo de palabras
ppal_petro <- df_petro %>% count(palabra) %>% filter(n>10) %>% arrange(desc(n)) 
ppal_duque <- df_duque %>% count(palabra) %>% filter(n>10) %>% arrange(desc(n)) 

p1 <- ppal_petro %>%
  head(20) %>% 
  ggplot(aes(x = reorder(palabra, n),
             y = n)) +
  geom_col() + coord_flip() + labs(title="Petro", x="Palabra", y="Conteo")

p2 <- ppal_duque %>%
  head(20) %>% 
  ggplot(aes(x = reorder(palabra, n),
             y = n)) +
  geom_col() + coord_flip() + labs(title="Duque", x="Palabra", y="Conteo")

p1+p2

# Nube de palabras
wordcloud(words= ppal_petro$palabra,
          freq= ppal_petro$n,
          random.order = F,
          max.words=45)

wordcloud(words= ppal_duque$palabra,
                freq= ppal_duque$n,
                random.order=F,
                max.words=45)


