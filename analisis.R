# Importar paquetes
if (!require(tidytext)) {
  install.packages(tidytext)
  library(tidytext)
}
if (!require(tidyverse)) {
  install.packages(tidyverse)
  library(tidyverse)
}
if (!require(quanteda)) {
  install.packages(quanteda)
  library(quanteda)
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
custom_stop_words <- readLines("stopwords-es.txt", warn=F)

# Convertir en tokens todo
df_petro <- tibble(contenido=petro) %>% 
  unnest_tokens(output=palabra, input=contenido) %>% 
  anti_join(custom_stop_words)
df_duque <- tibble(contenido=duque) %>% 
  unnest_tokens(output=palabra, input=contenido) %>% 
  anti_join(custom_stop_words)

# Conteo de palabras
df_petro %>%
  count(palabra) %>%
  ggplot(aes(x = palabra,
             y = n)) +
  geom_col()
