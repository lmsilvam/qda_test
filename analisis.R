# Importar paquetes
paquetes <- c("tidytext", "tidyverse", "patchwork", "wordcloud", "stopwords")

paquetes_instalados <- paquetes %in% rownames(installed.packages())
if (any(paquetes_instalados == FALSE)) {
  install.packages(paquetes[!paquetes_instalados])
}

invisible(lapply(paquetes, library, character.only = TRUE))

# Leer archivos
archivos <- list.files(path='discursos', pattern="\\.txt$", full.names = T)
contenido <- lapply(archivos, read_file)
autor <- lapply(archivos, function(x) str_extract(x, "/[a-z]*"))
datos <- data.frame(cbind(autor, contenido))

# Stopwords custom 
custom_stopwords <- data.frame(palabra= readLines('stopwords-es.txt', warn=F))

# Discursos de ambos, juntos
discursos <- datos %>% 
  group_by(autor) %>% 
  summarise(cont= paste(contenido, collapse='')) %>% 
  unnest_tokens(output=palabra, input=cont) %>% 
  select(autor, palabra) %>% 
  anti_join(custom_stopwords)

# Conteo de palabras
ppal_petro <- discursos %>% 
  filter(autor=="/petro") %>% 
  count(palabra) %>% 
  filter(n > 10) %>% 
  arrange(desc(n)) 

ppal_duque <- discursos %>% 
  filter(autor=="/duque") %>% 
  count(palabra) %>% 
  filter(n > 10) %>% 
  arrange(desc(n)) 

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


