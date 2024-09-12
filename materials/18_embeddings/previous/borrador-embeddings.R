library(tidyverse)
library(tidytext)
library(tictoc)
library(word2vec)
library(textclean)
library(textstem)

data <- read_csv('state_ofthe_union_texts.csv')

data <- data %>%
  drop_na()

data <- data %>%
  mutate(Text=replace_contraction(Text))

data <- data %>%
  mutate(Text = str_replace_all(Text, "'\\[.*?¿\\]\\%'", " ")) %>%
  mutate(Text = str_replace_all(Text, "[[:punct:]]", " ")) %>%
  mutate(Text = tolower(Text)) %>%
  mutate(Text = str_replace_all(Text, "[[:digit:]]+", "")) %>%
  mutate(Text = replace_non_ascii(Text))


unigramas <- data %>%
  unnest_tokens(word, Text)

data(stop_words)

#stop_words %>%
#  add_row(word=c('hey','ho'), lexicon=c('adhoc','adhoc'))

unigramas <- unigramas %>%
  anti_join(stop_words)

unigramas <- unigramas %>%
  group_by(President, Year, Title) %>%
  summarize(text = str_c(word, collapse = " ")) %>%
  ungroup()

unigramas <- unigramas %>%
  mutate(text_lemma = lemmatize_strings(unigramas$text))

uni_bigramas <- unigramas %>%
  unnest_ngrams(bigram, text, n_min=1, n=2)

uni_bigramas <- uni_bigramas %>%
  mutate(text = str_replace_all(bigram, " ", "_"))

uni_bigramas <- uni_bigramas %>%
  group_by(President, Year, Title) %>%
  summarize(text = str_c(text, collapse = " ")) %>%
  ungroup()


#word2vec_s <- word2vec(x=uni_bigramas$text, # Pasamos la columna con texto
#                       type='skip-gram', # Elegimos el método de ventana
#                        hs=FALSE,
#                        min_count=10, # Ignora palabras cuya frecuencia es menor a esta
#                       window=2, # Fijamos el tamaño de la ventana de contexto
#                       dim=300, # Definimos en cuántas dimensiones queremos el embedding
#                       sample=0.00006, # Umbral para downsamplear palabras muy frecuentes
#                       lr=0.005, #  Tasa de aprendizaje inicial (param. de la red neuronal)
#                       negative=20, # penalidad de palabras poco informaitvas
#                       iter=50, # Iteraciones del modelo
#                       split=c(" \n,.-!?:;/\"#$%&'()*+<=>@[]\\^`{|}~\t\v\f\r",
#                               ".\n?!")
#                       )

#write.word2vec(word2vec_s, 'uni_bigrams_model.bin')

word2vec <- read.word2vec('unigrams_model.bin')
word2vec_big <- read.word2vec('uni_bigrams_model.bin')

predict(word2vec_big, newdata = c("immigrant", "immigrants"), type = "nearest", top_n = 30)
predict(word2vec_big, newdata = c("woman", "women"), type = "nearest", top_n = 30)

predict(word2vec_big, newdata = c("family"), type = "nearest", top_n = 30)


wv <- predict(word2vec_big, newdata = c("america", "latin"), type = "embedding")

predict(word2vec_big, newdata = wv["america", ] - wv["latin", ], type = "nearest", top_n = 10)

