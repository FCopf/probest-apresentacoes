olisal_N <- function(amostra = FALSE, n = 10, seed = 1){
  library(magick)
  library(tidyverse)
  coluna = seq(20, 20+59.5*15, by = 59.5)
  linha = seq(6, 6+24.7*19, by = 24.7)
  positions = expand.grid(coluna, linha)
  N = nrow(positions)
  Lm = 35; Ls = 8
  set.seed(1)
  L = round(rnorm(n = N, mean = Lm, sd = Ls))
  positions = data.frame(obs = 1:N,
                         coluna = positions[,1],
                         linha = positions[,2]) %>% 
    mutate(coord = paste("+", coluna, "+", linha, sep = ''),
           L = L)
  if(amostra){
    set.seed(seed)
    positions = positions[sample(x = N, size = n),]
    N = n
    }
  
  img <- image_read("img/Oligoplites_saliens_multiplo.png")# %>% 
  for (i in 1:N){
    img <- img %>%
      image_annotate(text = positions$L[i], location = positions$coord[i], boxcolor = 'darkred', color ='white', style = 'bold', size = 12)
  }
  if (amostra & n==1){
    am = 'unidade_amostral'
  } else if (amostra & n!=1){
    am = 'amostra'
  } else {
    am = 'populacao'
  }
  file = paste("img/Oligoplites_saliens_multiplo_",am,".png", sep = '')
  image_write(img, path = file, format = "png")

}

olisal_N()
olisal_N(amostra = TRUE, seed = 4)
olisal_N(amostra = TRUE, seed = 4, n = 1)


