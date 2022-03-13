B = 1000
set.seed(2); p1 <- runif(n = B, min = 20, max = 80)
set.seed(2); p2 <- rnorm(n = B, mean = 20, sd = 10)
set.seed(2); p3 <- rexp(n = B, rate = 1)

n <- c(2, 5, 30)
nrep <- rep(n, each = 3)
tcl <- function(pop, n){
  Ym <- data.frame(
    Ym = apply(MARGIN = 2, FUN = mean,
               replicate(n = B,
                         sample(pop, size = n, replace = F))
    )
  )
  return(Ym)
}

YM <- list(
  p1 = data.frame(Ym = p1),
  p2 = data.frame(Ym = p2),
  p3 = data.frame(Ym = p3),
  
  n1_p1 = tcl(p1, n[1]),
  n1_p2 = tcl(p2, n[1]),
  n1_p3 = tcl(p3, n[1]),
  
  n2_p1 = tcl(p1, n[2]),
  n2_p2 = tcl(p2, n[2]),
  n2_p3 = tcl(p3, n[2]),
  
  n3_p1 = tcl(p1, n[3]),
  n3_p2 = tcl(p2, n[3]),
  n3_p3 = tcl(p3, n[3])
)

plt <- list()
titulo <- c(
  rep("População", times = 3),
  paste("n = ", rep(n, each = 3), sep = '')
)

for (i in 1:length(YM)){
  eixoX <- bquote(bar(Y))
  if (i <=3){
    eixoX <- "Y"
  }

  plt[[i]] <- ggplot(data = YM[[i]]) +
    geom_histogram(aes(x = Ym), 
                   fill = 'dodgerblue4', 
                   color = 'black', bins = 20) +
    ggtitle(titulo[i]) +
    ylab('') + 
    xlab(eixoX) +
    theme_classic() +
    theme(axis.text = element_blank(),
          axis.ticks =element_blank(),
          plot.title = element_text(hjust = 0.5, size = 20),
    )
}

gridExtra::grid.arrange(
  plt[[1]], plt[[4]], plt[[7]], plt[[10]],
  plt[[2]], plt[[5]], plt[[8]], plt[[11]],
  plt[[3]], plt[[6]], plt[[9]], plt[[12]],
  nrow = 3, ncol = 4)
