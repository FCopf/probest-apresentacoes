library(tydiverse)

xlim = c(-10,20)
mu1 = -3; sd1 = 3
mu2 = -0; sd2 = 1
mu3 = 10; sd3 = 4
mu4 = -5; sd4 = 2
df_norm <- data.frame(X = seq(xlim[1], xlim[2], length = 100)) %>% 
  mutate(dx1 = dnorm(X, mu1, sd1),
         dx2 = dnorm(X, mu2, sd2),
         dx3 = dnorm(X, mu3, sd3),
         dx4 = dnorm(X, mu4, sd4))

ggplot(df_norm, aes(x = X)) +
  stat_function(fun = dnorm, 
                args = list(mean = mu1, sd = sd1),
                color = 'red', size = 2, alpha = 0.5) +
  stat_function(fun = dnorm, 
                args = list(mean = mu2, sd = sd2),
                color = 'blue', size = 2, alpha = 0.5) +
  stat_function(fun = dnorm, 
                args = list(mean = mu3, sd = sd3),
                color = 'green', size = 2, alpha = 0.5) +
  stat_function(fun = dnorm, 
                args = list(mean = mu4, sd = sd4),
                color = 'black', size = 2, alpha = 0.5) +
  annotate(geom ='text', x = mu1-1,5, 
           y = dnorm(mu1, mu1, sd1) * 0.5, 
           label = bquote(mu == .(mu1) ~ ';' ~ sigma == .(sd1)), 
           color = 'red', size = 5) +
  annotate(geom ='text', x = mu2, 
           y = dnorm(mu2, mu2, sd2) * 1.05, 
           label = bquote(mu == .(mu2) ~ ';' ~ sigma == .(sd2)), 
           color = 'blue', size = 5) +
  annotate(geom ='text', x = mu3, 
           y = dnorm(mu3, mu3, sd3) * 0.5, 
           label = bquote(mu == .(mu3) ~ ';' ~ sigma == .(sd3)), 
           color = 'green', size = 5) +
  annotate(geom ='text', x = mu4, 
           y = dnorm(mu4, mu4, sd4) * 1.1, 
           label = bquote(mu == .(mu4) ~ ';' ~ sigma == .(sd4)), 
           color = 'black', size = 5) +
  scale_x_continuous(breaks = seq(xlim[1], xlim[2], by = 2)) +
  coord_cartesian(xlim = xlim, ylim = c(0, 0.5)) +
  labs(y = 'f(x)', x = 'x') +
  theme_classic(base_size = 15)


