library(tidyverse)
mmq <- function(
  frac_pt_size = 1.5,
  frac_ytext_Eq = 0.99,
  frac_xtext_Eq = 0.75,
  frac_ytext_SQres = 0.91,
  frac_xtext_SQres = 0.75,
  frac_ytext_SQy = 0.83,
  frac_xtext_SQy = 0.75,
  frac_ytext_SQx = 0.75,
  frac_xtext_SQx = 0.75,
  
  pt_size = 5,
  line_size = 1,
  seg_line_size = 1,
  axis_size = 8,
  text_size = 7,
  seg_size = 2,
  text_eq_size = 8,
  pt_selected = 1,
  b0 = 800,
  b1 = -4,
  dp = 10,
  n = 15,
  x = sort(runif(n, min = 0, max = 20)),
  alpha_nonselected = 0.5,
  reg_line_coef = c(780, -1.3),
  sem = 5,
  
  try_best_reg = FALSE,
  
  show_pt = TRUE,
  show_pt_selected = FALSE,
  
  show_reg_line = FALSE,
  show_ymean_line = FALSE,
  show_xmean_line = FALSE,
  
  show_seg_res = FALSE,
  show_seg_dy = FALSE,
  show_seg_dx = FALSE,
  
  show_text_res = FALSE,
  show_text_dy = FALSE,
  show_text_dx = FALSE,
  
  show_text_Eq = FALSE,
  show_text_SQres = FALSE,
  show_text_SQy = FALSE,
  show_text_SQx = FALSE
  
){
  set.seed(sem)
  #_________________________
  # Generate sampled points
  
  df <- data.frame(x, y = rnorm(n, b0 + b1 * x, dp))
  
  if (try_best_reg){
    reg_line_coef = coef(lm(y ~ x, data = df))
  }
  
  df <- df %>% 
    mutate(yhat = reg_line_coef[1] + reg_line_coef[2] * x,
           yres = y - (reg_line_coef[1] + reg_line_coef[2] * x),
           dy = y - mean(y),
           dx = x - mean(x))
  
  
  #_________________________
  # Change alpha color of selected point
  alpha_point <- rep(1, n)
  if(show_pt_selected){
    alpha_point[-pt_selected] <- alpha_nonselected
  }
  
  #_________________________
  # Expression for reg_line
  if (reg_line_coef[2] >= 0) {
    eq_expr <- bquote(y[i] == .(round(reg_line_coef[1],0)) + .(round(reg_line_coef[2],1)) * x[i])
  } else {
    eq_expr <- bquote(y[i] == .(round(reg_line_coef[1],0)) ~ .(round(reg_line_coef[2],1)) * x[i])
  }
  
  #_________________________
  # Calculates SQres from reg_line
  SQres <- sum(df$yres^2)
  
  #_________________________
  # Calculates SQy from y mean
  SQy <- sum(df$dy^2)
  
  #_________________________
  # Calculates SQx from x mean
  SQx <- sum(df$dx^2)
  
  #_________________________
  # Plots
  #_________________________
  # Plot sampled points
  plt <- ggplot(df, aes(y = y, x = x)) +
    labs(y = "Y", x = "X") + 
    ylim(c(min(df$y-10), max(df$y+10))) +
    xlim(range(df$x))
  
  plt_sel <- plt_pt <- plt_reg_line <- plt_ymean_line <- plt_xmean_line <- 
    seg_res <- seg_dy <- seg_dx <-
    plt_ymean_text <- plt_xmean_text <-
    text_res <- text_dy <- text_dx <-
    text_Eq <- text_SQres <- text_SQy <- text_SQx <- NULL
  
  #_________________________
  # Plot points
  if(show_pt){
    plt_pt <- geom_point(size = pt_size, alpha = alpha_point)
  }
  
  #_________________________
  # Mark Selected point
  if(show_pt_selected){
    plt_sel <- annotate(geom = "point", x = df$x[pt_selected],
                        y = df$y[pt_selected], color = 'blue', size = frac_pt_size * pt_size, 
                        shape = 1)
  }
  
  #_________________________
  # Regression line tested
  if(show_reg_line){
    plt_reg_line <- geom_abline(intercept = reg_line_coef[1], slope = reg_line_coef[2], 
                                color = 'red', size = line_size)
  }
  
  #_________________________
  # Y mean line
  if(show_ymean_line){
    plt_ymean_line <- geom_hline(yintercept = mean(df$y), color = '#23395b', size = line_size)
    plt_ymean_text <- annotate('text', x = min(df$x), y = mean(df$y), vjust = -0.5, 
               label = expression(bar(Y)), size = text_size)
  }
  
  #_________________________
  # X mean line
  if(show_xmean_line){
    plt_xmean_line <- geom_vline(xintercept = mean(df$x), color = '#23395b', size = line_size)
    plt_xmean_text <- annotate('text', x = mean(df$x), y = max(df$y), hjust = -0.5, 
                               label = expression(bar(X)), size = text_size)
  }
  
  #_________________________
  # Residual line
  if(show_seg_res){
    seg_res <- annotate(geom = "segment", x = df$x[pt_selected], xend = df$x[pt_selected],
                        y = df$y[pt_selected], yend = df$yhat[pt_selected],
                        linetype = "dotted", size = seg_line_size, color = 'red')
  }
  
  #_________________________
  # dy line
  if(show_seg_dy){
    seg_dy <- annotate(geom = "segment", x = df$x[pt_selected], y = df$y[pt_selected], 
                       xend = df$x[pt_selected], yend = mean(df$y),
                        linetype = "dotted", size = seg_line_size, color = 'black')
  }
  
  #_________________________
  # dx line
  if(show_seg_dx){
    seg_dx <- annotate(geom = "segment", x = df$x[pt_selected], y = df$y[pt_selected], 
                       xend = mean(df$x), yend = df$y[pt_selected],
                       linetype = "dotted", size = seg_line_size, color = 'black')
  }
  
  #_________________________
  # text residual
  if(show_text_res){
    text_res <- annotate(geom = 'text', x = df$x[pt_selected],
                         y = df$yhat[pt_selected] + df$yres[pt_selected]/2, 
                         label = expression(epsilon[i] == y[i] - hat(y)[i]), 
                         hjust = ifelse(df$x[pt_selected] > mean(df$x), yes = 1.1, no = -0.1),
                         size = text_size, color = 'red')
  }
  
  #_________________________
  # text dy
  if(show_text_dy){
    text_dy <- annotate(geom = 'text', x = df$x[pt_selected],
                         y = mean(df$y) + df$dy[pt_selected]/2, 
                         label = expression(y[i] - bar(y)[i]), 
                         hjust = ifelse(df$x[pt_selected] > mean(df$x), yes = 1.1, no = -0.1),
                         size = text_size, color = 'red')
  }
  
  #_________________________
  # text dy
  if(show_text_dx){
    text_dx <- annotate(geom = 'text', x = mean(df$x) + df$dx[pt_selected]/2,
                         y = df$y[pt_selected], 
                         label = expression(x[i] - bar(x)[i]), 
                         vjust = ifelse(df$y[pt_selected] > mean(df$y), yes = -0.2, no = 1.3),
                         size = text_size, color = 'red')
  }
  
  #_________________________
  # text Regression equation
  if(show_text_Eq){
    text_Eq <- annotate(geom = 'text', x = min(df$x) + diff(range(df$x)) * frac_xtext_Eq,
                        y = min(df$y) + diff(range(df$y))* frac_ytext_Eq, 
                        label = eq_expr, 
                        hjust = 0,
                        size = text_eq_size, color = 'black')
  }
  #_________________________
  # text SQres
  if(show_text_SQres){
    text_SQres <- annotate(geom = 'text', x = min(df$x) + diff(range(df$x)) * frac_xtext_SQres,
                        y = min(df$y) + diff(range(df$y)) * frac_ytext_SQres, 
                        label = bquote(SQ[res] == .(round(SQres,2))), 
                        hjust = 0,
                        size = text_eq_size, color = 'black')
  }
  #_________________________
  # text SQy
  if(show_text_SQy){
    text_SQy <- annotate(geom = 'text', x = min(df$x) + diff(range(df$x)) * frac_xtext_SQy,
                           y = min(df$y) + diff(range(df$y)) * frac_ytext_SQy, 
                           label = bquote(SQ[y] == .(round(SQy,2))), 
                           hjust = 0,
                           size = text_eq_size, color = 'black')
  }
  #_________________________
  # text SQx
  if(show_text_SQx){
    text_SQx <- annotate(geom = 'text', x = min(df$x) + diff(range(df$x)) * frac_xtext_SQx,
                         y = min(df$y) + diff(range(df$y)) * frac_ytext_SQx, 
                         label = bquote(SQ[x] == .(round(SQx,2))), 
                         hjust = 0,
                         size = text_eq_size, color = 'black')
  }

  #_________________________
  # Return plot
  plt + plt_pt + plt_sel + plt_reg_line + plt_ymean_line + plt_xmean_line + 
    seg_res + seg_dy + seg_dx +
    plt_ymean_text + plt_xmean_text +
    text_res + text_dy + text_dx + 
    text_Eq + text_SQres + text_SQy + text_SQx + 
    theme_classic()  

}

  

